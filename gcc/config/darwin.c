/* Functions for generic Darwin as target machine for GNU C compiler.
   Copyright (C) 1989, 1990, 1991, 1992, 1993, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "tree.h"
#include "expr.h"
#include "reload.h"
#include "function.h"
#include "ggc.h"
#include "langhooks.h"
#include "tm_p.h"
#include "errors.h"

static int machopic_data_defined_p (const char *);
static void update_non_lazy_ptrs (const char *);
static void update_stubs (const char *);
static const char *machopic_non_lazy_ptr_name (const char*);

int
name_needs_quotes (const char *name)
{
  int c;
  while ((c = *name++) != '\0')
    if (! ISIDNUM (c) && c != '.' && c != '$')
      return 1;
  return 0;
}

/*
 * flag_pic = 1 ... generate only indirections
 * flag_pic = 2 ... generate indirections and pure code
 */

/* This module assumes that (const (symbol_ref "foo")) is a legal pic
   reference, which will not be changed.  */

static GTY(()) tree machopic_defined_list;

enum machopic_addr_class
machopic_classify_ident (tree ident)
{
  const char *name = IDENTIFIER_POINTER (ident);
  int lprefix = (((name[0] == '*' || name[0] == '&')
		  && (name[1] == 'L' || (name[1] == '"' && name[2] == 'L')))
		 || (   name[0] == '_'
		     && name[1] == 'O'
		     && name[2] == 'B'
		     && name[3] == 'J'
		     && name[4] == 'C'
		     && name[5] == '_'));
  tree temp;

  /* The PIC base symbol is always defined. */
  if (! strcmp (name, "<pic base>"))
    return MACHOPIC_DEFINED_DATA;

  if (name[0] != '!')
    {
      /* Here if no special encoding to be found.  */
      if (lprefix)
	{
	  const char *name = IDENTIFIER_POINTER (ident);
	  int len = strlen (name);

	  if ((len > 5 && !strcmp (name + len - 5, "$stub"))
	      || (len > 6 && !strcmp (name + len - 6, "$stub\"")))
	    return MACHOPIC_DEFINED_FUNCTION;
	  return MACHOPIC_DEFINED_DATA;
	}

      for (temp = machopic_defined_list;
	   temp != NULL_TREE;
	   temp = TREE_CHAIN (temp))
	{
	  if (ident == TREE_VALUE (temp))
	    return MACHOPIC_DEFINED_DATA;
	}

      if (TREE_ASM_WRITTEN (ident))
	return MACHOPIC_DEFINED_DATA;

      return MACHOPIC_UNDEFINED;
    }

  else if (name[1] == 'D')
    return MACHOPIC_DEFINED_DATA;

  else if (name[1] == 'T')
    return MACHOPIC_DEFINED_FUNCTION;

  /* It is possible that someone is holding a "stale" name, which has
     since been defined.  See if there is a "defined" name (i.e,
     different from NAME only in having a '!D_' or a '!T_' instead of
     a '!d_' or '!t_' prefix) in the identifier hash tables.  If so, say
     that this identifier is defined.  */
  else if (name[1] == 'd' || name[1] == 't')
    {
      char *new_name;
      new_name = (char *)alloca (strlen (name) + 1);
      strcpy (new_name, name);
      new_name[1] = (name[1] == 'd') ? 'D' : 'T';
      if (maybe_get_identifier (new_name) != NULL)
	return  (name[1] == 'd') ? MACHOPIC_DEFINED_DATA
				 : MACHOPIC_DEFINED_FUNCTION;
    }

  for (temp = machopic_defined_list; temp != NULL_TREE; temp = TREE_CHAIN (temp))
    {
      if (ident == TREE_VALUE (temp))
	{
	  if (name[1] == 'T')
	    return MACHOPIC_DEFINED_FUNCTION;
	  else
	    return MACHOPIC_DEFINED_DATA;
	}
    }

  if (name[1] == 't' || name[1] == 'T')
    {
      if (lprefix)
	return MACHOPIC_DEFINED_FUNCTION;
      else
	return MACHOPIC_UNDEFINED_FUNCTION;
    }
  else
    {
      if (lprefix)
	return MACHOPIC_DEFINED_DATA;
      else
	return MACHOPIC_UNDEFINED_DATA;
    }
}


enum machopic_addr_class
machopic_classify_name (const char *name)
{
  return machopic_classify_ident (get_identifier (name));
}

int
machopic_ident_defined_p (tree ident)
{
  switch (machopic_classify_ident (ident))
    {
    case MACHOPIC_UNDEFINED:
    case MACHOPIC_UNDEFINED_DATA:
    case MACHOPIC_UNDEFINED_FUNCTION:
      return 0;
    default:
      return 1;
    }
}

static int
machopic_data_defined_p (const char *name)
{
  switch (machopic_classify_ident (get_identifier (name)))
    {
    case MACHOPIC_DEFINED_DATA:
      return 1;
    default:
      return 0;
    }
}

int
machopic_name_defined_p (const char *name)
{
  return machopic_ident_defined_p (get_identifier (name));
}

void
machopic_define_ident (tree ident)
{
  if (!machopic_ident_defined_p (ident))
    machopic_defined_list =
      tree_cons (NULL_TREE, ident, machopic_defined_list);
}

void
machopic_define_name (const char *name)
{
  machopic_define_ident (get_identifier (name));
}

static GTY(()) char * function_base;

const char *
machopic_function_base_name (void)
{
  const char *current_name;
  /* if dynamic-no-pic is on, we should not get here */
  if (MACHO_DYNAMIC_NO_PIC_P)
    abort ();
  current_name =
    IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl));

  if (function_base == NULL)
    function_base =
      (char *) ggc_alloc_string ("<pic base>", sizeof ("<pic base>"));

  current_function_uses_pic_offset_table = 1;

  return function_base;
}

static GTY(()) const char * function_base_func_name;
static GTY(()) int current_pic_label_num;

void
machopic_output_function_base_name (FILE *file)
{
  const char *current_name;

  /* If dynamic-no-pic is on, we should not get here. */
  if (MACHO_DYNAMIC_NO_PIC_P)
    abort ();
  current_name =
    IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl));
  if (function_base_func_name != current_name)
    {
      ++current_pic_label_num;
      function_base_func_name = current_name;
    }
  fprintf (file, "\"L%011d$pb\"", current_pic_label_num);
}

static GTY(()) tree machopic_non_lazy_pointers;

/* Return a non-lazy pointer name corresponding to the given name,
   either by finding it in our list of pointer names, or by generating
   a new one.  */

static const char *
machopic_non_lazy_ptr_name (const char *name)
{
  const char *temp_name;
  tree temp, ident = get_identifier (name);

  for (temp = machopic_non_lazy_pointers;
       temp != NULL_TREE;
       temp = TREE_CHAIN (temp))
    {
      if (ident == TREE_VALUE (temp))
	return IDENTIFIER_POINTER (TREE_PURPOSE (temp));
    }

  name = darwin_strip_name_encoding (name);

  /* Try again, but comparing names this time.  */
  for (temp = machopic_non_lazy_pointers;
       temp != NULL_TREE;
       temp = TREE_CHAIN (temp))
    {
      if (TREE_VALUE (temp))
	{
	  temp_name = IDENTIFIER_POINTER (TREE_VALUE (temp));
	  temp_name = darwin_strip_name_encoding (temp_name);
	  if (strcmp (name, temp_name) == 0)
	    return IDENTIFIER_POINTER (TREE_PURPOSE (temp));
	}
    }

  {
    char *buffer;
    int namelen = strlen (name);
    int bufferlen = 0;
    tree ptr_name;

    buffer = alloca (namelen + strlen("$non_lazy_ptr") + 5);

    strcpy (buffer, "&L");
    bufferlen = 2;
    if (name[0] == '*')
      {
        memcpy (buffer + bufferlen, name+1, namelen-1+1);
        bufferlen += namelen-1;
      }
    else
      {
	buffer[bufferlen] = '_';
	memcpy (buffer + bufferlen +1, name, namelen+1);
        bufferlen += namelen +1;
      }

    memcpy (buffer + bufferlen, "$non_lazy_ptr", strlen("$non_lazy_ptr")+1);
    bufferlen += strlen("$non_lazy_ptr");
    ptr_name = get_identifier (buffer);

    machopic_non_lazy_pointers
      = tree_cons (ptr_name, ident, machopic_non_lazy_pointers);

    TREE_USED (machopic_non_lazy_pointers) = 0;

    return IDENTIFIER_POINTER (ptr_name);
  }
}

static GTY(()) tree machopic_stubs;

/* Return the name of the stub corresponding to the given name,
   generating a new stub name if necessary.  */

const char *
machopic_stub_name (const char *name)
{
  tree temp, ident = get_identifier (name);
  const char *tname;

  for (temp = machopic_stubs;
       temp != NULL_TREE;
       temp = TREE_CHAIN (temp))
    {
      if (ident == TREE_VALUE (temp))
	return IDENTIFIER_POINTER (TREE_PURPOSE (temp));
      tname = IDENTIFIER_POINTER (TREE_VALUE (temp));
      if (strcmp (name, tname) == 0)
	return IDENTIFIER_POINTER (TREE_PURPOSE (temp));
      /* A library call name might not be section-encoded yet, so try
	 it against a stripped name.  */
      if (name[0] != '!'
	  && tname[0] == '!'
	  && strcmp (name, tname + 4) == 0)
	return IDENTIFIER_POINTER (TREE_PURPOSE (temp));
    }

  name = darwin_strip_name_encoding (name);

  {
    char *buffer;
    int bufferlen = 0;
    int namelen = strlen (name);
    tree ptr_name;
    int needs_quotes = name_needs_quotes (name);

    buffer = alloca (namelen + 20);

    if (needs_quotes)
      {
        strcpy (buffer, "&\"L");
        bufferlen = strlen("&\"L");
      }
    else
      {
        strcpy (buffer, "&L");
        bufferlen = strlen("&L");
      }
    
    if (name[0] == '*')
      {
	memcpy (buffer + bufferlen, name+1, namelen - 1 +1);
        bufferlen += namelen - 1;
      }
    else
      {
	buffer[bufferlen] = '_';
	memcpy (buffer + bufferlen +1, name, namelen+1);
        bufferlen += namelen +1;
      }

    if (needs_quotes)
      {
        memcpy (buffer + bufferlen, "$stub\"", strlen("$stub\"")+1);
        bufferlen += strlen("$stub\"");
      }
    else
      {
        memcpy (buffer + bufferlen, "$stub", strlen("$stub")+1);
        bufferlen += strlen("$stub");
      }
    ptr_name = get_identifier (buffer);

    machopic_stubs = tree_cons (ptr_name, ident, machopic_stubs);
    TREE_USED (machopic_stubs) = 0;

    return IDENTIFIER_POINTER (ptr_name);
  }
}

void
machopic_validate_stub_or_non_lazy_ptr (const char *name, int validate_stub)
{
  const char *real_name;
  tree temp, ident = get_identifier (name), id2;

    for (temp = (validate_stub ? machopic_stubs : machopic_non_lazy_pointers);
         temp != NULL_TREE;
         temp = TREE_CHAIN (temp))
      if (ident == TREE_PURPOSE (temp))
	{
	  /* Mark both the stub or non-lazy pointer as well as the
	     original symbol as being referenced.  */
          TREE_USED (temp) = 1;
	  if (TREE_CODE (TREE_VALUE (temp)) == IDENTIFIER_NODE)
	    mark_referenced (TREE_VALUE (temp));
	  real_name = IDENTIFIER_POINTER (TREE_VALUE (temp));
	  real_name = darwin_strip_name_encoding (real_name);
	  id2 = maybe_get_identifier (real_name);
	  if (id2)
	    mark_referenced (id2);
	}
}

/* Transform ORIG, which may be any data source, to the corresponding
   source using indirections.  */

rtx
machopic_indirect_data_reference (rtx orig, rtx reg)
{
  rtx ptr_ref = orig;

  if (! MACHOPIC_INDIRECT)
    return orig;

  if (GET_CODE (orig) == SYMBOL_REF)
    {
      const char *name = XSTR (orig, 0);
      int defined = machopic_data_defined_p (name);

      if (defined && MACHO_DYNAMIC_NO_PIC_P)
	{
#if defined (TARGET_TOC)
           emit_insn (gen_macho_high (reg, orig));
           emit_insn (gen_macho_low (reg, reg, orig));
#else
	   /* some other cpu -- writeme!  */
	   abort ();
#endif
	   return reg;
	}
      else if (defined)
	{
#if defined (TARGET_TOC) || defined (HAVE_lo_sum)
	  rtx pic_base = gen_rtx_SYMBOL_REF (Pmode,
					     machopic_function_base_name ());
	  rtx offset = gen_rtx_CONST (Pmode,
				      gen_rtx_MINUS (Pmode, orig, pic_base));
#endif

#if defined (TARGET_TOC) /* i.e., PowerPC */
	  rtx hi_sum_reg = (no_new_pseudos ? reg : gen_reg_rtx (Pmode));

	  if (reg == NULL)
	    abort ();

	  emit_insn (gen_rtx_SET (Pmode, hi_sum_reg,
			      gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
				       gen_rtx_HIGH (Pmode, offset))));
	  emit_insn (gen_rtx_SET (Pmode, reg,
				  gen_rtx_LO_SUM (Pmode, hi_sum_reg, offset)));

	  orig = reg;
#else
#if defined (HAVE_lo_sum)
	  if (reg == 0) abort ();

	  emit_insn (gen_rtx_SET (VOIDmode, reg,
				  gen_rtx_HIGH (Pmode, offset)));
	  emit_insn (gen_rtx_SET (VOIDmode, reg,
				  gen_rtx_LO_SUM (Pmode, reg, offset)));
	  emit_insn (gen_rtx_USE (VOIDmode, pic_offset_table_rtx));

	  orig = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, reg);
#endif
#endif
	  return orig;
	}

      ptr_ref = gen_rtx_SYMBOL_REF (Pmode,
				    machopic_non_lazy_ptr_name (name));

      ptr_ref = gen_rtx_MEM (Pmode, ptr_ref);
      RTX_UNCHANGING_P (ptr_ref) = 1;

      return ptr_ref;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, result;

      /* legitimize both operands of the PLUS */
      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base = machopic_indirect_data_reference (XEXP (XEXP (orig, 0), 0),
						   reg);
	  orig = machopic_indirect_data_reference (XEXP (XEXP (orig, 0), 1),
						   (base == reg ? 0 : reg));
	}
      else
	return orig;

      if (MACHOPIC_PURE && GET_CODE (orig) == CONST_INT)
	result = plus_constant (base, INTVAL (orig));
      else
	result = gen_rtx_PLUS (Pmode, base, orig);

      if (MACHOPIC_JUST_INDIRECT && GET_CODE (base) == MEM)
	{
	  if (reg)
	    {
	      emit_move_insn (reg, result);
	      result = reg;
	    }
	  else
	    {
	      result = force_reg (GET_MODE (result), result);
	    }
	}

      return result;

    }
  else if (GET_CODE (orig) == MEM)
    XEXP (ptr_ref, 0) = machopic_indirect_data_reference (XEXP (orig, 0), reg);
  /* When the target is i386, this code prevents crashes due to the
     compiler's ignorance on how to move the PIC base register to
     other registers.  (The reload phase sometimes introduces such
     insns.)  */
  else if (GET_CODE (orig) == PLUS
	   && GET_CODE (XEXP (orig, 0)) == REG
	   && REGNO (XEXP (orig, 0)) == PIC_OFFSET_TABLE_REGNUM
#ifdef I386
	   /* Prevent the same register from being erroneously used
	      as both the base and index registers.  */
	   && GET_CODE (XEXP (orig, 1)) == CONST
#endif
	   && reg)
    {
      emit_move_insn (reg, XEXP (orig, 0));
      XEXP (ptr_ref, 0) = reg;
    }
  return ptr_ref;
}

/* Transform TARGET (a MEM), which is a function call target, to the
   corresponding symbol_stub if necessary.  Return a new MEM.  */

rtx
machopic_indirect_call_target (rtx target)
{
  if (GET_CODE (target) != MEM)
    return target;

  if (MACHOPIC_INDIRECT && GET_CODE (XEXP (target, 0)) == SYMBOL_REF)
    {
      enum machine_mode mode = GET_MODE (XEXP (target, 0));
      const char *name = XSTR (XEXP (target, 0), 0);

      /* If the name is already defined, we need do nothing.  */
      if (name[0] == '!' && name[1] == 'T')
	return target;

      if (!machopic_name_defined_p (name))
	{
	  const char *stub_name = machopic_stub_name (name);

	  XEXP (target, 0) = gen_rtx_SYMBOL_REF (mode, stub_name);
	  RTX_UNCHANGING_P (target) = 1;
	}
    }

  return target;
}

rtx
machopic_legitimize_pic_address (rtx orig, enum machine_mode mode, rtx reg)
{
  rtx pic_ref = orig;

  if (! MACHOPIC_INDIRECT)
    return orig;

  /* First handle a simple SYMBOL_REF or LABEL_REF */
  if (GET_CODE (orig) == LABEL_REF
      || (GET_CODE (orig) == SYMBOL_REF
	  ))
    {
      /* addr(foo) = &func+(foo-func) */
      rtx pic_base;

      orig = machopic_indirect_data_reference (orig, reg);

      if (GET_CODE (orig) == PLUS
	  && GET_CODE (XEXP (orig, 0)) == REG)
	{
	  if (reg == 0)
	    return force_reg (mode, orig);

	  emit_move_insn (reg, orig);
	  return reg;
	}

      /* if dynamic-no-pic then use 0 as the pic base  */
      if (MACHO_DYNAMIC_NO_PIC_P)
	pic_base = CONST0_RTX (Pmode);
      else
      pic_base = gen_rtx_SYMBOL_REF (Pmode, machopic_function_base_name ());

      if (GET_CODE (orig) == MEM)
	{
	  if (reg == 0)
	    {
	      if (reload_in_progress)
		abort ();
	      else
		reg = gen_reg_rtx (Pmode);
	    }

#ifdef HAVE_lo_sum
	  if (MACHO_DYNAMIC_NO_PIC_P
	      && (GET_CODE (XEXP (orig, 0)) == SYMBOL_REF
		  || GET_CODE (XEXP (orig, 0)) == LABEL_REF))
	    {
#if defined (TARGET_TOC)	/* ppc  */
	      rtx temp_reg = (no_new_pseudos) ? reg : gen_reg_rtx (Pmode);
	      rtx asym = XEXP (orig, 0);
	      rtx mem;

	      emit_insn (gen_macho_high (temp_reg, asym));
	      mem = gen_rtx_MEM (GET_MODE (orig),
				 gen_rtx_LO_SUM (Pmode, temp_reg, asym));
	      RTX_UNCHANGING_P (mem) = 1;
	      emit_insn (gen_rtx_SET (VOIDmode, reg, mem));
#else
	      /* Some other CPU -- WriteMe! but right now there are no other platform that can use dynamic-no-pic  */
	      abort ();
#endif
	      pic_ref = reg;
	    }
	  else
	  if (GET_CODE (XEXP (orig, 0)) == SYMBOL_REF
	      || GET_CODE (XEXP (orig, 0)) == LABEL_REF)
	    {
	      rtx offset = gen_rtx_CONST (Pmode,
					  gen_rtx_MINUS (Pmode,
							 XEXP (orig, 0),
							 pic_base));
#if defined (TARGET_TOC) /* i.e., PowerPC */
	      /* Generating a new reg may expose opportunities for
		 common subexpression elimination.  */
              rtx hi_sum_reg = no_new_pseudos ? reg : gen_reg_rtx (SImode);
	      rtx mem;
	      rtx insn;
	      rtx sum;
	      
	      sum = gen_rtx_HIGH (Pmode, offset);
	      if (! MACHO_DYNAMIC_NO_PIC_P)
		sum = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, sum);

	      emit_insn (gen_rtx_SET (Pmode, hi_sum_reg, sum));

	      mem = gen_rtx_MEM (GET_MODE (orig),
				 gen_rtx_LO_SUM (Pmode, 
						 hi_sum_reg, offset));
	      RTX_UNCHANGING_P (mem) = 1;
	      insn = emit_insn (gen_rtx_SET (VOIDmode, reg, mem));
	      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL, pic_ref, 
						    REG_NOTES (insn));

	      pic_ref = reg;
#else
	      emit_insn (gen_rtx_USE (VOIDmode,
				      gen_rtx_REG (Pmode, 
						   PIC_OFFSET_TABLE_REGNUM)));

	      emit_insn (gen_rtx_SET (VOIDmode, reg,
				      gen_rtx_HIGH (Pmode,
						    gen_rtx_CONST (Pmode, 
								   offset))));
	      emit_insn (gen_rtx_SET (VOIDmode, reg,
				  gen_rtx_LO_SUM (Pmode, reg,
					   gen_rtx_CONST (Pmode, offset))));
	      pic_ref = gen_rtx_PLUS (Pmode,
				      pic_offset_table_rtx, reg);
#endif
	    }
	  else
#endif  /* HAVE_lo_sum */
	    {
	      rtx pic = pic_offset_table_rtx;
	      if (GET_CODE (pic) != REG)
		{
		  emit_move_insn (reg, pic);
		  pic = reg;
		}
#if 0
	      emit_insn (gen_rtx_USE (VOIDmode,
				      gen_rtx_REG (Pmode, 
						   PIC_OFFSET_TABLE_REGNUM)));
#endif

	      pic_ref = gen_rtx_PLUS (Pmode,
				      pic,
				      gen_rtx_CONST (Pmode,
					  gen_rtx_MINUS (Pmode,
							 XEXP (orig, 0),
							 pic_base)));
	    }

#if !defined (TARGET_TOC)
	  emit_move_insn (reg, pic_ref);
	  pic_ref = gen_rtx_MEM (GET_MODE (orig), reg);
#endif
	  RTX_UNCHANGING_P (pic_ref) = 1;
	}
      else
	{

#ifdef HAVE_lo_sum
	  if (GET_CODE (orig) == SYMBOL_REF
	      || GET_CODE (orig) == LABEL_REF)
	    {
	      rtx offset = gen_rtx_CONST (Pmode,
					  gen_rtx_MINUS (Pmode, 
							 orig, pic_base));
#if defined (TARGET_TOC) /* i.e., PowerPC */
              rtx hi_sum_reg;

	      if (reg == 0)
		{
		  if (reload_in_progress)
		    abort ();
		  else
		    reg = gen_reg_rtx (SImode);
		}

	      hi_sum_reg = reg;

	      emit_insn (gen_rtx_SET (Pmode, hi_sum_reg,
				      (MACHO_DYNAMIC_NO_PIC_P)
				      ? gen_rtx_HIGH (Pmode, offset)
				      : gen_rtx_PLUS (Pmode,
						      pic_offset_table_rtx,
						      gen_rtx_HIGH (Pmode, 
								    offset))));
	      emit_insn (gen_rtx_SET (VOIDmode, reg,
				      gen_rtx_LO_SUM (Pmode,
						      hi_sum_reg, offset)));
	      pic_ref = reg;
	      RTX_UNCHANGING_P (pic_ref) = 1;
#else
	      emit_insn (gen_rtx_SET (VOIDmode, reg,
				      gen_rtx_HIGH (Pmode, offset)));
	      emit_insn (gen_rtx_SET (VOIDmode, reg,
				      gen_rtx_LO_SUM (Pmode, reg, offset)));
	      pic_ref = gen_rtx_PLUS (Pmode,
				      pic_offset_table_rtx, reg);
	      RTX_UNCHANGING_P (pic_ref) = 1;
#endif
	    }
	  else
#endif  /*  HAVE_lo_sum  */
	    {
	      if (GET_CODE (orig) == REG)
		{
		  return orig;
		}
	      else
		{
		  rtx pic = pic_offset_table_rtx;
		  if (GET_CODE (pic) != REG)
		    {
		      emit_move_insn (reg, pic);
		      pic = reg;
		    }
#if 0
		  emit_insn (gen_rtx_USE (VOIDmode,
					  pic_offset_table_rtx));
#endif
		  pic_ref = gen_rtx_PLUS (Pmode,
					  pic,
					  gen_rtx_CONST (Pmode,
					      gen_rtx_MINUS (Pmode,
							     orig, pic_base)));
		}
	    }
	}

      if (GET_CODE (pic_ref) != REG)
        {
          if (reg != 0)
            {
              emit_move_insn (reg, pic_ref);
              return reg;
            }
          else
            {
              return force_reg (mode, pic_ref);
            }
        }
      else
        {
          return pic_ref;
        }
    }

  else if (GET_CODE (orig) == SYMBOL_REF)
    return orig;

  else if (GET_CODE (orig) == PLUS
	   && (GET_CODE (XEXP (orig, 0)) == MEM
	       || GET_CODE (XEXP (orig, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (orig, 0)) == LABEL_REF)
	   && XEXP (orig, 0) != pic_offset_table_rtx
	   && GET_CODE (XEXP (orig, 1)) != REG)

    {
      rtx base;
      int is_complex = (GET_CODE (XEXP (orig, 0)) == MEM);

      base = machopic_legitimize_pic_address (XEXP (orig, 0), Pmode, reg);
      orig = machopic_legitimize_pic_address (XEXP (orig, 1),
					      Pmode, (base == reg ? 0 : reg));
      if (GET_CODE (orig) == CONST_INT)
	{
	  pic_ref = plus_constant (base, INTVAL (orig));
	  is_complex = 1;
	}
      else
	pic_ref = gen_rtx_PLUS (Pmode, base, orig);

      if (RTX_UNCHANGING_P (base) && RTX_UNCHANGING_P (orig))
	RTX_UNCHANGING_P (pic_ref) = 1;

      if (reg && is_complex)
	{
	  emit_move_insn (reg, pic_ref);
	  pic_ref = reg;
	}
      /* Likewise, should we set special REG_NOTEs here?  */
    }

  else if (GET_CODE (orig) == CONST)
    {
      return machopic_legitimize_pic_address (XEXP (orig, 0), Pmode, reg);
    }

  else if (GET_CODE (orig) == MEM
	   && GET_CODE (XEXP (orig, 0)) == SYMBOL_REF)
    {
      rtx addr = machopic_legitimize_pic_address (XEXP (orig, 0), Pmode, reg);

      addr = gen_rtx_MEM (GET_MODE (orig), addr);
      RTX_UNCHANGING_P (addr) = RTX_UNCHANGING_P (orig);
      emit_move_insn (reg, addr);
      pic_ref = reg;
    }

  return pic_ref;
}


void
machopic_finish (FILE *asm_out_file)
{
  tree temp;

  for (temp = machopic_stubs;
       temp != NULL_TREE;
       temp = TREE_CHAIN (temp))
    {
      const char *sym_name = IDENTIFIER_POINTER (TREE_VALUE (temp));
      const char *stub_name = IDENTIFIER_POINTER (TREE_PURPOSE (temp));
      char *sym;
      char *stub;

      if (! TREE_USED (temp))
	continue;

      sym_name = darwin_strip_name_encoding (sym_name);

      sym = alloca (strlen (sym_name) + 2);
      if (sym_name[0] == '*' || sym_name[0] == '&')
	strcpy (sym, sym_name + 1);
      else if (sym_name[0] == '-' || sym_name[0] == '+')
	strcpy (sym, sym_name);
      else
	sym[0] = '_', strcpy (sym + 1, sym_name);

      stub = alloca (strlen (stub_name) + 2);
      if (stub_name[0] == '*' || stub_name[0] == '&')
	strcpy (stub, stub_name + 1);
      else
	stub[0] = '_', strcpy (stub + 1, stub_name);

      machopic_output_stub (asm_out_file, sym, stub);
    }

  for (temp = machopic_non_lazy_pointers;
       temp != NULL_TREE;
       temp = TREE_CHAIN (temp))
    {
      const char *const sym_name = IDENTIFIER_POINTER (TREE_VALUE (temp));
      const char *const lazy_name = IDENTIFIER_POINTER (TREE_PURPOSE (temp));

      if (! TREE_USED (temp))
	continue;

      if (machopic_ident_defined_p (TREE_VALUE (temp)))
	{
	  data_section ();
	  assemble_align (GET_MODE_ALIGNMENT (Pmode));
	  assemble_label (lazy_name);
	  assemble_integer (gen_rtx_SYMBOL_REF (Pmode, sym_name),
			    GET_MODE_SIZE (Pmode),
			    GET_MODE_ALIGNMENT (Pmode), 1);
	}
      else
	{
	  machopic_nl_symbol_ptr_section ();
	  assemble_name (asm_out_file, lazy_name);
	  fprintf (asm_out_file, ":\n");

	  fprintf (asm_out_file, "\t.indirect_symbol ");
	  assemble_name (asm_out_file, sym_name);
	  fprintf (asm_out_file, "\n");

	  assemble_integer (const0_rtx, GET_MODE_SIZE (Pmode),
			    GET_MODE_ALIGNMENT (Pmode), 1);
	}
    }
}

int
machopic_operand_p (rtx op)
{
  if (MACHOPIC_JUST_INDIRECT)
    {
      while (GET_CODE (op) == CONST)
	op = XEXP (op, 0);

      if (GET_CODE (op) == SYMBOL_REF)
	return machopic_name_defined_p (XSTR (op, 0));
      else
	return 0;
    }

  while (GET_CODE (op) == CONST)
    op = XEXP (op, 0);

  if (GET_CODE (op) == MINUS
      && GET_CODE (XEXP (op, 0)) == SYMBOL_REF
      && GET_CODE (XEXP (op, 1)) == SYMBOL_REF
      && machopic_name_defined_p (XSTR (XEXP (op, 0), 0))
      && machopic_name_defined_p (XSTR (XEXP (op, 1), 0)))
      return 1;

  return 0;
}

/* This function records whether a given name corresponds to a defined
   or undefined function or variable, for machopic_classify_ident to
   use later.  */

void
darwin_encode_section_info (tree decl, rtx rtl, int first ATTRIBUTE_UNUSED)
{
  char code = '\0';
  int defined = 0;
  rtx sym_ref;
  const char *orig_str;
  char *new_str;
  size_t len, new_len;

  /* Do the standard encoding things first.  */
  default_encode_section_info (decl, rtl, first);

  /* With the introduction of symbol_ref flags, some of the following
     code has become redundant and should be removed at some point.  */

  if ((TREE_CODE (decl) == FUNCTION_DECL
       || TREE_CODE (decl) == VAR_DECL)
      && !DECL_EXTERNAL (decl)
      && ((TREE_STATIC (decl)
	   && (!DECL_COMMON (decl) || !TREE_PUBLIC (decl)))
	  || (!DECL_COMMON (decl) && DECL_INITIAL (decl)
	      && DECL_INITIAL (decl) != error_mark_node)))
    defined = 1;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    code = (defined ? 'T' : 't');
  else if (TREE_CODE (decl) == VAR_DECL)
    code = (defined ? 'D' : 'd');

  if (code == '\0')
    return;

  sym_ref = XEXP (rtl, 0);
  orig_str = XSTR (sym_ref, 0);
  len = strlen (orig_str) + 1;

  if (orig_str[0] == '!')
    {
      /* Already encoded; see if we need to change it.  */
      if (code == orig_str[1])
	return;
      /* Yes, tweak a copy of the name and put it in a new string.  */
      new_str = alloca (len);
      memcpy (new_str, orig_str, len);
      new_str[1] = code;
      XSTR (sym_ref, 0) = ggc_alloc_string (new_str, len);
    }
  else
    {
      /* Add the encoding.  */
      new_len = len + 4;
      new_str = alloca (new_len);
      new_str[0] = '!';
      new_str[1] = code;
      new_str[2] = '_';
      new_str[3] = '_';
      memcpy (new_str + 4, orig_str, len);
      XSTR (sym_ref, 0) = ggc_alloc_string (new_str, new_len);
    }
  /* The non-lazy pointer list may have captured references to the
     old encoded name, change them.  */
  if (TREE_CODE (decl) == VAR_DECL)
    update_non_lazy_ptrs (XSTR (sym_ref, 0));
  else
    update_stubs (XSTR (sym_ref, 0));
}

/* Undo the effects of the above.  */

const char *
darwin_strip_name_encoding (const char *str)
{
  return str[0] == '!' ? str + 4 : str;
}

/* Scan the list of non-lazy pointers and update any recorded names whose
   stripped name matches the argument.  */

static void
update_non_lazy_ptrs (const char *name)
{
  const char *name1, *name2;
  tree temp;

  name1 = darwin_strip_name_encoding (name);

  for (temp = machopic_non_lazy_pointers;
       temp != NULL_TREE;
       temp = TREE_CHAIN (temp))
    {
      const char *sym_name = IDENTIFIER_POINTER (TREE_VALUE (temp));

      if (*sym_name == '!')
	{
	  name2 = darwin_strip_name_encoding (sym_name);
	  if (strcmp (name1, name2) == 0)
	    {
	      /* FIXME: This breaks the identifier hash table.  */
	      IDENTIFIER_NODE_CHECK (TREE_VALUE (temp))->identifier.id.str
		= (unsigned char *) name;
	      break;
	    }
	}
    }
}

/* Scan the list of stubs and update any recorded names whose
   stripped name matches the argument.  */

static void
update_stubs (const char *name)
{
  const char *name1, *name2;
  tree temp;

  name1 = darwin_strip_name_encoding (name);

  for (temp = machopic_stubs;
       temp != NULL_TREE;
       temp = TREE_CHAIN (temp))
    {
      const char *sym_name = IDENTIFIER_POINTER (TREE_VALUE (temp));

      if (*sym_name == '!')
	{
	  name2 = darwin_strip_name_encoding (sym_name);
	  if (strcmp (name1, name2) == 0)
	    {
	      /* FIXME: This breaks the identifier hash table.  */
	      IDENTIFIER_NODE_CHECK (TREE_VALUE (temp))->identifier.id.str
		= (unsigned char *) name;
	      break;
	    }
	}
    }
}

void
machopic_select_section (tree exp, int reloc,
			 unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  void (*base_function)(void);

  if (decl_readonly_section_1 (exp, reloc, MACHOPIC_INDIRECT))
    base_function = readonly_data_section;
  else if (TREE_READONLY (exp) || TREE_CONSTANT (exp))
    base_function = const_data_section;
  else
    base_function = data_section;

  if (TREE_CODE (exp) == STRING_CST
      && ((size_t) TREE_STRING_LENGTH (exp)
	  == strlen (TREE_STRING_POINTER (exp)) + 1)
      && ! flag_writable_strings)
    cstring_section ();
  else if ((TREE_CODE (exp) == INTEGER_CST || TREE_CODE (exp) == REAL_CST)
	   && flag_merge_constants)
    {
      tree size = TYPE_SIZE (TREE_TYPE (exp));

      if (TREE_CODE (size) == INTEGER_CST &&
	  TREE_INT_CST_LOW (size) == 4 &&
	  TREE_INT_CST_HIGH (size) == 0)
	literal4_section ();
      else if (TREE_CODE (size) == INTEGER_CST &&
	       TREE_INT_CST_LOW (size) == 8 &&
	       TREE_INT_CST_HIGH (size) == 0)
	literal8_section ();
      else
	base_function ();
    }
  else if (TREE_CODE (exp) == CONSTRUCTOR
	   && TREE_TYPE (exp)
	   && TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
	   && TYPE_NAME (TREE_TYPE (exp)))
    {
      tree name = TYPE_NAME (TREE_TYPE (exp));
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);
      if (!strcmp (IDENTIFIER_POINTER (name), "NSConstantString"))
	objc_constant_string_object_section ();
      else if (!strcmp (IDENTIFIER_POINTER (name), "NXConstantString"))
	objc_string_object_section ();
      else
	base_function ();
    }
  else if (TREE_CODE (exp) == VAR_DECL &&
	   DECL_NAME (exp) &&
	   TREE_CODE (DECL_NAME (exp)) == IDENTIFIER_NODE &&
	   IDENTIFIER_POINTER (DECL_NAME (exp)) &&
	   !strncmp (IDENTIFIER_POINTER (DECL_NAME (exp)), "_OBJC_", 6))
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (exp));

      if (!strncmp (name, "_OBJC_CLASS_METHODS_", 20))
	objc_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_INSTANCE_METHODS_", 23))
	objc_inst_meth_section ();
      else if (!strncmp (name, "_OBJC_CATEGORY_CLASS_METHODS_", 20))
	objc_cat_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_CATEGORY_INSTANCE_METHODS_", 23))
	objc_cat_inst_meth_section ();
      else if (!strncmp (name, "_OBJC_CLASS_VARIABLES_", 22))
	objc_class_vars_section ();
      else if (!strncmp (name, "_OBJC_INSTANCE_VARIABLES_", 25))
	objc_instance_vars_section ();
      else if (!strncmp (name, "_OBJC_CLASS_PROTOCOLS_", 22))
	objc_cat_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_CLASS_NAME_", 17))
	objc_class_names_section ();
      else if (!strncmp (name, "_OBJC_METH_VAR_NAME_", 20))
	objc_meth_var_names_section ();
      else if (!strncmp (name, "_OBJC_METH_VAR_TYPE_", 20))
	objc_meth_var_types_section ();
      else if (!strncmp (name, "_OBJC_CLASS_REFERENCES", 22))
	objc_cls_refs_section ();
      else if (!strncmp (name, "_OBJC_CLASS_", 12))
	objc_class_section ();
      else if (!strncmp (name, "_OBJC_METACLASS_", 16))
	objc_meta_class_section ();
      else if (!strncmp (name, "_OBJC_CATEGORY_", 15))
	objc_category_section ();
      else if (!strncmp (name, "_OBJC_SELECTOR_REFERENCES", 25))
	objc_selector_refs_section ();
      else if (!strncmp (name, "_OBJC_SELECTOR_FIXUP", 20))
	objc_selector_fixup_section ();
      else if (!strncmp (name, "_OBJC_SYMBOLS", 13))
	objc_symbols_section ();
      else if (!strncmp (name, "_OBJC_MODULES", 13))
	objc_module_info_section ();
      else if (!strncmp (name, "_OBJC_IMAGE_INFO", 16))
	objc_image_info_section ();
      else if (!strncmp (name, "_OBJC_PROTOCOL_INSTANCE_METHODS_", 32))
	objc_cat_inst_meth_section ();
      else if (!strncmp (name, "_OBJC_PROTOCOL_CLASS_METHODS_", 29))
	objc_cat_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_PROTOCOL_REFS_", 20))
	objc_cat_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_PROTOCOL_", 15))
	objc_protocol_section ();
      else
	base_function ();
    }
  else
    base_function ();
}

/* This can be called with address expressions as "rtx".
   They must go in "const".  */

void
machopic_select_rtx_section (enum machine_mode mode, rtx x,
			     unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  if (GET_MODE_SIZE (mode) == 8)
    literal8_section ();
  else if (GET_MODE_SIZE (mode) == 4
	   && (GET_CODE (x) == CONST_INT
	       || GET_CODE (x) == CONST_DOUBLE))
    literal4_section ();
  else if (MACHOPIC_INDIRECT
	   && (GET_CODE (x) == SYMBOL_REF
	       || GET_CODE (x) == CONST
	       || GET_CODE (x) == LABEL_REF))
    const_data_section ();
  else
    const_section ();
}

void
machopic_asm_out_constructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  if (MACHOPIC_INDIRECT)
    mod_init_section ();
  else
    constructor_section ();
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);

  if (! MACHOPIC_INDIRECT)
    fprintf (asm_out_file, ".reference .constructors_used\n");
}

void
machopic_asm_out_destructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  if (MACHOPIC_INDIRECT)
    mod_term_section ();
  else
    destructor_section ();
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);

  if (! MACHOPIC_INDIRECT)
    fprintf (asm_out_file, ".reference .destructors_used\n");
}

void
darwin_globalize_label (FILE *stream, const char *name)
{
  if (!!strncmp (name, "_OBJC_", 6))
    default_globalize_label (stream, name);
}

/* Emit an assembler directive to set visibility for a symbol.  The
   only supported visibilities are VISIBILITY_DEFAULT and
   VISIBILITY_HIDDEN; the latter corresponds to Darwin's "private
   extern".  There is no MACH-O equivalent of ELF's
   VISIBILITY_INTERNAL or VISIBILITY_PROTECTED. */

void 
darwin_assemble_visibility (tree decl, int vis)
{
  if (vis == VISIBILITY_DEFAULT)
    ;
  else if (vis == VISIBILITY_HIDDEN)
    {
      fputs ("\t.private_extern ", asm_out_file);
      assemble_name (asm_out_file,
		     (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl))));
      fputs ("\n", asm_out_file);
    }
  else
    warning ("internal and protected visibility attributes not supported"
	     "in this configuration; ignored");
}

/* Output a difference of two labels that will be an assembly time
   constant if the two labels are local.  (.long lab1-lab2 will be
   very different if lab1 is at the boundary between two sections; it
   will be relocated according to the second section, not the first,
   so one ends up with a difference between labels in different
   sections, which is bad in the dwarf2 eh context for instance.)  */

static int darwin_dwarf_label_counter;

void
darwin_asm_output_dwarf_delta (FILE *file, int size ATTRIBUTE_UNUSED,
			       const char *lab1, const char *lab2)
{
  const char *p = lab1 + (lab1[0] == '*');
  int islocaldiff = (p[0] == 'L');

  if (islocaldiff)
    fprintf (file, "\t.set L$set$%d,", darwin_dwarf_label_counter);
  else
    fprintf (file, "\t%s\t", ".long");
  assemble_name (file, lab1);
  fprintf (file, "-");
  assemble_name (file, lab2);
  if (islocaldiff)
    fprintf (file, "\n\t.long L$set$%d", darwin_dwarf_label_counter++);
}

void
darwin_file_end (void)
{
  machopic_finish (asm_out_file);
  if (strcmp (lang_hooks.name, "GNU C++") == 0)
    {
      constructor_section ();
      destructor_section ();
      ASM_OUTPUT_ALIGN (asm_out_file, 1);
    }
}

#include "gt-darwin.h"
