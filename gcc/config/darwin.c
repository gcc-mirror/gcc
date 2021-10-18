/* Functions for generic Darwin as target machine for GNU C compiler.
   Copyright (C) 1989-2021 Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "insn-config.h"
#include "emit-rtl.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "output.h"
#include "varasm.h"
#include "stor-layout.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "targhooks.h"
#include "toplev.h"
#include "lto-section-names.h"
#include "intl.h"
#include "optabs.h"
#include "flags.h"
#include "opts.h"

/* Fix and Continue.

   NOTES:
   1) this facility requires suitable support from a modified version
   of GDB, which is not provided on any system after MacOS 10.7/Darwin11.
   2) There is no support for this in any X86 version of the FSF compiler.

   Fix and continue was used on some earlier MacOS systems for rapid turn
   around debugging.  When code is compiled with the -mfix-and-continue
   flag, two changes are made to the generated code that allow the system
   to do things that it would normally not be able to do easily.  These
   changes allow gdb to load in recompilation of a translation unit that
   has been changed into a running program and replace existing functions
   and methods of that translation unit with versions of those functions
   and methods from the newly compiled translation unit.  The new functions
   access the existing static symbols from the old translation unit, if the
   symbol existed in the unit to be replaced, and from the new translation
   unit, otherwise.

   The changes are to insert 5 nops at the beginning of all functions
   and to use indirection to get at static symbols.  The 5 nops
   are required by consumers of the generated code.  Currently, gdb
   uses this to patch in a jump to the overriding function, this
   allows all uses of the old name to forward to the replacement,
   including existing function pointers and virtual methods.  See
   rs6000_emit_prologue for the code that handles the nop insertions.

   The added indirection allows gdb to redirect accesses to static
   symbols from the newly loaded translation unit to the existing
   symbol, if any.  @code{static} symbols are special and are handled by
   setting the second word in the .non_lazy_symbol_pointer data
   structure to symbol.  See indirect_data for the code that handles
   the extra indirection, and machopic_output_indirection and its use
   of MACHO_SYMBOL_FLAG_STATIC for the code that handles @code{static}
   symbol indirection.  */

typedef struct GTY(()) cdtor_record {
  rtx symbol;
  int priority;		/* [con/de]structor priority */
  int position;		/* original position */
} cdtor_record;

static GTY(()) vec<cdtor_record, va_gc> *ctors = NULL;
static GTY(()) vec<cdtor_record, va_gc> *dtors = NULL;

/* A flag to determine whether we are running c++ or obj-c++.  This has to be
   settable from non-c-family contexts too (i.e. we can't use the c_dialect_
   functions).  */
int darwin_running_cxx;

/* Some code-gen now depends on OS major version numbers (at least).  */
int generating_for_darwin_version ;

/* For older linkers we need to emit special sections (marked 'coalesced') for
   for weak or single-definition items.  */
static bool ld_uses_coal_sects = false;

/* Very old (ld_classic) linkers need a symbol to mark the start of
   each FDE.  */
static bool ld_needs_eh_markers = false;

/* Emit a section-start symbol for mod init and term sections.  */
static bool ld_init_term_start_labels = false;

/* Section names.  */
section * darwin_sections[NUM_DARWIN_SECTIONS];

/* While we transition to using in-tests instead of ifdef'd code.  */
#if !HAVE_lo_sum
#define gen_macho_high(m,a,b) (a)
#define gen_macho_low(m,a,b,c) (a)
#endif

/* True if we're setting __attribute__ ((ms_struct)).  */
int darwin_ms_struct = false;

/* Earlier versions of Darwin as do not recognize an alignment field in
   .comm directives, this should be set for versions that allow it.  */
int emit_aligned_common = false;

/* A get_unnamed_section callback used to switch to an ObjC section.
   DIRECTIVE is as for output_section_asm_op.  */

static void
output_objc_section_asm_op (const void *directive)
{
  static bool been_here = false;

  /* The NeXT ObjC Runtime requires these sections to be present and in
     order in the object.  The code below implements this by emitting
     a section header for each ObjC section the first time that an ObjC
     section is requested.  */
  if (darwin_symbol_stubs && ! been_here)
    {
      section *saved_in_section = in_section;
      static const enum darwin_section_enum tomark[] =
	{
	  /* written, cold -> hot */
	  objc_cat_cls_meth_section,
	  objc_cat_inst_meth_section,
	  objc_string_object_section,
	  objc_constant_string_object_section,
	  objc_selector_refs_section,
	  objc_selector_fixup_section,
	  objc_cls_refs_section,
	  objc_class_section,
	  objc_meta_class_section,
	  /* shared, hot -> cold */
	  objc_cls_meth_section,
	  objc_inst_meth_section,
	  objc_protocol_section,
	  objc_class_names_section,
	  objc_meth_var_types_section,
	  objc_meth_var_names_section,
	  objc_category_section,
	  objc_class_vars_section,
	  objc_instance_vars_section,
	  objc_module_info_section,
	  objc_symbols_section,
	};
      /* ABI=1 */
      static const enum darwin_section_enum tomarkv1[] =
	{
	  objc1_protocol_ext_section,
	  objc1_class_ext_section,
	  objc1_prop_list_section
	} ;
      /* ABI=2 */
      static const enum darwin_section_enum tomarkv2[] =
	{
	  objc2_method_names_section,
	  objc2_message_refs_section,
	  objc2_selector_refs_section,
	  objc2_ivar_section,
	  objc2_classdefs_section,
	  objc2_metadata_section,
	  objc2_classrefs_section,
	  objc2_class_names_section,
	  objc2_classlist_section,
	  objc2_categorylist_section,
	  objc2_nonlazy_class_section,
	  objc2_nonlazy_category_section,
	  objc2_protocollist_section,
	  objc2_protocolrefs_section,
	  objc2_super_classrefs_section,
	  objc2_constant_string_object_section,
	  objc2_image_info_section,
	} ;
      size_t i;

      been_here = true;
      if (flag_objc_abi < 2)
	{
	  for (i = 0; i < ARRAY_SIZE (tomark); i++)
	    switch_to_section (darwin_sections[tomark[i]]);
	  if (flag_objc_abi == 1)
	    for (i = 0; i < ARRAY_SIZE (tomarkv1); i++)
	      switch_to_section (darwin_sections[tomarkv1[i]]);
	}
      else
	for (i = 0; i < ARRAY_SIZE (tomarkv2); i++)
	  switch_to_section (darwin_sections[tomarkv2[i]]);
      /* Make sure we don't get varasm.c out of sync with us.  */
      switch_to_section (saved_in_section);
    }
  output_section_asm_op (directive);
}


/* Private flag applied to disable section-anchors in a particular section.  */
#define SECTION_NO_ANCHOR SECTION_MACH_DEP


/* Implement TARGET_ASM_INIT_SECTIONS.  */

void
darwin_init_sections (void)
{
#define DEF_SECTION(NAME, FLAGS, DIRECTIVE, OBJC)		\
  darwin_sections[NAME] =					\
    get_unnamed_section (FLAGS, (OBJC				\
				 ? output_objc_section_asm_op	\
				 : output_section_asm_op),	\
			 "\t" DIRECTIVE);
#include "config/darwin-sections.def"
#undef DEF_SECTION

  readonly_data_section = darwin_sections[const_section];
  exception_section = darwin_sections[darwin_exception_section];
  eh_frame_section = darwin_sections[darwin_eh_frame_section];

  /* If our linker is new enough to coalesce weak symbols, then we
     can just put picbase_thunks into the text section.  */
  if (! ld_uses_coal_sects )
    darwin_sections[picbase_thunk_section] = text_section;
}

int
name_needs_quotes (const char *name)
{
  int c;
  while ((c = *name++) != '\0')
    if (! ISIDNUM (c)
	  && c != '.' && c != '$' && c != '_' )
      return 1;
  return 0;
}

/* Return true if SYM_REF can be used without an indirection.  */
int
machopic_symbol_defined_p (rtx sym_ref)
{
  if (MACHO_SYMBOL_DEFINED_P (sym_ref))
    return true;

  /* If a symbol references local and is not an extern to this
     file, then the symbol might be able to declared as defined.  */
  if (SYMBOL_REF_LOCAL_P (sym_ref) && ! SYMBOL_REF_EXTERNAL_P (sym_ref))
    {
      /* If the symbol references a variable and the variable is a
	 common symbol, then this symbol is not defined.  */
      if (MACHO_SYMBOL_VARIABLE_P (sym_ref))
	{
	  tree decl = SYMBOL_REF_DECL (sym_ref);
	  if (!decl)
	    return true;
	  if (DECL_COMMON (decl))
	    return false;
	}
      return true;
    }
  return false;
}

/* This module assumes that (const (symbol_ref "foo")) is a legal pic
   reference, which will not be changed.  */

enum machopic_addr_class
machopic_classify_symbol (rtx sym_ref)
{
  bool function_p;

  function_p = SYMBOL_REF_FUNCTION_P (sym_ref);
  if (machopic_symbol_defined_p (sym_ref))
    return (function_p
	    ? MACHOPIC_DEFINED_FUNCTION : MACHOPIC_DEFINED_DATA);
  else
    return (function_p
	    ? MACHOPIC_UNDEFINED_FUNCTION : MACHOPIC_UNDEFINED_DATA);
}

#ifndef TARGET_FIX_AND_CONTINUE
#define TARGET_FIX_AND_CONTINUE 0
#endif

/* Indicate when fix-and-continue style code generation is being used
   and when a reference to data should be indirected so that it can be
   rebound in a new translation unit to reference the original instance
   of that data.  Symbol names that are for code generation local to
   the translation unit are bound to the new translation unit;
   currently this means symbols that begin with L or _OBJC_;
   otherwise, we indicate that an indirect reference should be made to
   permit the runtime to rebind new instances of the translation unit
   to the original instance of the data.  */

static int
indirect_data (rtx sym_ref)
{
  int lprefix;
  const char *name;

  /* If we aren't generating fix-and-continue code, don't do anything
     special.  */
  if (TARGET_FIX_AND_CONTINUE == 0)
    return 0;

  /* Otherwise, all symbol except symbols that begin with L or _OBJC_
     are indirected.  Symbols that begin with L and _OBJC_ are always
     bound to the current translation unit as they are used for
     generated local data of the translation unit.  */

  name = XSTR (sym_ref, 0);

  lprefix = (((name[0] == '*' || name[0] == '&')
              && (name[1] == 'L' || (name[1] == '"' && name[2] == 'L')))
	     || (startswith (name, "_OBJC_")));

  return ! lprefix;
}

static int
machopic_data_defined_p (rtx sym_ref)
{
  if (indirect_data (sym_ref))
    return 0;

  switch (machopic_classify_symbol (sym_ref))
    {
    case MACHOPIC_DEFINED_DATA:
    case MACHOPIC_DEFINED_FUNCTION:
      return 1;
    default:
      return 0;
    }
}

void
machopic_define_symbol (rtx mem)
{
  rtx sym_ref;

  gcc_assert (GET_CODE (mem) == MEM);
  sym_ref = XEXP (mem, 0);
  SYMBOL_REF_FLAGS (sym_ref) |= MACHO_SYMBOL_FLAG_DEFINED;
}

/* Return either ORIG or:

     (const:P (unspec:P [ORIG] UNSPEC_MACHOPIC_OFFSET))

   depending on MACHO_DYNAMIC_NO_PIC_P.  */
rtx
machopic_gen_offset (rtx orig)
{
  if (MACHO_DYNAMIC_NO_PIC_P)
    return orig;
  else
    {
      /* Play games to avoid marking the function as needing pic if we
	 are being called as part of the cost-estimation process.  */
      if (current_ir_type () != IR_GIMPLE || currently_expanding_to_rtl)
	crtl->uses_pic_offset_table = 1;
      orig = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, orig),
			     UNSPEC_MACHOPIC_OFFSET);
      return gen_rtx_CONST (Pmode, orig);
    }
}

static GTY(()) const char * function_base_func_name = NULL;
static GTY(()) unsigned current_pic_label_num = 0;
static GTY(()) unsigned emitted_pic_label_num = 0;

/* We need to keep one picbase label per function, but (when we emit code
   to reload the picbase for setjump receiver) we might need to check for
   a second use.  So, only update the picbase label counter when we see a
   new function.  When there's no function decl, we assume that the call is
   from the x86 stub generation code.  */
static void
update_pic_label_number_if_needed (void)
{
  if (current_function_decl)
    {

      const char *current_name =
	IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl));
      if (function_base_func_name != current_name)
	{
	  ++current_pic_label_num;
	  function_base_func_name = current_name;
	}
    }
  else
    {
      ++current_pic_label_num;
      function_base_func_name = "L_machopic_stub_dummy";
    }
}

void
machopic_output_function_base_name (FILE *file)
{
  /* We should only get here for -fPIC.  */
  gcc_checking_assert (MACHOPIC_PURE);

  update_pic_label_number_if_needed ();
  fprintf (file, "L%u$pb", current_pic_label_num);
}

char curr_picbasename[32];

const char *
machopic_get_function_picbase (void)
{
  /* We should only get here for -fPIC.  */
  gcc_checking_assert (MACHOPIC_PURE);

  update_pic_label_number_if_needed ();
  snprintf (curr_picbasename, 32, "L%u$pb", current_pic_label_num);
  return (const char *) curr_picbasename;
}

bool
machopic_should_output_picbase_label (void)
{
  update_pic_label_number_if_needed ();

  if (current_pic_label_num == emitted_pic_label_num)
    return false;

  emitted_pic_label_num = current_pic_label_num;
  return true;
}

/* The suffix attached to non-lazy pointer symbols.  */
#define NON_LAZY_POINTER_SUFFIX "$non_lazy_ptr"
/* The suffix attached to stub symbols.  */
#define STUB_SUFFIX "$stub"

typedef struct GTY ((for_user)) machopic_indirection
{
  /* The SYMBOL_REF for the entity referenced.  */
  rtx symbol;
  /* The name of the stub or non-lazy pointer.  */
  const char * ptr_name;
  /* True iff this entry is for a stub (as opposed to a non-lazy
     pointer).  */
  bool stub_p;
  /* True iff this stub or pointer has been referenced.  */
  bool used;
  /* True iff a non-lazy symbol pointer should be emitted into the .data
     section, rather than the non-lazy symbol pointers section.  The cases
     for which this occurred seem to have been unintentional, and later
     toolchains emit all of the indirections to the 'usual' section.  We
     are keeping this in case it is necessary to preserve compatibility with
     older toolchains.  */
  bool nlsp_in_data_section;
} machopic_indirection;

struct indirection_hasher : ggc_ptr_hash<machopic_indirection>
{
  typedef const char *compare_type;
  static hashval_t hash (machopic_indirection *);
  static bool equal (machopic_indirection *, const char *);
};

/* A table mapping stub names and non-lazy pointer names to
   SYMBOL_REFs for the stubbed-to and pointed-to entities.  */

static GTY (()) hash_table<indirection_hasher> *machopic_indirections;

/* Return a hash value for a SLOT in the indirections hash table.  */

hashval_t
indirection_hasher::hash (machopic_indirection *p)
{
  return htab_hash_string (p->ptr_name);
}

/* Returns true if the KEY is the same as that associated with
   SLOT.  */

bool
indirection_hasher::equal (machopic_indirection *s, const char *k)
{
  return strcmp (s->ptr_name, k) == 0;
}

/* Return the name of the non-lazy pointer (if STUB_P is false) or
   stub (if STUB_B is true) corresponding to the given name.

  PR71767 - If we have a situation like:

global_weak_symbol:
  ....
Lnon_weak_local:
  ....

  ld64 will be unable to split this into two atoms (because the "L" makes
  the second symbol 'invisible').  This means that legitimate direct accesses
  to the second symbol will appear to be direct accesses to an atom of type
  weak, global which are not allowed.

  To avoid this, we make any data-section indirections have a leading 'l'
  (lower-case L) which has a special meaning: linker can see this and use
  it to determine  atoms, but it is not placed into the final symbol table.

  Symbols in the non-lazy symbol pointers section (or stubs) do not have this
  problem because ld64 already knows the size of each entry.
*/

const char *
machopic_indirection_name (rtx sym_ref, bool stub_p)
{
  const char *name = XSTR (sym_ref, 0);
  tree id = maybe_get_identifier (name);
  if (id)
    {
      tree id_orig = id;

      while (IDENTIFIER_TRANSPARENT_ALIAS (id))
	id = TREE_CHAIN (id);
      if (id != id_orig)
	name = IDENTIFIER_POINTER (id);
    }

  const char *prefix = user_label_prefix;
  /* If we are emitting the label 'verbatim' then omit the U_L_P and count
     the name without the leading '*'.  */
  if (name[0] == '*')
    {
      prefix = "";
      ++name;
    }

  /* Here we are undoing a number of causes that placed some indirections
     (apparently erroneously) into the .data section.  Specifically, some
     symbols that are ABI mandated indirections and some hidden symbols
     were being placed there - which cause difficulties with later
     versions of ld64.  Iff (after these checks) some symbol still gets an
     indirection in the data section, we want to adjust the indirection
     name to be linker visible to deal with PR71767 (notes above).  */
  bool nlsp_in_data_section =
       ! MACHO_SYMBOL_MUST_INDIRECT_P (sym_ref)
    && ! MACHO_SYMBOL_HIDDEN_VIS_P (sym_ref)
    && (machopic_symbol_defined_p (sym_ref) || SYMBOL_REF_LOCAL_P (sym_ref))
    && ! indirect_data (sym_ref);

  const char *suffix = stub_p ? STUB_SUFFIX : NON_LAZY_POINTER_SUFFIX;
  /* If the indirection is in the data section, let the linker see it.  */
  char L_or_l = (!stub_p && nlsp_in_data_section) ? 'l' : 'L';
  /* We have mangled symbols with spaces and punctuation which typically
     need surrounding in quotes for the assembler to consume them.  */
  const char *quote = name_needs_quotes (name) ? "\"" : "";
  char *buffer = XALLOCAVEC (char, 2  /* strlen ("&L") or ("&l") */
			     + strlen (prefix)
			     + strlen (name)
			     + strlen (suffix)
			     + 2 * strlen (quote)
			     + 1 /* '\0' */);

  /* Construct the name of the non-lazy pointer or stub.  */
  sprintf (buffer, "&%s%c%s%s%s%s", quote, L_or_l, prefix, name,
	   suffix, quote);

  if (!machopic_indirections)
    machopic_indirections = hash_table<indirection_hasher>::create_ggc (37);

  machopic_indirection **slot
    = machopic_indirections->find_slot_with_hash (buffer,
						  htab_hash_string (buffer),
						  INSERT);
  machopic_indirection *p;
  if (*slot)
    p = *slot;
  else
    {
      p = ggc_alloc<machopic_indirection> ();
      p->symbol = sym_ref;
      p->ptr_name = xstrdup (buffer);
      p->stub_p = stub_p;
      p->used = false;
      p->nlsp_in_data_section = nlsp_in_data_section;
      *slot = p;
    }

  return p->ptr_name;
}

/* If NAME is the name of a stub or a non-lazy pointer , mark the stub
   or non-lazy pointer as used -- and mark the object to which the
   pointer/stub refers as used as well, since the pointer/stub will
   emit a reference to it.  */

void
machopic_validate_stub_or_non_lazy_ptr (const char *name)
{
  machopic_indirection *p
    = machopic_indirections->find_with_hash (name, htab_hash_string (name));
  if (p && ! p->used)
    {
      const char *real_name;
      tree id;

      p->used = true;

      /* Do what output_addr_const will do when we actually call it.  */
      if (SYMBOL_REF_DECL (p->symbol))
	mark_decl_referenced (SYMBOL_REF_DECL (p->symbol));

      real_name = targetm.strip_name_encoding (XSTR (p->symbol, 0));

      id = maybe_get_identifier (real_name);
      if (id)
	mark_referenced (id);
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
      int defined = machopic_data_defined_p (orig);

      if (defined && MACHO_DYNAMIC_NO_PIC_P)
	{
	  if (DARWIN_PPC)
	    {
	  /* Create a new register for CSE opportunities.  */
	  rtx hi_reg = (!can_create_pseudo_p () ? reg : gen_reg_rtx (Pmode));
	  emit_insn (gen_macho_high (Pmode, hi_reg, orig));
	  emit_insn (gen_macho_low (Pmode, reg, hi_reg, orig));
	      return reg;
 	    }
	  else if (DARWIN_X86)
	    return orig;
	  else
	   /* some other cpu -- writeme!  */
	   gcc_unreachable ();
	}
      else if (defined && ! MACHO_SYMBOL_MUST_INDIRECT_P (orig))
	{
	  rtx offset = NULL;
	  if (DARWIN_PPC || HAVE_lo_sum)
	    offset = machopic_gen_offset (orig);

	  if (DARWIN_PPC)
	    {
	  rtx hi_sum_reg = (!can_create_pseudo_p ()
			    ? reg
			    : gen_reg_rtx (Pmode));

	  gcc_assert (reg);

	  emit_insn (gen_rtx_SET (hi_sum_reg,
			      gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
				       gen_rtx_HIGH (Pmode, offset))));
	  emit_insn (gen_rtx_SET (reg,
				  gen_rtx_LO_SUM (Pmode, hi_sum_reg,
						  copy_rtx (offset))));

	  orig = reg;
	    }
	  else if (HAVE_lo_sum)
	    {
	  gcc_assert (reg);

	  emit_insn (gen_rtx_SET (reg, gen_rtx_HIGH (Pmode, offset)));
	  emit_insn (gen_rtx_SET (reg, gen_rtx_LO_SUM (Pmode, reg,
						       copy_rtx (offset))));
	  emit_use (pic_offset_table_rtx);

	  orig = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, reg);
	    }
	  return orig;
	}

      ptr_ref = (gen_rtx_SYMBOL_REF
		 (Pmode,
		  machopic_indirection_name (orig, /*stub_p=*/false)));

      SYMBOL_REF_DATA (ptr_ref) = SYMBOL_REF_DATA (orig);
      SYMBOL_REF_FLAGS (ptr_ref) |= MACHO_SYMBOL_FLAG_INDIRECTION;

      ptr_ref = gen_const_mem (Pmode, ptr_ref);
      machopic_define_symbol (ptr_ref);

      if (DARWIN_X86
          && reg
          && MACHO_DYNAMIC_NO_PIC_P)
	{
	    emit_insn (gen_rtx_SET (reg, ptr_ref));
	    ptr_ref = reg;
	}

      return ptr_ref;
    }
  else if (GET_CODE (orig) == CONST)
    {
      /* If "(const (plus ...", walk the PLUS and return that result.
	 PLUS processing (below) will restore the "(const ..." if
	 appropriate.  */
      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	return machopic_indirect_data_reference (XEXP (orig, 0), reg);
      else
	return orig;
    }
  else if (GET_CODE (orig) == MEM)
    {
      XEXP (ptr_ref, 0) =
		machopic_indirect_data_reference (XEXP (orig, 0), reg);
      return ptr_ref;
    }
  else if (GET_CODE (orig) == PLUS)
    {
      rtx base, result;

      /* Legitimize both operands of the PLUS.  */
      base = machopic_indirect_data_reference (XEXP (orig, 0), reg);
      orig = machopic_indirect_data_reference (XEXP (orig, 1),
					       (base == reg ? 0 : reg));
      if (MACHOPIC_INDIRECT && (GET_CODE (orig) == CONST_INT))
	result = plus_constant (Pmode, base, INTVAL (orig));
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
  return ptr_ref;
}

/* Transform TARGET (a MEM), which is a function call target, to the
   corresponding symbol_stub if necessary.  Return a new MEM.  */

rtx
machopic_indirect_call_target (rtx target)
{
  if (! darwin_symbol_stubs)
    return target;

  if (GET_CODE (target) != MEM)
    return target;

  if (MACHOPIC_INDIRECT
      && GET_CODE (XEXP (target, 0)) == SYMBOL_REF
      && ! MACHO_SYMBOL_DEFINED_P (XEXP (target, 0)))
    {
      rtx sym_ref = XEXP (target, 0);
      const char *stub_name = machopic_indirection_name (sym_ref,
							 /*stub_p=*/true);
      machine_mode mode = GET_MODE (sym_ref);

      XEXP (target, 0) = gen_rtx_SYMBOL_REF (mode, stub_name);
      SYMBOL_REF_DATA (XEXP (target, 0)) = SYMBOL_REF_DATA (sym_ref);
      SYMBOL_REF_FLAGS (XEXP (target, 0)) |= MACHO_SYMBOL_FLAG_INDIRECTION;
      MEM_READONLY_P (target) = 1;
      MEM_NOTRAP_P (target) = 1;
    }

  return target;
}

rtx
machopic_legitimize_pic_address (rtx orig, machine_mode mode, rtx reg)
{
  rtx pic_ref = orig;

  if (! MACHOPIC_INDIRECT)
    return orig;

  /* First handle a simple SYMBOL_REF or LABEL_REF */
  if (GET_CODE (orig) == LABEL_REF
      || GET_CODE (orig) == SYMBOL_REF)
    {
      /* addr(foo) = &func+(foo-func) */
      orig = machopic_indirect_data_reference (orig, reg);

      if (GET_CODE (orig) == PLUS
	  && GET_CODE (XEXP (orig, 0)) == REG)
	{
	  if (reg == 0)
	    return force_reg (mode, orig);

	  emit_move_insn (reg, orig);
	  return reg;
	}

      if (GET_CODE (orig) == MEM)
	{
	  if (reg == 0)
	    {
	      gcc_assert (!lra_in_progress);
	      reg = gen_reg_rtx (Pmode);
	    }

#if HAVE_lo_sum
	  if (MACHO_DYNAMIC_NO_PIC_P
	      && (GET_CODE (XEXP (orig, 0)) == SYMBOL_REF
		  || GET_CODE (XEXP (orig, 0)) == LABEL_REF))
	    {
#if defined (TARGET_TOC)	/* ppc  */
	      rtx temp_reg = (!can_create_pseudo_p ()
			      ? reg :
			      gen_reg_rtx (Pmode));
	      rtx asym = XEXP (orig, 0);
	      rtx mem;

	      emit_insn (gen_macho_high (Pmode, temp_reg, asym));
	      mem = gen_const_mem (GET_MODE (orig),
				   gen_rtx_LO_SUM (Pmode, temp_reg,
						   copy_rtx (asym)));
	      emit_insn (gen_rtx_SET (reg, mem));
#else
	      /* Some other CPU -- WriteMe! but right now there are no other
		 platforms that can use dynamic-no-pic  */
	      gcc_unreachable ();
#endif
	      pic_ref = reg;
	    }
	  else
	  if (GET_CODE (XEXP (orig, 0)) == SYMBOL_REF
	      || GET_CODE (XEXP (orig, 0)) == LABEL_REF)
	    {
	      rtx offset = machopic_gen_offset (XEXP (orig, 0));
#if defined (TARGET_TOC) /* i.e., PowerPC */
	      /* Generating a new reg may expose opportunities for
		 common subexpression elimination.  */
              rtx hi_sum_reg = (!can_create_pseudo_p ()
				? reg
				: gen_reg_rtx (Pmode));
	      rtx mem;
	      rtx sum;

	      sum = gen_rtx_HIGH (Pmode, offset);
	      if (! MACHO_DYNAMIC_NO_PIC_P)
		sum = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, sum);

	      emit_insn (gen_rtx_SET (hi_sum_reg, sum));

	      mem = gen_const_mem (GET_MODE (orig),
				  gen_rtx_LO_SUM (Pmode,
						  hi_sum_reg,
						  copy_rtx (offset)));
	      rtx_insn *insn = emit_insn (gen_rtx_SET (reg, mem));
	      set_unique_reg_note (insn, REG_EQUAL, pic_ref);

	      pic_ref = reg;
#else
	      emit_use (gen_rtx_REG (Pmode, PIC_OFFSET_TABLE_REGNUM));

	      emit_insn (gen_rtx_SET (reg,
				      gen_rtx_HIGH (Pmode,
						    gen_rtx_CONST (Pmode,
								   offset))));
	      emit_insn (gen_rtx_SET (reg,
				  gen_rtx_LO_SUM (Pmode, reg,
					   gen_rtx_CONST (Pmode,
						   	  copy_rtx (offset)))));
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

	      if (lra_in_progress && HARD_REGISTER_P (pic))
		df_set_regs_ever_live (REGNO (pic), true);
	      pic_ref = gen_rtx_PLUS (Pmode, pic,
				      machopic_gen_offset (XEXP (orig, 0)));
	    }

#if !defined (TARGET_TOC)
	  emit_move_insn (reg, pic_ref);
	  pic_ref = gen_const_mem (GET_MODE (orig), reg);
#endif
	}
      else
	{

#if HAVE_lo_sum
	  if (GET_CODE (orig) == SYMBOL_REF
	      || GET_CODE (orig) == LABEL_REF)
	    {
	      rtx offset = machopic_gen_offset (orig);
#if defined (TARGET_TOC) /* i.e., PowerPC */
              rtx hi_sum_reg;

	      if (reg == 0)
		{
		  gcc_assert (!lra_in_progress);
		  reg = gen_reg_rtx (Pmode);
		}

	      hi_sum_reg = reg;

	      emit_insn (gen_rtx_SET (hi_sum_reg,
				      (MACHO_DYNAMIC_NO_PIC_P)
				      ? gen_rtx_HIGH (Pmode, offset)
				      : gen_rtx_PLUS (Pmode,
						      pic_offset_table_rtx,
						      gen_rtx_HIGH (Pmode,
								    offset))));
	      emit_insn (gen_rtx_SET (reg,
				      gen_rtx_LO_SUM (Pmode,
						      hi_sum_reg,
						      copy_rtx (offset))));
	      pic_ref = reg;
#else
	      emit_insn (gen_rtx_SET (reg, gen_rtx_HIGH (Pmode, offset)));
	      emit_insn (gen_rtx_SET (reg,
				      gen_rtx_LO_SUM (Pmode, reg,
						      copy_rtx (offset))));
	      pic_ref = gen_rtx_PLUS (Pmode,
				      pic_offset_table_rtx, reg);
#endif
	    }
	  else
#endif  /*  HAVE_lo_sum  */
	    {
	      if (REG_P (orig)
	          || GET_CODE (orig) == SUBREG)
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

		  if (lra_in_progress && HARD_REGISTER_P (pic))
		    df_set_regs_ever_live (REGNO (pic), true);
		  pic_ref = gen_rtx_PLUS (Pmode,
					  pic,
					  machopic_gen_offset (orig));
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
	  pic_ref = plus_constant (Pmode, base, INTVAL (orig));
	  is_complex = 1;
	}
      else
	pic_ref = gen_rtx_PLUS (Pmode, base, orig);

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
      addr = replace_equiv_address (orig, addr);
      emit_move_insn (reg, addr);
      pic_ref = reg;
    }

  return pic_ref;
}

/* Callbacks to output the stub or non-lazy pointers.
   Each works on the item in *SLOT,if it has been used.
   DATA is the FILE* for assembly output.
   Called from htab_traverses, invoked from machopic_finish().  */

int
machopic_output_data_section_indirection (machopic_indirection **slot,
					  FILE *asm_out_file)
{
  machopic_indirection *p = *slot;

  if (!p->used || !p->nlsp_in_data_section)
    return 1;

  rtx symbol = p->symbol;
  /* The original symbol name.  */
  const char *sym_name = XSTR (symbol, 0);
  /* The name of the indirection symbol.  */
  const char *ptr_name = p->ptr_name;

  switch_to_section (data_section);
  assemble_align (GET_MODE_ALIGNMENT (Pmode));
  assemble_label (asm_out_file, ptr_name);
  assemble_integer (gen_rtx_SYMBOL_REF (Pmode, sym_name),
		    GET_MODE_SIZE (Pmode),
		    GET_MODE_ALIGNMENT (Pmode), 1);

  return 1;
}

int
machopic_output_stub_indirection (machopic_indirection **slot,
				  FILE *asm_out_file)
{
  machopic_indirection *p = *slot;

  if (!p->used || !p->stub_p)
    return 1;

  rtx symbol = p->symbol;
  /* The original symbol name.  */
  const char *sym_name = XSTR (symbol, 0);
  /* The name of the stub symbol.  */
  const char *ptr_name = p->ptr_name;

  tree id = maybe_get_identifier (sym_name);
  if (id)
    {
      tree id_orig = id;

      while (IDENTIFIER_TRANSPARENT_ALIAS (id))
	id = TREE_CHAIN (id);
      if (id != id_orig)
	sym_name = IDENTIFIER_POINTER (id);
    }

  char *sym = XALLOCAVEC (char, strlen (sym_name) + 2);
  if (sym_name[0] == '*' || sym_name[0] == '&')
    strcpy (sym, sym_name + 1);
  else if (sym_name[0] == '-' || sym_name[0] == '+')
    strcpy (sym, sym_name);
  else
    sprintf (sym, "%s%s", user_label_prefix, sym_name);

  char *stub = XALLOCAVEC (char, strlen (ptr_name) + 2);
  if (ptr_name[0] == '*' || ptr_name[0] == '&')
    strcpy (stub, ptr_name + 1);
  else
    sprintf (stub, "%s%s", user_label_prefix, ptr_name);

  machopic_output_stub (asm_out_file, sym, stub);

  return 1;
}

int
machopic_output_indirection (machopic_indirection **slot, FILE *asm_out_file)
{
  machopic_indirection *p = *slot;

  if (!p->used || p->stub_p || p->nlsp_in_data_section)
    return 1;

  rtx symbol = p->symbol;
  /* The original symbol name.  */
  const char *sym_name = XSTR (symbol, 0);
  /* The nonlazy-stub symbol name.  */
  const char *ptr_name = p->ptr_name;

  switch_to_section (darwin_sections[machopic_nl_symbol_ptr_section]);

  /* Mach-O symbols are passed around in code through indirect references and
     the original symbol_ref hasn't passed through the generic handling and
     reference-catching in output_operand, so we need to manually mark weak
     references as such.  */

  if (SYMBOL_REF_WEAK (symbol))
    {
      tree decl = SYMBOL_REF_DECL (symbol);
      gcc_checking_assert (DECL_P (decl));

      if (decl != NULL_TREE
	  && DECL_EXTERNAL (decl) && TREE_PUBLIC (decl)
	  /* Handle only actual external-only definitions, not
	     e.g. extern inline code or variables for which
	     storage has been allocated.  */
	  && !TREE_STATIC (decl))
	{
	  fputs ("\t.weak_reference ", asm_out_file);
	  assemble_name (asm_out_file, sym_name);
	  fputc ('\n', asm_out_file);
	}
    }

  assemble_name (asm_out_file, ptr_name);
  fprintf (asm_out_file, ":\n");

  fprintf (asm_out_file, "\t.indirect_symbol ");
  assemble_name (asm_out_file, sym_name);
  fprintf (asm_out_file, "\n");

  /* Variables that are marked with MACHO_SYMBOL_FLAG_STATIC need to
     have their symbol name instead of 0 in the second entry of
     the non-lazy symbol pointer data structure when they are
     defined.  This allows the runtime to rebind newer instances
     of the translation unit with the original instance of the
     symbol.  */

  rtx init = const0_rtx;
  if (MACHO_SYMBOL_STATIC_P (symbol) && machopic_symbol_defined_p (symbol))
    init = gen_rtx_SYMBOL_REF (Pmode, sym_name);

  assemble_integer (init, GET_MODE_SIZE (Pmode),
		    GET_MODE_ALIGNMENT (Pmode), 1);

  return 1;
}

static void
machopic_finish (FILE *asm_out_file)
{
  if (!machopic_indirections)
    return;

  /* First output an symbol indirections that have been placed into .data
     (we don't expect these now).  */
  machopic_indirections->traverse_noresize
    <FILE *, machopic_output_data_section_indirection> (asm_out_file);

  machopic_indirections->traverse_noresize
    <FILE *, machopic_output_stub_indirection> (asm_out_file);

  machopic_indirections->traverse_noresize
    <FILE *, machopic_output_indirection> (asm_out_file);
}

int
machopic_operand_p (rtx op)
{
  if (MACHOPIC_JUST_INDIRECT)
    return (GET_CODE (op) == SYMBOL_REF
	    && machopic_symbol_defined_p (op));
  else
    return (GET_CODE (op) == CONST
	    && GET_CODE (XEXP (op, 0)) == UNSPEC
	    && XINT (XEXP (op, 0), 1) == UNSPEC_MACHOPIC_OFFSET);
}

/* This function:
   computes and caches a series of flags that characterise the symbol's
   properties that affect Mach-O code gen (including accidental cases
   from older toolchains).

   TODO:
   Here we also need to do enough analysis to determine if a symbol's
   name needs to be made linker-visible.  This is more tricky - since
   it depends on whether we've previously seen a global weak definition
   in the same section.
   */

void
darwin_encode_section_info (tree decl, rtx rtl, int first)
{
  /* Careful not to prod global register variables.  */
  if (!MEM_P (rtl))
    return;

  /* Do the standard encoding things first; this sets:
     SYMBOL_FLAG_FUNCTION,
     SYMBOL_FLAG_LOCAL, (binds_local_p)
     TLS_MODEL, SYMBOL_FLAG_SMALL
     SYMBOL_FLAG_EXTERNAL.  */
  default_encode_section_info (decl, rtl, first);

  if (! VAR_OR_FUNCTION_DECL_P (decl))
    return;

  rtx sym_ref = XEXP (rtl, 0);
  if (VAR_P (decl))
    SYMBOL_REF_FLAGS (sym_ref) |= MACHO_SYMBOL_FLAG_VARIABLE;

  /* Only really common if there's no initialiser.  */
  bool really_common_p = (DECL_COMMON (decl)
			  && (DECL_INITIAL (decl) == NULL
			      || (!in_lto_p
				  && DECL_INITIAL (decl) == error_mark_node)));

  /* For Darwin, if we have specified visibility and it's not the default
     that's counted 'hidden'.  */
  if (DECL_VISIBILITY_SPECIFIED (decl)
      && DECL_VISIBILITY (decl) != VISIBILITY_DEFAULT)
    SYMBOL_REF_FLAGS (sym_ref) |= MACHO_SYMBOL_FLAG_HIDDEN_VIS;

  if (!DECL_EXTERNAL (decl)
      && (!TREE_PUBLIC (decl) || !DECL_WEAK (decl))
      && ! lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))
      && ((TREE_STATIC (decl)
	   && (!DECL_COMMON (decl) || !TREE_PUBLIC (decl)))
	  || (!DECL_COMMON (decl) && DECL_INITIAL (decl)
	      && DECL_INITIAL (decl) != error_mark_node)))
    SYMBOL_REF_FLAGS (sym_ref) |= MACHO_SYMBOL_FLAG_DEFINED;

  if (! TREE_PUBLIC (decl))
    SYMBOL_REF_FLAGS (sym_ref) |= MACHO_SYMBOL_FLAG_STATIC;

  /* Short cut check for Darwin 'must indirect' rules.  */
  if (really_common_p
      || (DECL_WEAK (decl) && ! MACHO_SYMBOL_HIDDEN_VIS_P (sym_ref))
      || lookup_attribute ("weakref", DECL_ATTRIBUTES (decl)))
     SYMBOL_REF_FLAGS (sym_ref) |= MACHO_SYMBOL_FLAG_MUST_INDIRECT;

#if DARWIN_PPC
  /* Objective C V2 (m64) IVAR offset refs from Apple GCC-4.x have an
     indirection for m64 code on PPC.  Historically, these indirections
     also appear in the .data section.  */
  tree o2meta = lookup_attribute ("OBJC2META", DECL_ATTRIBUTES (decl));
  o2meta = o2meta ? TREE_VALUE (o2meta) : NULL_TREE;

  if (o2meta && startswith (IDENTIFIER_POINTER (o2meta), "V2_IVRF"))
    SYMBOL_REF_FLAGS (sym_ref) |= MACHO_SYMBOL_FLAG_MUST_INDIRECT;
#endif
}

void
darwin_mark_decl_preserved (const char *name)
{
  /* Actually we shouldn't mark any local symbol this way, but for now
     this only happens with ObjC meta-data.  */
  if (darwin_label_is_anonymous_local_objc_name (name))
    return;

  fprintf (asm_out_file, "\t.no_dead_strip ");
  assemble_name (asm_out_file, name);
  fputc ('\n', asm_out_file);
}

static section *
darwin_rodata_section (int use_coal, bool zsize, int reloc)
{
  return (use_coal
	  ? darwin_sections[const_coal_section]
	  : (zsize ? darwin_sections[zobj_const_section]
		   : reloc ? darwin_sections[const_data_section]
			   : darwin_sections[const_section]));
}

static section *
darwin_mergeable_string_section (tree exp,
				 unsigned HOST_WIDE_INT align)
{
  /* Darwin's ld expects to see non-writable string literals in the .cstring
     section.  Later versions of ld check and complain when CFStrings are
     enabled.  Therefore we shall force the strings into .cstring since we
     don't support writable ones anyway.  */
  if ((darwin_constant_cfstrings || flag_merge_constants)
      && TREE_CODE (exp) == STRING_CST
      && TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE
      && align <= 256
      && (int_size_in_bytes (TREE_TYPE (exp))
	  == TREE_STRING_LENGTH (exp))
      && ((size_t) TREE_STRING_LENGTH (exp)
	  == strlen (TREE_STRING_POINTER (exp)) + 1))
    return darwin_sections[cstring_section];

  if (DARWIN_SECTION_ANCHORS && flag_section_anchors
      && TREE_CODE (exp) == STRING_CST
      && TREE_STRING_LENGTH (exp) == 0)
    return darwin_sections[zobj_const_section];

  return readonly_data_section;
}

#ifndef HAVE_GAS_LITERAL16
#define HAVE_GAS_LITERAL16 0
#endif

static section *
darwin_mergeable_constant_section (tree exp,
				   unsigned HOST_WIDE_INT align,
				   bool zsize)
{
  if (zsize)
    return darwin_sections[zobj_const_section];

  machine_mode mode = DECL_MODE (exp);
  if (!flag_merge_constants
      || mode == VOIDmode
      || mode == BLKmode
      || align < 8
      || align > 256
      || (align & (align -1)) != 0)
    return readonly_data_section;

  /* This will ICE if the mode is not a constant size, but that is reasonable,
     since one cannot put a variable-sized thing into a constant section, we
     shouldn't be trying.  */
  const unsigned int modesize = GET_MODE_BITSIZE (mode).to_constant ();

  if (modesize > align)
    return readonly_data_section;

  tree size = TYPE_SIZE_UNIT (TREE_TYPE (exp));

  if (TREE_CODE (size) != INTEGER_CST)
    return readonly_data_section;

  unsigned isize = TREE_INT_CST_LOW (size);
  if (isize == 4)
    return darwin_sections[literal4_section];
  else if (isize == 8)
    return darwin_sections[literal8_section];
  else if (HAVE_GAS_LITERAL16
	   && TARGET_64BIT
	   && isize == 16)
    return darwin_sections[literal16_section];

  return readonly_data_section;
}

section *
darwin_tm_clone_table_section (void)
{
  return get_named_section (NULL,
			    "__DATA,__tm_clone_table,regular,no_dead_strip",
			    3);
}

int
machopic_reloc_rw_mask (void)
{
  return MACHOPIC_INDIRECT ? 3 : 0;
}

/* We have to deal with ObjC/C++ metadata section placement in the common
   code, since it will also be called from LTO.

   Return metadata attributes, if present (searching for ABI=2 first)
   Return NULL_TREE if no such attributes are found.  */

static tree
is_objc_metadata (tree decl)
{
  if (DECL_P (decl)
      && (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == CONST_DECL)
      && DECL_ATTRIBUTES (decl))
    {
      tree meta = lookup_attribute ("OBJC2META", DECL_ATTRIBUTES (decl));
      if (meta)
	return meta;
      meta = lookup_attribute ("OBJC1META", DECL_ATTRIBUTES (decl));
      if (meta)
	return meta;
    }
  return NULL_TREE;
}

static int classes_seen;
static int objc_metadata_seen;

/* Return the section required for Objective C ABI 2 metadata.  */
static section *
darwin_objc2_section (tree decl ATTRIBUTE_UNUSED, tree meta, section * base)
{
  const char *p;
  tree ident = TREE_VALUE (meta);
  gcc_assert (TREE_CODE (ident) == IDENTIFIER_NODE);
  p = IDENTIFIER_POINTER (ident);

  gcc_checking_assert (flag_next_runtime >= 1 && flag_objc_abi == 2);

  objc_metadata_seen = 1;

  if (base == data_section)
    base = darwin_sections[objc2_metadata_section];

  /* Most of the OBJC2 META-data end up in the base section, so check it
     first.  */
  if      (startswith (p, "V2_BASE"))
    return base;
  else if (startswith (p, "V2_CNAM"))
    return darwin_sections[objc2_class_names_section];
  else if (startswith (p, "V2_MNAM"))
    return darwin_sections[objc2_method_names_section];
  else if (startswith (p, "V2_MTYP"))
    return darwin_sections[objc2_method_types_section];
  else if (startswith (p, "V2_STRG"))
    return darwin_sections[cstring_section];

  else if (startswith (p, "G2_META") || startswith (p, "G2_CLAS"))
    return darwin_sections[objc2_classdefs_section];
  else if (startswith (p, "V2_PCOL"))
    return ld_uses_coal_sects ? darwin_sections[data_coal_section]
			      : darwin_sections[objc2_data_section];
  else if (startswith (p, "V2_MREF"))
    return darwin_sections[objc2_message_refs_section];
  else if (startswith (p, "V2_CLRF"))
    return darwin_sections[objc2_classrefs_section];
  else if (startswith (p, "V2_SURF"))
    return darwin_sections[objc2_super_classrefs_section];
  else if (startswith (p, "V2_NLCL"))
    return darwin_sections[objc2_nonlazy_class_section];
  else if (startswith (p, "V2_CLAB"))
    {
      classes_seen = 1;
      return darwin_sections[objc2_classlist_section];
    }
  else if (startswith (p, "V2_SRFS"))
    return darwin_sections[objc2_selector_refs_section];
  else if (startswith (p, "V2_NLCA"))
    return darwin_sections[objc2_nonlazy_category_section];
  else if (startswith (p, "V2_CALA"))
    return darwin_sections[objc2_categorylist_section];

  else if (startswith (p, "V2_PLST"))
    return darwin_sections[objc2_protocollist_section];
  else if (startswith (p, "V2_PRFS"))
    return darwin_sections[objc2_protocolrefs_section];

  else if (startswith (p, "V2_INFO"))
    return darwin_sections[objc2_image_info_section];

  else if (startswith (p, "V2_EHTY"))
    return ld_uses_coal_sects ? darwin_sections[data_coal_section]
                              : data_section;

  else if (startswith (p, "V2_CSTR"))
    return darwin_sections[objc2_constant_string_object_section];

  else if (startswith (p, "V2_IVRF"))
    return darwin_sections[objc2_ivar_section];

  /* Not recognized, default.  */
  return base;
}

/* Return the section required for Objective C ABI 0/1 metadata.  */
static section *
darwin_objc1_section (tree decl ATTRIBUTE_UNUSED, tree meta, section * base)
{
  const char *p;
  tree ident = TREE_VALUE (meta);
  gcc_assert (TREE_CODE (ident) == IDENTIFIER_NODE);
  p = IDENTIFIER_POINTER (ident);

  gcc_checking_assert (flag_next_runtime >= 1 && flag_objc_abi < 2);

  objc_metadata_seen = 1;

  /* String sections first, cos there are lots of strings.  */
  if      (startswith (p, "V1_STRG"))
    return darwin_sections[cstring_section];
  else if (startswith (p, "V1_CLSN"))
    return darwin_sections[objc_class_names_section];
  else if (startswith (p, "V1_METN"))
    return darwin_sections[objc_meth_var_names_section];
  else if (startswith (p, "V1_METT"))
    return darwin_sections[objc_meth_var_types_section];

  else if (startswith (p, "V1_CLAS"))
    {
      classes_seen = 1;
      return darwin_sections[objc_class_section];
    }
  else if (startswith (p, "V1_META"))
    return darwin_sections[objc_meta_class_section];
  else if (startswith (p, "V1_CATG"))
    return darwin_sections[objc_category_section];
  else if (startswith (p, "V1_PROT"))
    return darwin_sections[objc_protocol_section];

  else if (startswith (p, "V1_CLCV"))
    return darwin_sections[objc_class_vars_section];
  else if (startswith (p, "V1_CLIV"))
    return darwin_sections[objc_instance_vars_section];

  else if (startswith (p, "V1_CLCM"))
    return darwin_sections[objc_cls_meth_section];
  else if (startswith (p, "V1_CLIM"))
    return darwin_sections[objc_inst_meth_section];
  else if (startswith (p, "V1_CACM"))
    return darwin_sections[objc_cat_cls_meth_section];
  else if (startswith (p, "V1_CAIM"))
    return darwin_sections[objc_cat_inst_meth_section];
  else if (startswith (p, "V1_PNSM"))
    return darwin_sections[objc_cat_inst_meth_section];
  else if (startswith (p, "V1_PCLM"))
    return darwin_sections[objc_cat_cls_meth_section];

  else if (startswith (p, "V1_CLPR"))
    return darwin_sections[objc_cat_cls_meth_section];
  else if (startswith (p, "V1_CAPR"))
    return darwin_sections[objc_category_section]; /* ??? CHECK me.  */

  else if (startswith (p, "V1_PRFS"))
    return darwin_sections[objc_cat_cls_meth_section];
  else if (startswith (p, "V1_CLRF"))
    return darwin_sections[objc_cls_refs_section];
  else if (startswith (p, "V1_SRFS"))
    return darwin_sections[objc_selector_refs_section];

  else if (startswith (p, "V1_MODU"))
    return darwin_sections[objc_module_info_section];
  else if (startswith (p, "V1_SYMT"))
    return darwin_sections[objc_symbols_section];
  else if (startswith (p, "V1_INFO"))
    return darwin_sections[objc_image_info_section];

  else if (startswith (p, "V1_PLST"))
    return darwin_sections[objc1_prop_list_section];
  else if (startswith (p, "V1_PEXT"))
    return darwin_sections[objc1_protocol_ext_section];
  else if (startswith (p, "V1_CEXT"))
    return darwin_sections[objc1_class_ext_section];

  else if (startswith (p, "V2_CSTR"))
    return darwin_sections[objc_constant_string_object_section];

  return base;
}

section *
machopic_select_section (tree decl,
			 int reloc,
			 unsigned HOST_WIDE_INT align)
{
  bool zsize, one, weak, use_coal, ro;
  section *base_section = NULL;

  weak = (DECL_P (decl)
	  && DECL_WEAK (decl)
	  && !lookup_attribute ("weak_import", DECL_ATTRIBUTES (decl)));

  /* Darwin pads zero-sized objects with at least one byte, so that the ld64
     atom model is preserved (objects must have distinct regions starting with
     a unique linker-visible symbol).
     In order to support section anchors, we need to move objects with zero
     size into sections which are marked as "no section anchors"; the padded
     objects, obviously, have real sizes that differ from their DECL sizes.  */
  zsize = DARWIN_SECTION_ANCHORS && flag_section_anchors;

  /* In the streaming of LTO symbol data, we might have a situation where the
     var is incomplete or layout not finished (DECL_SIZE_UNIT is NULL_TREE).
     We cannot tell if it is zero-sized then, but we can get the section
     category correct so that nm reports the right kind of section
     (e.g. BSS c.f. data).  */
  zsize = (zsize
	   && DECL_P (decl)
	   && (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == CONST_DECL)
	   && DECL_SIZE_UNIT (decl)
	   && tree_to_uhwi (DECL_SIZE_UNIT (decl)) == 0);

  one = DECL_P (decl)
	&& TREE_CODE (decl) == VAR_DECL
	&& DECL_COMDAT_GROUP (decl);

  use_coal = (weak || one) && ld_uses_coal_sects;

  ro = TREE_READONLY (decl) || TREE_CONSTANT (decl) ;

  switch (categorize_decl_for_section (decl, reloc))
    {
    case SECCAT_TEXT:
      gcc_unreachable ();
      break;

    case SECCAT_RODATA:
    case SECCAT_SRODATA:
      base_section = darwin_rodata_section (use_coal, zsize, reloc);
      break;

    case SECCAT_RODATA_MERGE_STR:
      base_section = darwin_mergeable_string_section (decl, align);
      break;

    case SECCAT_RODATA_MERGE_STR_INIT:
      base_section = darwin_mergeable_string_section (DECL_INITIAL (decl), align);
      break;

    case SECCAT_RODATA_MERGE_CONST:
      base_section =  darwin_mergeable_constant_section (decl, align, zsize);
      break;

    case SECCAT_DATA:
    case SECCAT_DATA_REL:
    case SECCAT_DATA_REL_LOCAL:
    case SECCAT_DATA_REL_RO:
    case SECCAT_DATA_REL_RO_LOCAL:
    case SECCAT_SDATA:
    case SECCAT_TDATA:
      if (use_coal)
	{
	  if (ro)
	    base_section = darwin_sections[const_data_coal_section];
	  else
	    base_section = darwin_sections[data_coal_section];
	}
      else if (zsize)
	{
	  /* If we're doing section anchors, then punt zero-sized objects into
	     their own sections so that they don't interfere with offset
	     computation for the remaining vars.  */
	  if (ro)
	    base_section = darwin_sections[zobj_const_data_section];
	  else
	    base_section = darwin_sections[zobj_data_section];
	}
      else if (ro)
	base_section = darwin_sections[const_data_section];
      else
	base_section = data_section;
      break;
    case SECCAT_BSS:
    case SECCAT_SBSS:
    case SECCAT_TBSS:
      if (use_coal)
	base_section = darwin_sections[data_coal_section];
      else
	{
	  if (!TREE_PUBLIC (decl))
	    base_section = lcomm_section;
	  else if (bss_noswitch_section)
	    base_section = bss_noswitch_section;
	  else
	    base_section = data_section;
	}
      break;

    default:
      gcc_unreachable ();
    }

  /* Darwin weird special cases.
     a) OBJC Meta-data. */
  if (DECL_P (decl)
      && (TREE_CODE (decl) == VAR_DECL
	  || TREE_CODE (decl) == CONST_DECL)
      && DECL_ATTRIBUTES (decl))
    {
      tree meta = lookup_attribute ("OBJC2META", DECL_ATTRIBUTES (decl));
      if (meta)
	return darwin_objc2_section (decl, meta, base_section);
      meta = lookup_attribute ("OBJC1META", DECL_ATTRIBUTES (decl));
      if (meta)
	return darwin_objc1_section (decl, meta, base_section);
      meta = lookup_attribute ("OBJC1METG", DECL_ATTRIBUTES (decl));
      if (meta)
	return base_section; /* GNU runtime is happy with it all in one pot.  */
    }

  /* b) Constant string objects.  */
  if (TREE_CODE (decl) == CONSTRUCTOR
      && TREE_TYPE (decl)
      && TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE
      && TYPE_NAME (TREE_TYPE (decl)))
    {
      tree name = TYPE_NAME (TREE_TYPE (decl));
      if (TREE_CODE (name) == TYPE_DECL)
        name = DECL_NAME (name);

      if (!strcmp (IDENTIFIER_POINTER (name), "__builtin_ObjCString"))
	{
	  if (flag_next_runtime)
	    {
	      if (flag_objc_abi == 2)
		return darwin_sections[objc2_constant_string_object_section];
	      else
		return darwin_sections[objc_constant_string_object_section];
	    }
	  else
	    return darwin_sections[objc_string_object_section];
	}
      else if (!strcmp (IDENTIFIER_POINTER (name), "__builtin_CFString"))
	return darwin_sections[cfstring_constant_object_section];
      else
	return base_section;
    }
  else if (flag_next_runtime
	   && VAR_P (decl)
	   && DECL_NAME (decl)
	   && TREE_CODE (DECL_NAME (decl)) == IDENTIFIER_NODE
	   && IDENTIFIER_POINTER (DECL_NAME (decl))
	   && startswith (IDENTIFIER_POINTER (DECL_NAME (decl)), "_OBJC_"))
    /* c) legacy meta-data selection was deprecated at 4.6, removed now.  */
    gcc_unreachable ();

  return base_section;
}

/* This can be called with address expressions as "rtx".
   They must go in "const".  */

section *
machopic_select_rtx_section (machine_mode mode, rtx x,
			     unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  if (known_eq (GET_MODE_SIZE (mode), 8)
      && (GET_CODE (x) == CONST_INT
	  || GET_CODE (x) == CONST_WIDE_INT
	  || GET_CODE (x) == CONST_DOUBLE))
    return darwin_sections[literal8_section];
  else if (known_eq (GET_MODE_SIZE (mode), 4)
	   && (GET_CODE (x) == CONST_INT
	       || GET_CODE (x) == CONST_WIDE_INT
	       || GET_CODE (x) == CONST_DOUBLE))
    return darwin_sections[literal4_section];
  else if (HAVE_GAS_LITERAL16
	   && TARGET_64BIT
	   && known_eq (GET_MODE_SIZE (mode), 16)
	   && (GET_CODE (x) == CONST_INT
	       || GET_CODE (x) == CONST_WIDE_INT
	       || GET_CODE (x) == CONST_DOUBLE
	       || GET_CODE (x) == CONST_VECTOR))
    return darwin_sections[literal16_section];
  else if (MACHOPIC_INDIRECT
	   && (GET_CODE (x) == SYMBOL_REF
	       || GET_CODE (x) == CONST
	       || GET_CODE (x) == LABEL_REF))
    return darwin_sections[const_data_section];
  else
    return darwin_sections[const_section];
}

void
machopic_asm_out_constructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  cdtor_record new_elt = {symbol, priority, vec_safe_length (ctors)};

  vec_safe_push (ctors, new_elt);

  if (! MACHOPIC_INDIRECT)
    fprintf (asm_out_file, ".reference .constructors_used\n");
}

void
machopic_asm_out_destructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  cdtor_record new_elt = {symbol, priority, vec_safe_length (dtors)};

  vec_safe_push (dtors, new_elt);

  if (! MACHOPIC_INDIRECT)
    fprintf (asm_out_file, ".reference .destructors_used\n");
}

static int
sort_cdtor_records (const void * a, const void * b)
{
  const cdtor_record *cda = (const cdtor_record *)a;
  const cdtor_record *cdb = (const cdtor_record *)b;
  if (cda->priority > cdb->priority)
    return 1;
  if (cda->priority < cdb->priority)
    return -1;
  if (cda->position > cdb->position)
    return 1;
  if (cda->position < cdb->position)
    return -1;
  return 0;
}

static void
finalize_ctors ()
{
  unsigned int i;
  cdtor_record *elt;

  if (MACHOPIC_INDIRECT)
    switch_to_section (darwin_sections[mod_init_section]);
  else
    switch_to_section (darwin_sections[constructor_section]);

  /* Where needed, provide a linker-visible section-start symbol so that we
     have stable output between debug and non-debug.  */
  if (ld_init_term_start_labels)
    fputs (MACHOPIC_INDIRECT ? "_Mod.init:\n" : "_CTOR.sect:\n", asm_out_file);

  if (vec_safe_length (ctors) > 1)
    ctors->qsort (sort_cdtor_records);
  FOR_EACH_VEC_SAFE_ELT (ctors, i, elt)
    {
      assemble_align (POINTER_SIZE);
      assemble_integer (elt->symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
    }
}

static void
finalize_dtors ()
{
  unsigned int i;
  cdtor_record *elt;

  if (MACHOPIC_INDIRECT)
    switch_to_section (darwin_sections[mod_term_section]);
  else
    switch_to_section (darwin_sections[destructor_section]);

  /* Where needed, provide a linker-visible section-start symbol so that we
     have stable output between debug and non-debug.  */
  if (ld_init_term_start_labels)
    fputs (MACHOPIC_INDIRECT ? "_Mod.term:\n" : "_DTOR.sect:\n", asm_out_file);

  if (vec_safe_length (dtors) > 1)
    dtors->qsort (sort_cdtor_records);
  FOR_EACH_VEC_SAFE_ELT (dtors, i, elt)
    {
      assemble_align (POINTER_SIZE);
      assemble_integer (elt->symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
    }
}

void
darwin_globalize_label (FILE *stream, const char *name)
{
  if (!startswith (name, "_OBJC_"))
    default_globalize_label (stream, name);
  /* We have some Objective C cases that need to be global, but only on newer
     OS versions.  */
  if (flag_objc_abi < 2 || flag_next_runtime < 100700)
    return;
  if (startswith (name+6, "LabelPro"))
    default_globalize_label (stream, name);
  if (startswith (name+6, "Protocol_"))
    default_globalize_label (stream, name);
}

/* This routine returns non-zero if 'name' starts with the special objective-c
   anonymous file-scope static name.  It accommodates c++'s mangling of such
   symbols (in this case the symbols will have form _ZL{d}*_OBJC_* d=digit).  */

int
darwin_label_is_anonymous_local_objc_name (const char *name)
{
  const unsigned char *p = (const unsigned char *) name;
  if (*p != '_')
    return 0;
  if (p[1] == 'Z' && p[2] == 'L')
  {
    p += 3;
    while (*p >= '0' && *p <= '9')
      p++;
  }
  if (!startswith ((const char *)p, "_OBJC_"))
    return false;

  /* We need some of the objective c meta-data symbols to be visible to the
     linker (when the target OS version is newer).  FIXME: this is horrible,
     we need a better mechanism.  */

  if (flag_objc_abi < 2 || flag_next_runtime < 100700)
    return true;

  p += 6;
  if (startswith ((const char *)p, "ClassRef"))
    return false;
  else if (startswith ((const char *)p, "SelRef"))
    return false;
  else if (startswith ((const char *)p, "Category"))
    {
      if (p[8] == '_' || p[8] == 'I' || p[8] == 'P' || p[8] == 'C' )
	return false;
      return true;
    }
  else if (startswith ((const char *)p, "ClassMethods"))
    return false;
  else if (startswith ((const char *)p, "Instance"))
    {
      if (p[8] == 'I' || p[8] == 'M')
	return false;
      return true;
    }
  else if (startswith ((const char *)p, "CLASS_RO"))
    return false;
  else if (startswith ((const char *)p, "METACLASS_RO"))
    return false;
  else if (startswith ((const char *)p, "Protocol"))
    {
      if (p[8] == '_' || p[8] == 'I' || p[8] == 'P'
	  || p[8] == 'M' || p[8] == 'C' || p[8] == 'O')
	return false;
      return true;
    }
  else if (startswith ((const char *)p, "LabelPro"))
    return false;
  return true;
}

/* LTO support for Mach-O.

   This version uses three mach-o sections to encapsulate the (unlimited
   number of) lto sections.

   __GNU_LTO, __lto_sections  contains the concatented GNU LTO section data.
   __GNU_LTO, __section_names contains the GNU LTO section names.
   __GNU_LTO, __section_index contains an array of values that index these.

   Indexed thus:
     <section offset from the start of __GNU_LTO, __lto_sections>,
     <section length>
     <name offset from the start of __GNU_LTO, __section_names,
     <name length>.

   At present, for both m32 and m64 mach-o files each of these fields is
   represented  by a uint32_t.  This is because, AFAICT, a mach-o object
   cannot exceed 4Gb because the section_64 offset field (see below) is 32bits.

    uint32_t offset;
   "offset  An integer specifying the offset to this section in the file."  */

/* Count lto section numbers.  */
static unsigned int lto_section_num = 0;

/* A vector of information about LTO sections, at present, we only have
   the name.  TODO: see if we can get the data length somehow.  */
typedef struct GTY (()) darwin_lto_section_e {
  const char *sectname;
} darwin_lto_section_e ;

static GTY (()) vec<darwin_lto_section_e, va_gc> *lto_section_names;

/* Section wrapper scheme (used here to wrap the unlimited number of LTO
   sections into three Mach-O ones).
   NOTE: These names MUST be kept in sync with those in
	 libiberty/simple-object-mach-o.  */
#define LTO_SECTS_SECTION "__wrapper_sects"
#define LTO_NAMES_SECTION "__wrapper_names"
#define LTO_INDEX_SECTION "__wrapper_index"

/* File to temporarily store LTO data.  This is appended to asm_out_file
   in darwin_end_file.  */
static FILE *lto_asm_out_file, *saved_asm_out_file;
static char *lto_asm_out_name;
static enum debug_info_levels saved_debug_info_level;

/* Prepare asm_out_file for LTO output.  For darwin, this means hiding
   asm_out_file and switching to an alternative output file.  */
void
darwin_asm_lto_start (void)
{
  gcc_assert (! saved_asm_out_file);
  saved_asm_out_file = asm_out_file;
  saved_debug_info_level = debug_info_level;
  debug_info_level = DINFO_LEVEL_NONE;
  if (! lto_asm_out_name)
    lto_asm_out_name = make_temp_file (".lto.s");
  lto_asm_out_file = fopen (lto_asm_out_name, "a");
  if (lto_asm_out_file == NULL)
    fatal_error (input_location,
		 "failed to open temporary file %s for LTO output",
		 lto_asm_out_name);
  asm_out_file = lto_asm_out_file;
}

/* Restore asm_out_file.  */
void
darwin_asm_lto_end (void)
{
  gcc_assert (saved_asm_out_file);
  fclose (lto_asm_out_file);
  asm_out_file = saved_asm_out_file;
  saved_asm_out_file = NULL;
  debug_info_level = saved_debug_info_level;
}

static void
darwin_asm_dwarf_section (const char *name, unsigned int flags,
			  tree decl, bool is_for_lto);

/*  Called for the TARGET_ASM_NAMED_SECTION hook.  */

void
darwin_asm_named_section (const char *name,
			  unsigned int flags,
			  tree decl ATTRIBUTE_UNUSED)
{
  /* LTO sections go in a special section that encapsulates the (unlimited)
     number of GNU LTO sections within a single mach-o one.  */
  if (startswith (name, LTO_SECTION_NAME_PREFIX))
    {
      darwin_lto_section_e e;
      /* We expect certain flags to be set...  */
      gcc_assert ((flags & (SECTION_DEBUG | SECTION_NAMED))
		  == (SECTION_DEBUG | SECTION_NAMED));

      /* Switch to our combined section.  */
      fprintf (asm_out_file, "\t.section %s,%s,regular,debug\n",
	       LTO_SEGMENT_NAME, LTO_SECTS_SECTION);
      /* Output a label for the start of this sub-section.  */
      fprintf (asm_out_file, "L_GNU_LTO%d:\t;# %s\n",
	       lto_section_num, name);
      /* We have to jump through hoops to get the values of the intra-section
         offsets... */
      fprintf (asm_out_file, "\t.set L$gnu$lto$offs%d,L_GNU_LTO%d-L_GNU_LTO0\n",
	       lto_section_num, lto_section_num);
      fprintf (asm_out_file,
	       "\t.set L$gnu$lto$size%d,L_GNU_LTO%d-L_GNU_LTO%d\n",
	       lto_section_num, lto_section_num+1, lto_section_num);
      lto_section_num++;
      e.sectname = xstrdup (name);
      /* Keep the names, we'll need to make a table later.
         TODO: check that we do not revisit sections, that would break
         the assumption of how this is done.  */
      if (lto_section_names == NULL)
        vec_alloc (lto_section_names, 16);
      vec_safe_push (lto_section_names, e);
   }
  else if (startswith (name, "__DWARF,"))
    darwin_asm_dwarf_section (name, flags, decl, false);
  else if (startswith (name, "__GNU_DWARF_LTO,"))
    darwin_asm_dwarf_section (name, flags, decl, true);
  else
    fprintf (asm_out_file, "\t.section %s\n", name);
}

void
darwin_unique_section (tree decl ATTRIBUTE_UNUSED, int reloc ATTRIBUTE_UNUSED)
{
  /* Darwin does not use unique sections.  */
}

/* Handle __attribute__ ((apple_kext_compatibility)).
   This only applies to darwin kexts for 2.95 compatibility -- it shrinks the
   vtable for classes with this attribute (and their descendants) by not
   outputting the new 3.0 nondeleting destructor.  This means that such
   objects CANNOT be allocated on the stack or as globals UNLESS they have
   a completely empty `operator delete'.
   Luckily, this fits in with the Darwin kext model.

   This attribute also disables gcc3's potential overlaying of derived
   class data members on the padding at the end of the base class.  */

tree
darwin_handle_kext_attribute (tree *node, tree name,
			      tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED,
			      bool *no_add_attrs)
{
  /* APPLE KEXT stuff -- only applies with pure static C++ code.  */
  if (! TARGET_KEXTABI)
    {
      warning (0, "%qE 2.95 vtable-compatibility attribute applies "
	       "only when compiling a kext", name);

      *no_add_attrs = true;
    }
  else if (TREE_CODE (*node) != RECORD_TYPE)
    {
      warning (0, "%qE 2.95 vtable-compatibility attribute applies "
	       "only to C++ classes", name);

      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "weak_import" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
darwin_handle_weak_import_attribute (tree *node, tree name,
				     tree ARG_UNUSED (args),
				     int ARG_UNUSED (flags),
				     bool * no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL && TREE_CODE (*node) != VAR_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored",
	       name);
      *no_add_attrs = true;
    }
  else
    declare_weak (*node);

  return NULL_TREE;
}

/* Emit a label for an FDE, making it global and/or weak if appropriate.
   The third parameter is nonzero if this is for exception handling.
   The fourth parameter is nonzero if this is just a placeholder for an
   FDE that we are omitting. */

void
darwin_emit_unwind_label (FILE *file, tree decl, int for_eh, int empty)
{
  char *lab ;
  char buf[32];
  static int invok_count = 0;
  static tree last_fun_decl = NULL_TREE;

  /* Modern linkers can produce distinct FDEs without compiler support.  */
  if (! for_eh || ! ld_needs_eh_markers)
    return;

  /* FIXME: This only works when the eh for all sections of a function are
     emitted at the same time.  If that changes, we would need to use a lookup
     table of some form to determine what to do.  Also, we should emit the
     unadorned label for the partition containing the public label for a
     function.  This is of limited use, probably, since we do not currently
     enable partitioning.  */
  strcpy (buf, ".eh");
  if (decl && TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (decl == last_fun_decl)
        {
	  invok_count++;
	  snprintf (buf, 31, "$$part$$%d.eh", invok_count);
	}
      else
	{
	  last_fun_decl = decl;
	  invok_count = 0;
	}
    }

  lab = concat (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)), buf, NULL);

  if (TREE_PUBLIC (decl))
    {
      targetm.asm_out.globalize_label (file, lab);
      if (DECL_VISIBILITY (decl) == VISIBILITY_HIDDEN)
	{
	  fputs ("\t.private_extern ", file);
	  assemble_name (file, lab);
	  fputc ('\n', file);
	}
    }

  if (DECL_WEAK (decl))
    {
      fputs ("\t.weak_definition ", file);
      assemble_name (file, lab);
      fputc ('\n', file);
    }

  assemble_name (file, lab);
  if (empty)
    {
      fputs (" = 0\n", file);

      /* Mark the absolute .eh and .eh1 style labels as needed to
	 ensure that we don't dead code strip them and keep such
	 labels from another instantiation point until we can fix this
	 properly with group comdat support.  */
      darwin_mark_decl_preserved (lab);
    }
  else
    fputs (":\n", file);

  free (lab);
}

static GTY(()) unsigned long except_table_label_num;

void
darwin_emit_except_table_label (FILE *file)
{
  char section_start_label[30];

  ASM_GENERATE_INTERNAL_LABEL (section_start_label, "GCC_except_table",
			       except_table_label_num++);
  ASM_OUTPUT_LABEL (file, section_start_label);
}

rtx
darwin_make_eh_symbol_indirect (rtx orig, bool ARG_UNUSED (pubvis))
{
  if (DARWIN_PPC == 0 && TARGET_64BIT)
    return orig;

  return gen_rtx_SYMBOL_REF (Pmode,
			     machopic_indirection_name (orig,
							/*stub_p=*/false));
}

/* The unwinders in earlier Darwin versions are based on an old version
   of libgcc_s and need current frame address stateto be reset after a
   DW_CFA_restore_state recovers the register values.  */

bool
darwin_should_restore_cfa_state (void)
{
  return generating_for_darwin_version <= 10;
}

/* Return, and mark as used, the name of the stub for the mcount function.
   Currently, this is only called by X86 code in the expansion of the
   FUNCTION_PROFILER macro, when stubs are enabled.  */

const char*
machopic_mcount_stub_name (void)
{
  rtx symbol = gen_rtx_SYMBOL_REF (Pmode, "*mcount");
  const char *name = machopic_indirection_name (symbol, /*stub_p=*/true);
  machopic_validate_stub_or_non_lazy_ptr (name);
  return name;
}

/* Generate a PC-relative reference to a Mach-O non-lazy-symbol.  */

void
darwin_non_lazy_pcrel (FILE *file, rtx addr)
{
  const char *nlp_name;

  gcc_assert (GET_CODE (addr) == SYMBOL_REF);

  nlp_name = machopic_indirection_name (addr, /*stub_p=*/false);
  fputs ("\t.long\t", file);
  ASM_OUTPUT_LABELREF (file, nlp_name);
  fputs ("-.", file);
}

/* If this is uncommented, details of each allocation will be printed
   in the asm right before the actual code.  WARNING - this will cause some
   test-suite fails (since the printout will contain items that some tests
   are not expecting) -- so don't leave it on by default (it bloats the
   asm too).  */
/*#define DEBUG_DARWIN_MEM_ALLOCATORS*/

/* The first two of these routines are ostensibly just intended to put
   names into the asm.  However, they are both hijacked in order to ensure
   that zero-sized items do not make their way into the output.  Consequently,
   we also need to make these participate in provisions for dealing with
   such items in section anchors.  */

/* The implementation of ASM_DECLARE_OBJECT_NAME.  */
/* The RTTI data (e.g., __ti4name) is common and public (and static),
   but it does need to be referenced via indirect PIC data pointers.
   The machopic_define_symbol calls are telling the machopic subsystem
   that the name *is* defined in this module, so it doesn't need to
   make them indirect.  */
void
darwin_asm_declare_object_name (FILE *file,
				const char *nam, tree decl)
{
  const char *xname = nam;
  unsigned HOST_WIDE_INT size;
  bool local_def, weak;

  weak = (DECL_P (decl)
	  && DECL_WEAK (decl)
	  && !lookup_attribute ("weak_import",
				 DECL_ATTRIBUTES (decl)));

  local_def = DECL_INITIAL (decl) || (TREE_STATIC (decl)
				      && (!DECL_COMMON (decl)
					  || !TREE_PUBLIC (decl)));

  if (GET_CODE (XEXP (DECL_RTL (decl), 0)) != SYMBOL_REF)
    xname = IDENTIFIER_POINTER (DECL_NAME (decl));

  if (local_def)
    {
      (* targetm.encode_section_info) (decl, DECL_RTL (decl), false);
      if (!weak)
	machopic_define_symbol (DECL_RTL (decl));
    }

  size = tree_to_uhwi (DECL_SIZE_UNIT (decl));

#ifdef DEBUG_DARWIN_MEM_ALLOCATORS
fprintf (file, "# dadon: %s %s (%llu, %u) local %d weak %d"
	       " stat %d com %d pub %d t-const %d t-ro %d init %lx\n",
	xname, (TREE_CODE (decl) == VAR_DECL?"var":"const"),
	(unsigned long long)size, DECL_ALIGN (decl), local_def,
	DECL_WEAK (decl), TREE_STATIC (decl), DECL_COMMON (decl),
	TREE_PUBLIC (decl), TREE_CONSTANT (decl), TREE_READONLY (decl),
	(unsigned long)DECL_INITIAL (decl));
#endif

  /* Darwin needs help to support local zero-sized objects.
     They must be made at least one byte, and the section containing must be
     marked as unsuitable for section-anchors (see storage allocators below).

     For non-zero objects this output is handled by varasm.c.
  */
  if (!size)
    {
      unsigned int l2align = 0;

      /* The align must be honored, even for zero-sized.  */
      if (DECL_ALIGN (decl))
	{
	  l2align = floor_log2 (DECL_ALIGN (decl) / BITS_PER_UNIT);
	  fprintf (file, "\t.align\t%u\n", l2align);
	}

      ASM_OUTPUT_LABEL (file, xname);
      size = 1;
      fprintf (file, "\t.space\t" HOST_WIDE_INT_PRINT_UNSIGNED"\n", size);

      /* Check that we've correctly picked up the zero-sized item and placed it
         properly.  */
      gcc_assert ((!DARWIN_SECTION_ANCHORS || !flag_section_anchors)
		  || (in_section
		      && (in_section->common.flags & SECTION_NO_ANCHOR)));
    }
  else
    ASM_OUTPUT_LABEL (file, xname);
}

/* The implementation of ASM_DECLARE_CONSTANT_NAME.  */
void
darwin_asm_declare_constant_name (FILE *file, const char *name,
				  const_tree exp ATTRIBUTE_UNUSED,
				  HOST_WIDE_INT size)
{
  assemble_label (file, name);
  /* As for other items, we need at least one byte.  */
  if (!size)
    {
      fputs ("\t.space\t1\n", file);
      /* Check that we've correctly picked up the zero-sized item and placed it
         properly.  */
      gcc_assert ((!DARWIN_SECTION_ANCHORS || !flag_section_anchors)
		  || (in_section
		      && (in_section->common.flags & SECTION_NO_ANCHOR)));
    }
}

/* Darwin storage allocators.

   Zerofill sections are desirable for large blank data since, otherwise, these
   data bloat objects (PR33210).

   However, section anchors don't work in .zerofill sections (one cannot switch
   to a zerofill section).  Ergo, for Darwin targets using section anchors we need
   to put (at least some) data into 'normal' switchable sections.

   Here we set a relatively arbitrary value for the size of an object to trigger
   zerofill when section anchors are enabled (anything bigger than a page for
   current Darwin implementations).  FIXME: there ought to be some objective way
   to make this choice.

   When section anchor are off this is ignored anyway.  */

#define BYTES_ZFILL 4096

/* Emit a chunk of data for items coalesced by the linker.  */
static void
darwin_emit_weak_or_comdat (FILE *fp, tree decl, const char *name,
				  unsigned HOST_WIDE_INT size,
				  bool use_coal,
				  unsigned int align)
{
  /* Since the sections used here are coalesced, they will not be eligible
     for section anchors, and therefore we don't need to break that out.
     CHECKME: for modern linker on PowerPC.  */
 if (TREE_READONLY (decl) || TREE_CONSTANT (decl))
    switch_to_section (use_coal ? darwin_sections[const_data_coal_section]
				: darwin_sections[const_data_section]);
  else
    switch_to_section (use_coal ? darwin_sections[data_coal_section]
				: data_section);

  /* To be consistent, we'll allow darwin_asm_declare_object_name to assemble
     the align info for zero-sized items... but do it here otherwise.  */
  if (size && align)
    fprintf (fp, "\t.align\t%d\n", floor_log2 (align / BITS_PER_UNIT));

  if (TREE_PUBLIC (decl))
    darwin_globalize_label (fp, name);

  /* ... and we let it deal with outputting one byte of zero for them too.  */
  darwin_asm_declare_object_name (fp, name, decl);
  if (size)
    assemble_zeros (size);
}

/* Emit a chunk of data for ObjC meta-data that got placed in BSS erroneously.  */
static void
darwin_emit_objc_zeroed (FILE *fp, tree decl, const char *name,
				  unsigned HOST_WIDE_INT size,
				  unsigned int align, tree meta)
{
  section *ocs = data_section;

  if (TREE_PURPOSE (meta) == get_identifier("OBJC2META"))
    ocs = darwin_objc2_section (decl, meta, ocs);
  else
    ocs = darwin_objc1_section (decl, meta, ocs);

  switch_to_section (ocs);

  /* We shall declare that zero-sized meta-data are not valid (yet).  */
  gcc_assert (size);
  fprintf (fp, "\t.align\t%d\n", floor_log2 (align / BITS_PER_UNIT));

  /* ... and we let it deal with outputting one byte of zero for them too.  */
  darwin_asm_declare_object_name (fp, name, decl);
  assemble_zeros (size);
}

/* This routine emits 'local' storage:

   When Section Anchors are off this routine emits .zerofill commands in
   sections named for their alignment.

   When Section Anchors are on, smaller (non-zero-sized) items are placed in
   the .static_data section so that the section anchoring system can see them.
   Larger items are still placed in .zerofill sections, addressing PR33210.
   The routine has no checking - it is all assumed to be done by the caller.
*/
static void
darwin_emit_local_bss (FILE *fp, tree decl, const char *name,
			unsigned HOST_WIDE_INT size,
			unsigned int l2align)
{
   if (DARWIN_SECTION_ANCHORS && flag_section_anchors && size < BYTES_ZFILL)
    {
      /* Put smaller objects in _static_data, where the section anchors system
	 can get them.
	 However, if they are zero-sized punt them to yet a different section
	 (that is not allowed to participate in anchoring).  */
      if (!size)
	{
	  fputs ("\t.section\t__DATA,__zobj_bss\n", fp);
	  in_section = darwin_sections[zobj_bss_section];
	  size = 1;
	}
      else
	{
	  fputs ("\t.static_data\n", fp);
	  in_section = darwin_sections[static_data_section];
	}

      if (l2align)
	fprintf (fp, "\t.align\t%u\n", l2align);

      assemble_name (fp, name);
      fprintf (fp, ":\n\t.space\t" HOST_WIDE_INT_PRINT_UNSIGNED"\n", size);
    }
  else
    {
      /* When we are on a non-section anchor target (or not using section
	 anchors, we can get zero-sized items here.  However, all we need to
	 do is to bump them to one byte and the section alignment will take
	 care of the rest.  */
      char secnam[64];
      snprintf (secnam, 64, "__DATA,__bss");
      unsigned int flags = SECTION_BSS|SECTION_WRITE|SECTION_NO_ANCHOR;
      in_section = get_section (secnam, flags, NULL);
      fprintf (fp, "\t.zerofill %s,", secnam);
      assemble_name (fp, name);
      if (!size)
	size = 1;

      if (l2align)
	fprintf (fp, "," HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",
		 size, (unsigned) l2align);
      else
	fprintf (fp, "," HOST_WIDE_INT_PRINT_UNSIGNED",0\n", size);
    }

  (*targetm.encode_section_info) (decl, DECL_RTL (decl), false);
  /* This is defined as a file-scope var, so we know to notify machopic.  */
  machopic_define_symbol (DECL_RTL (decl));
}

/* Emit a chunk of common.  */
static void
darwin_emit_common (FILE *fp, const char *name,
		    unsigned HOST_WIDE_INT size, unsigned int align)
{
  unsigned HOST_WIDE_INT rounded;
  unsigned int l2align;

  /* Earlier systems complain if the alignment exceeds the page size.
     The magic number is 4096 * 8 - hard-coded for legacy systems.  */
  if (!emit_aligned_common && (align > 32768UL))
    align = 4096UL; /* In units.  */
  else
    align /= BITS_PER_UNIT;

  /* Make sure we have a meaningful align.  */
  if (!align)
    align = 1;

  /* For earlier toolchains, we need to emit the var as a rounded size to
     tell ld the alignment.  */
  if (size < align)
    rounded = align;
  else
    rounded = (size + (align-1)) & ~(align-1);

  l2align = floor_log2 (align);
  gcc_assert (l2align <= L2_MAX_OFILE_ALIGNMENT);

  in_section = comm_section;
  /* We mustn't allow multiple public symbols to share an address when using
     the normal OSX toolchain.  */
  if (!size)
    {
      /* Put at least one byte.  */
      size = 1;
      /* This section can no longer participate in section anchoring.  */
      comm_section->common.flags |= SECTION_NO_ANCHOR;
    }

  fputs ("\t.comm\t", fp);
  assemble_name (fp, name);
  fprintf (fp, "," HOST_WIDE_INT_PRINT_UNSIGNED,
	   emit_aligned_common?size:rounded);
  if (l2align && emit_aligned_common)
    fprintf (fp, ",%u", l2align);
  fputs ("\n", fp);
}

/* Output a var which is all zero - into aligned BSS sections, common, lcomm
   or coalescable data sections (for weak or comdat) as appropriate.  */

void
darwin_output_aligned_bss (FILE *fp, tree decl, const char *name,
			  unsigned HOST_WIDE_INT size, unsigned int align)
{
  unsigned int l2align;
  bool one, pub, weak;
  tree meta;

  pub = TREE_PUBLIC (decl);
  one = DECL_ONE_ONLY (decl);
  weak = (DECL_P (decl)
	  && DECL_WEAK (decl)
	  && !lookup_attribute ("weak_import",
				 DECL_ATTRIBUTES (decl)));

#ifdef DEBUG_DARWIN_MEM_ALLOCATORS
fprintf (fp, "# albss: %s (%lld,%d) ro %d cst %d stat %d com %d"
	     " pub %d weak %d one %d init %lx\n",
	name, (long long)size, (int)align, TREE_READONLY (decl),
	TREE_CONSTANT (decl), TREE_STATIC (decl), DECL_COMMON (decl),
	pub, weak, one, (unsigned long)DECL_INITIAL (decl));
#endif

  /* ObjC metadata can get put in BSS because varasm.c decides it's BSS
     before the target has a chance to comment.  */
  if ((meta = is_objc_metadata (decl)))
    {
      darwin_emit_objc_zeroed (fp, decl, name, size, DECL_ALIGN (decl), meta);
      return;
    }

  /* Check that any initializer is valid.  */
  gcc_assert ((DECL_INITIAL (decl) == NULL)
	       || (DECL_INITIAL (decl) == error_mark_node)
	       || initializer_zerop (DECL_INITIAL (decl)));

  gcc_assert (DECL_SECTION_NAME (decl) == NULL);
  gcc_assert (!DECL_COMMON (decl));

  /*  Pick up the correct alignment.  */
  if (!size || !align)
    align = DECL_ALIGN (decl);

  l2align = floor_log2 (align / BITS_PER_UNIT);
  gcc_assert (l2align <= L2_MAX_OFILE_ALIGNMENT);

  last_assemble_variable_decl = decl;

  /* We would rather not have to check this here - but it seems that we might
     be passed a decl that should be in coalesced space.  */
  if (one || weak)
    {
      /* Weak or COMDAT objects are put in mergeable sections.  */
      darwin_emit_weak_or_comdat (fp, decl, name, size,
				  ld_uses_coal_sects, DECL_ALIGN (decl));
      return;
    }

  /* If this is not public, then emit according to local rules.  */
  if (!pub)
    {
      darwin_emit_local_bss (fp, decl, name, size, l2align);
      return;
    }

  /* So we have a public symbol.  */
  if (DARWIN_SECTION_ANCHORS && flag_section_anchors && size < BYTES_ZFILL)
    {
      /* Put smaller objects in data, where the section anchors system can get
	 them.  However, if they are zero-sized punt them to yet a different
	 section (that is not allowed to participate in anchoring).  */
      if (!size)
	{
	  fputs ("\t.section\t__DATA,__zobj_data\n", fp);
	  in_section = darwin_sections[zobj_data_section];
	  size = 1;
	}
      else
	{
	  fputs ("\t.data\n", fp);
	  in_section = data_section;
	}

      if (l2align)
	fprintf (fp, "\t.align\t%u\n", l2align);

      assemble_name (fp, name);
      fprintf (fp, ":\n\t.space\t" HOST_WIDE_INT_PRINT_UNSIGNED"\n", size);
    }
  else
    {
      /* Section anchors not in use.  */
      unsigned int flags = SECTION_BSS|SECTION_WRITE|SECTION_NO_ANCHOR;
      char secnam[64];
      snprintf (secnam, 64, "__DATA,__common");
      in_section = get_section (secnam, flags, NULL);
      fprintf (fp, "\t.zerofill %s,", secnam);
      assemble_name (fp, name);
      if (!size)
	size = 1;

      if (l2align)
	fprintf (fp, "," HOST_WIDE_INT_PRINT_UNSIGNED",%u\n", size, l2align);
      else
	fprintf (fp, "," HOST_WIDE_INT_PRINT_UNSIGNED",0\n", size);
    }
  (* targetm.encode_section_info) (decl, DECL_RTL (decl), false);
}

/* Output a chunk of common, with alignment specified (where the target
   supports this).  */
void
darwin_asm_output_aligned_decl_common (FILE *fp, tree decl, const char *name,
				       unsigned HOST_WIDE_INT size,
				       unsigned int align)
{
  unsigned int l2align;
  bool one, weak;
  tree meta;

  /* No corresponding var.  */
  if (decl==NULL)
    {
#ifdef DEBUG_DARWIN_MEM_ALLOCATORS
fprintf (fp, "# adcom: %s (%d,%d) decl=0x0\n", name, (int)size, (int)align);
#endif
      darwin_emit_common (fp, name, size, align);
      return;
    }

  one = DECL_ONE_ONLY (decl);
  weak = (DECL_P (decl)
	  && DECL_WEAK (decl)
	  && !lookup_attribute ("weak_import",
				 DECL_ATTRIBUTES (decl)));

#ifdef DEBUG_DARWIN_MEM_ALLOCATORS
fprintf (fp, "# adcom: %s (%lld,%d) ro %d cst %d stat %d com %d pub %d"
	     " weak %d one %d init %lx\n",
	name,  (long long)size, (int)align, TREE_READONLY (decl),
	TREE_CONSTANT (decl), TREE_STATIC (decl), DECL_COMMON (decl),
	TREE_PUBLIC (decl), weak, one, (unsigned long)DECL_INITIAL (decl));
#endif

  /* ObjC metadata can get put in BSS because varasm.c decides it's BSS
     before the target has a chance to comment.  */
  if ((meta = is_objc_metadata (decl)))
    {
      darwin_emit_objc_zeroed (fp, decl, name, size, DECL_ALIGN (decl), meta);
      return;
    }

  /* We shouldn't be messing with this if the decl has a section name.  */
  gcc_assert (DECL_SECTION_NAME (decl) == NULL);

  /* We would rather not have to check this here - but it seems that we might
     be passed a decl that should be in coalesced space.  */
  if (one || weak)
    {
      /* Weak or COMDAT objects are put in mergable sections.  */
      darwin_emit_weak_or_comdat (fp, decl, name, size,
				  ld_uses_coal_sects, DECL_ALIGN (decl));
      return;
    }

  /* We should only get here for DECL_COMMON, with a zero init (and, in
     principle, only for public symbols too - although we deal with local
     ones below).  */

  /* Check the initializer is OK.  */
  gcc_assert (DECL_COMMON (decl)
	      && ((DECL_INITIAL (decl) == NULL)
	       || (DECL_INITIAL (decl) == error_mark_node)
	       || initializer_zerop (DECL_INITIAL (decl))));

  last_assemble_variable_decl = decl;

  if (!size || !align)
    align = DECL_ALIGN (decl);

  l2align = floor_log2 (align / BITS_PER_UNIT);
  /* Check we aren't asking for more aligment than the platform allows.  */
  gcc_assert (l2align <= L2_MAX_OFILE_ALIGNMENT);

  if (TREE_PUBLIC (decl) != 0)
    darwin_emit_common (fp, name, size, align);
  else
    darwin_emit_local_bss (fp, decl, name, size, l2align);
}

/* Output a chunk of BSS with alignment specfied.  */
void
darwin_asm_output_aligned_decl_local (FILE *fp, tree decl, const char *name,
				      unsigned HOST_WIDE_INT size,
				      unsigned int align)
{
  unsigned long l2align;
  bool one, weak;
  tree meta;

  one = DECL_ONE_ONLY (decl);
  weak = (DECL_P (decl)
	  && DECL_WEAK (decl)
	  && !lookup_attribute ("weak_import",
				 DECL_ATTRIBUTES (decl)));

#ifdef DEBUG_DARWIN_MEM_ALLOCATORS
fprintf (fp, "# adloc: %s (%lld,%d) ro %d cst %d stat %d one %d pub %d"
	     " weak %d init %lx\n",
	name, (long long)size, (int)align, TREE_READONLY (decl),
	TREE_CONSTANT (decl), TREE_STATIC (decl), one, TREE_PUBLIC (decl),
	weak , (unsigned long)DECL_INITIAL (decl));
#endif

  /* ObjC metadata can get put in BSS because varasm.c decides it's BSS
     before the target has a chance to comment.  */
  if ((meta = is_objc_metadata (decl)))
    {
      darwin_emit_objc_zeroed (fp, decl, name, size, DECL_ALIGN (decl), meta);
      return;
    }

  /* We shouldn't be messing with this if the decl has a section name.  */
  gcc_assert (DECL_SECTION_NAME (decl) == NULL);

  /* We would rather not have to check this here - but it seems that we might
     be passed a decl that should be in coalesced space.  */
  if (one || weak)
    {
      /* Weak or COMDAT objects are put in mergable sections.  */
      darwin_emit_weak_or_comdat (fp, decl, name, size,
				  ld_uses_coal_sects, DECL_ALIGN (decl));
      return;
    }

  /* .. and it should be suitable for placement in local mem.  */
  gcc_assert(!TREE_PUBLIC (decl) && !DECL_COMMON (decl));
  /* .. and any initializer must be all-zero.  */
  gcc_assert ((DECL_INITIAL (decl) == NULL)
	       || (DECL_INITIAL (decl) == error_mark_node)
	       || initializer_zerop (DECL_INITIAL (decl)));

  last_assemble_variable_decl = decl;

  if (!size || !align)
    align = DECL_ALIGN (decl);

  l2align = floor_log2 (align / BITS_PER_UNIT);
  gcc_assert (l2align <= L2_MAX_OFILE_ALIGNMENT);

  darwin_emit_local_bss (fp, decl, name, size, l2align);
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
  else if (vis == VISIBILITY_HIDDEN || vis == VISIBILITY_INTERNAL)
    {
      fputs ("\t.private_extern ", asm_out_file);
      assemble_name (asm_out_file,
		     (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl))));
      fputs ("\n", asm_out_file);
    }
  else
    warning (OPT_Wattributes, "protected visibility attribute "
	     "not supported in this configuration; ignored");
}

/* vec used by darwin_asm_dwarf_section.
   Maybe a hash tab would be better here - but the intention is that this is
   a very short list (fewer than 16 items) and each entry should (ideally,
   eventually) only be presented once.

   A structure to hold a dwarf debug section used entry.  */

typedef struct GTY(()) dwarf_sect_used_entry {
  const char *name;
  unsigned count;
}
dwarf_sect_used_entry;


/* A list of used __DWARF sections.  */
static GTY (()) vec<dwarf_sect_used_entry, va_gc> *dwarf_sect_names_table;

/* This is called when we are asked to assemble a named section and the
   name begins with __DWARF,.  We keep a list of the section names (without
   the __DWARF, prefix) and use this to emit our required start label on the
   first switch to each section.  */

static void
darwin_asm_dwarf_section (const char *name, unsigned int flags,
			  tree ARG_UNUSED (decl), bool is_for_lto)
{
  unsigned i;
  int namelen, extra = 0;
  const char *sect, *lto_add = "";
  char sname[64];
  dwarf_sect_used_entry *ref;
  bool found = false;

  gcc_checking_assert ((flags & (SECTION_DEBUG | SECTION_NAMED))
			== (SECTION_DEBUG | SECTION_NAMED));

  /* We know that the name starts with __DWARF, or __GNU_DAWRF_LTO  */
  sect = strchr (name, ',') + 1;
  namelen = strchr (sect, ',') - sect;
  gcc_checking_assert (namelen);

  /* The section switch is output as written...  */
  fprintf (asm_out_file, "\t.section %s\n", name);

  /* ... but the string we keep to make section start labels needs
     adjustment for lto cases.  */
  if (is_for_lto)
    {
      lto_add = "_lto";
      extra = 4;
    }

  snprintf (sname, 64, "%.*s%.*s", namelen, sect, extra, lto_add);
  namelen += extra;

  if (dwarf_sect_names_table == NULL)
    vec_alloc (dwarf_sect_names_table, 16);
  else
    for (i = 0;
	 dwarf_sect_names_table->iterate (i, &ref);
	 i++)
      {
	if (!ref)
	  break;
	if (!strcmp (ref->name, sname))
	  {
	    found = true;
	    ref->count++;
	    break;
	  }
      }

  if (!found)
    {
      dwarf_sect_used_entry e;
      fprintf (asm_out_file, "Lsection%.*s:\n", namelen, sname);
      e.count = 1;
      e.name = xstrdup (sname);
      vec_safe_push (dwarf_sect_names_table, e);
    }
}

/* Output a difference of two labels that will be an assembly time
   constant if the two labels are local.  (.long lab1-lab2 will be
   very different if lab1 is at the boundary between two sections; it
   will be relocated according to the second section, not the first,
   so one ends up with a difference between labels in different
   sections, which is bad in the dwarf2 eh context for instance.)  */

static int darwin_dwarf_label_counter;

void
darwin_asm_output_dwarf_delta (FILE *file, int size,
			       const char *lab1, const char *lab2,
			       HOST_WIDE_INT offset)
{
  int islocaldiff = (lab1[0] == '*' && lab1[1] == 'L'
		     && lab2[0] == '*' && lab2[1] == 'L');
  const char *directive = (size == 8 ? ".quad" : ".long");

  if (islocaldiff)
    fprintf (file, "\t.set L$set$%d,", darwin_dwarf_label_counter);
  else
    fprintf (file, "\t%s\t", directive);

  assemble_name_raw (file, lab1);
  fprintf (file, "-");
  assemble_name_raw (file, lab2);
  if (offset != 0)
    fprintf (file, "+" HOST_WIDE_INT_PRINT_DEC, offset);
  if (islocaldiff)
    fprintf (file, "\n\t%s L$set$%d", directive, darwin_dwarf_label_counter++);
}

/* Output an offset in a DWARF section on Darwin.  On Darwin, DWARF section
   offsets are not represented using relocs in .o files; either the
   section never leaves the .o file, or the linker or other tool is
   responsible for parsing the DWARF and updating the offsets.  */

void
darwin_asm_output_dwarf_offset (FILE *file, int size, const char * lab,
				HOST_WIDE_INT offset, section *base)
{
  char sname[64];
  int namelen, extra = 0;
  bool is_for_lto;
  const char *lto_add = "";

  gcc_checking_assert (base->common.flags & SECTION_NAMED);
  is_for_lto = startswith (base->named.name, "__GNU_DWARF_LTO,");
  gcc_checking_assert (is_for_lto
		       || startswith (base->named.name, "__DWARF,"));
  const char *name = strchr (base->named.name, ',') + 1;
  gcc_checking_assert (name);

  namelen = strchr (name, ',') - (name);
  if (is_for_lto)
    {
      lto_add = "_lto";
      extra = 4;
    }
  snprintf (sname, 64, "*Lsection%.*s%.*s", namelen, name, extra, lto_add);
  darwin_asm_output_dwarf_delta (file, size, lab, sname, offset);
}

/* Called from the within the TARGET_ASM_FILE_START for each target.  */

void
darwin_file_start (void)
{
  /* Nothing to do.  */
}

/* Called for the TARGET_ASM_FILE_END hook.
   Emit the mach-o pic indirection data, the lto data and, finally a flag
   to tell the linker that it can break the file object into sections and
   move those around for efficiency.  */

void
darwin_file_end (void)
{
  if (!vec_safe_is_empty (ctors))
    finalize_ctors ();
  if (!vec_safe_is_empty (dtors))
    finalize_dtors ();

  /* If we are expecting to output NeXT ObjC meta-data, (and we actually see
     some) then we output the fix-and-continue marker (Image Info).
     This applies to Objective C, Objective C++ and LTO with either language
     as part of the input.  */
  if (flag_next_runtime && objc_metadata_seen)
    {
      unsigned int flags = 0;
      if (flag_objc_abi >= 2)
	{
	  flags = 16;
          switch_to_section (darwin_sections[objc2_image_info_section]);
	}
      else
	switch_to_section (darwin_sections[objc_image_info_section]);

      ASM_OUTPUT_ALIGN (asm_out_file, 2);
      fputs ("L_OBJC_ImageInfo:\n", asm_out_file);

      flags |= (flag_replace_objc_classes && classes_seen) ? 1 : 0;
      flags |= flag_objc_gc ? 2 : 0;

      fprintf (asm_out_file, "\t.long\t0\n\t.long\t%u\n", flags);
     }

  machopic_finish (asm_out_file);
  if (flag_apple_kext)
    {
      /* These sections are only used for kernel code.  */
      switch_to_section (darwin_sections[constructor_section]);
      switch_to_section (darwin_sections[destructor_section]);
      ASM_OUTPUT_ALIGN (asm_out_file, 1);
    }

  /* If there was LTO assembler output, append it to asm_out_file.  */
  if (lto_asm_out_name)
    {
      int n;
      char *buf, *lto_asm_txt;

      /* Shouldn't be here if we failed to switch back.  */
      gcc_assert (! saved_asm_out_file);

      lto_asm_out_file = fopen (lto_asm_out_name, "r");
      if (lto_asm_out_file == NULL)
	fatal_error (input_location,
		     "failed to open temporary file %s with LTO output",
		     lto_asm_out_name);
      fseek (lto_asm_out_file, 0, SEEK_END);
      n = ftell (lto_asm_out_file);
      if (n > 0)
        {
	  fseek (lto_asm_out_file, 0, SEEK_SET);
	  lto_asm_txt = buf = (char *) xmalloc (n + 1);
	  while (fgets (lto_asm_txt, n, lto_asm_out_file))
	    fputs (lto_asm_txt, asm_out_file);
	  /* Put a termination label.  */
	  fprintf (asm_out_file, "\t.section %s,%s,regular,debug\n",
		   LTO_SEGMENT_NAME, LTO_SECTS_SECTION);
	  fprintf (asm_out_file, "L_GNU_LTO%d:\t;# end of lto\n",
		   lto_section_num);
	  /* Make sure our termination label stays in this section.  */
	  fputs ("\t.space\t1\n", asm_out_file);
	}

      /* Remove the temporary file.  */
      fclose (lto_asm_out_file);
      unlink_if_ordinary (lto_asm_out_name);
      free (lto_asm_out_name);
    }

  /* Output the names and indices.  */
  if (lto_section_names && lto_section_names->length ())
    {
      int count;
      darwin_lto_section_e *ref;
      /* For now, we'll make the offsets 4 bytes and unaligned - we'll fix
         the latter up ourselves.  */
      const char *op = integer_asm_op (4,0);

      /* Emit the names.  */
      fprintf (asm_out_file, "\t.section %s,%s,regular,debug\n",
	       LTO_SEGMENT_NAME, LTO_NAMES_SECTION);
      FOR_EACH_VEC_ELT (*lto_section_names, count, ref)
	{
	  fprintf (asm_out_file, "L_GNU_LTO_NAME%d:\n", count);
         /* We have to jump through hoops to get the values of the intra-section
            offsets... */
	  fprintf (asm_out_file,
		   "\t.set L$gnu$lto$noff%d,L_GNU_LTO_NAME%d-L_GNU_LTO_NAME0\n",
		   count, count);
	  fprintf (asm_out_file,
		   "\t.set L$gnu$lto$nsiz%d,L_GNU_LTO_NAME%d-L_GNU_LTO_NAME%d\n",
		   count, count+1, count);
	  fprintf (asm_out_file, "\t.asciz\t\"%s\"\n", ref->sectname);
	}
      fprintf (asm_out_file, "L_GNU_LTO_NAME%d:\t;# end\n", lto_section_num);
      /* make sure our termination label stays in this section.  */
      fputs ("\t.space\t1\n", asm_out_file);

      /* Emit the Index.  */
      fprintf (asm_out_file, "\t.section %s,%s,regular,debug\n",
	       LTO_SEGMENT_NAME, LTO_INDEX_SECTION);
      fputs ("\t.align\t2\n", asm_out_file);
      fputs ("# Section offset, Section length, Name offset, Name length\n",
	     asm_out_file);
      FOR_EACH_VEC_ELT (*lto_section_names, count, ref)
	{
	  fprintf (asm_out_file, "%s L$gnu$lto$offs%d\t;# %s\n",
		   op, count, ref->sectname);
	  fprintf (asm_out_file, "%s L$gnu$lto$size%d\n", op, count);
	  fprintf (asm_out_file, "%s L$gnu$lto$noff%d\n", op, count);
	  fprintf (asm_out_file, "%s L$gnu$lto$nsiz%d\n", op, count);
	}
    }

  /* If we have section anchors, then we must prevent the linker from
     re-arranging data.  */
  if (!DARWIN_SECTION_ANCHORS || !flag_section_anchors)
    fprintf (asm_out_file, "\t.subsections_via_symbols\n");

  /* We rely on this being NULL at the start of compilation; reset it here
     so that JIT can reuse a context.  */
  if (dwarf_sect_names_table != NULL)
    {
      dwarf_sect_names_table->truncate (0);
      dwarf_sect_names_table = NULL;
    }
}

/* TODO: Add a language hook for identifying if a decl is a vtable.  */
#define DARWIN_VTABLE_P(DECL) 0

/* Cross-module name binding.  Darwin does not support overriding
   functions at dynamic-link time, except for vtables in kexts.  */

bool
darwin_binds_local_p (const_tree decl)
{
  /* We use the "shlib" input to indicate that a symbol should be
     considered overridable; only relevant for vtables in kernel modules
     on earlier system versions, and with a TODO to complete.  */
  bool force_overridable = TARGET_KEXTABI && DARWIN_VTABLE_P (decl);
  return default_binds_local_p_3 (decl, force_overridable /* shlib */,
				  false /* weak dominate */,
				  false /* extern_protected_data */,
				  false /* common_local_p */);
}

/* The Darwin's implementation of TARGET_ASM_OUTPUT_ANCHOR.  Define the
   anchor relative to ".", the current section position.  We cannot use
   the default one because ASM_OUTPUT_DEF is wrong for Darwin.  */
void
darwin_asm_output_anchor (rtx symbol)
{
  fprintf (asm_out_file, "\t.set\t");
  assemble_name (asm_out_file, XSTR (symbol, 0));
  fprintf (asm_out_file, ", . + " HOST_WIDE_INT_PRINT_DEC "\n",
	   SYMBOL_REF_BLOCK_OFFSET (symbol));
}

/* Disable section anchoring on any section containing a zero-sized
   object.  */
bool
darwin_use_anchors_for_symbol_p (const_rtx symbol)
{
  if (DARWIN_SECTION_ANCHORS && flag_section_anchors)
    {
      section *sect;
      /* If the section contains a zero-sized object it's ineligible.  */
      sect = SYMBOL_REF_BLOCK (symbol)->sect;
      /* This should have the effect of disabling anchors for vars that follow
         any zero-sized one, in a given section.  */
      if (sect->common.flags & SECTION_NO_ANCHOR)
	return false;

      /* Also check the normal reasons for suppressing.  */
      return default_use_anchors_for_symbol_p (symbol);
    }
  else
    return false;
}

/* Set the darwin specific attributes on TYPE.  */
void
darwin_set_default_type_attributes (tree type)
{
  if (darwin_ms_struct
      && TREE_CODE (type) == RECORD_TYPE)
    TYPE_ATTRIBUTES (type) = tree_cons (get_identifier ("ms_struct"),
                                        NULL_TREE,
                                        TYPE_ATTRIBUTES (type));
}

/* True, iff we're generating code for loadable kernel extensions.  */

bool
darwin_kextabi_p (void) {
  return flag_apple_kext;
}

void
darwin_override_options (void)
{
  /* Keep track of which (major) version we're generating code for.  */
  if (darwin_macosx_version_min)
    {
      if (strverscmp (darwin_macosx_version_min, "10.7") >= 0)
	generating_for_darwin_version = 11;
      else if (strverscmp (darwin_macosx_version_min, "10.6") >= 0)
	generating_for_darwin_version = 10;
      else if (strverscmp (darwin_macosx_version_min, "10.5") >= 0)
	generating_for_darwin_version = 9;
      else if (strverscmp (darwin_macosx_version_min, "10.4") >= 0)
	generating_for_darwin_version = 8;

      /* Earlier versions are not specifically accounted, until required.  */
    }

  /* Some codegen needs to account for the capabilities of the target
     linker.  */
  if (darwin_target_linker)
    {
      /* Older Darwin ld could not coalesce weak entities without them being
	 placed in special sections.  */
      if (strverscmp (darwin_target_linker, MIN_LD64_NO_COAL_SECTS) < 0)
	ld_uses_coal_sects = true;

      /* Some newer assemblers emit section start temp symbols for mod init
	 and term sections if there is no suitable symbol present already.
	 The temp symbols are linker visible and therefore appear in the
	 symbol tables.  Since the temp symbol number can vary when debug is
	 enabled, that causes compare-debug fails.  The solution is to provide
	 a stable linker-visible symbol.  */
      if (strverscmp (darwin_target_linker,
		      MIN_LD64_INIT_TERM_START_LABELS) >= 0)
	ld_init_term_start_labels = true;
    }

  /* In principle, this should be c-family only.  However, we really need to
     set sensible defaults for LTO as well, since the section selection stuff
     should check for correctness re. the ABI.  TODO: check and provide the
     flags (runtime & ABI) from the lto wrapper).  */

  /* At present, make a hard update to the runtime version based on the target
     OS version.  */
  if (flag_next_runtime)
    {
      if (generating_for_darwin_version > 10)
	flag_next_runtime = 100705;
      else if (generating_for_darwin_version > 9)
	flag_next_runtime = 100608;
      else if (generating_for_darwin_version > 8)
	flag_next_runtime = 100508;
      else
	flag_next_runtime = 100000;
    }

  /* Unless set, force ABI=2 for NeXT and m64, 0 otherwise.  */
  if (!OPTION_SET_P (flag_objc_abi))
    global_options.x_flag_objc_abi
	= (!flag_next_runtime)
		? 0
		: (TARGET_64BIT ? 2
				: (generating_for_darwin_version >= 9) ? 1
								       : 0);

  if (OPTION_SET_P (flag_objc_abi) && flag_next_runtime)
    {
      if (TARGET_64BIT && global_options.x_flag_objc_abi != 2)
	/* The Objective-C family ABI 2 is the only valid version NeXT/m64.  */
	error_at (UNKNOWN_LOCATION,
		  "%<-fobjc-abi-version%> 2 must be used for 64 bit targets"
		  " with %<-fnext-runtime%>");
      else if (!TARGET_64BIT && global_options.x_flag_objc_abi >= 2)
	/* ABI versions 0 and 1 are the only valid versions NeXT/m32.  */
	error_at (UNKNOWN_LOCATION,
		  "%<-fobjc-abi-version%> %d is not supported for 32 bit"
		  " targets with %<-fnext-runtime%>",
		  global_options.x_flag_objc_abi);
    }

  /* Don't emit DWARF3/4 unless specifically selected.  This is a
     workaround for tool bugs.  */
  if (!OPTION_SET_P (dwarf_strict))
    dwarf_strict = 1;
  if (!OPTION_SET_P (dwarf_version))
    dwarf_version = 2;

  if (OPTION_SET_P (dwarf_split_debug_info))
    {
      inform (input_location,
	      "%<-gsplit-dwarf%> is not supported on this platform, ignored");
      dwarf_split_debug_info = 0;
      OPTION_SET_P (dwarf_split_debug_info) = 0;
    }

  /* Do not allow unwind tables to be generated by default for m32.
     fnon-call-exceptions will override this, regardless of what we do.  */
  if (generating_for_darwin_version < 10
      && !OPTION_SET_P (flag_asynchronous_unwind_tables)
      && !TARGET_64BIT)
    global_options.x_flag_asynchronous_unwind_tables = 0;

   /* Disable -freorder-blocks-and-partition when unwind tables are being
      emitted for Darwin < 9 (OSX 10.5).
      The strategy is, "Unless the User has specifically set/unset an unwind
      flag we will switch off -freorder-blocks-and-partition when unwind tables
      will be generated".  If the User specifically sets flags... we assume
      (s)he knows why...  */
   if (generating_for_darwin_version < 9
       && OPTION_SET_P (flag_reorder_blocks_and_partition)
       && ((global_options.x_flag_exceptions 		/* User, c++, java */
	    && !OPTION_SET_P (flag_exceptions)) 	/* User specified... */
	   || (global_options.x_flag_unwind_tables
	       && !OPTION_SET_P (flag_unwind_tables))
	   || (global_options.x_flag_non_call_exceptions
	       && !OPTION_SET_P (flag_non_call_exceptions))
	   || (global_options.x_flag_asynchronous_unwind_tables
	       && !OPTION_SET_P (flag_asynchronous_unwind_tables))))
    {
      inform (input_location,
	      "%<-freorder-blocks-and-partition%> does not work with "
	      "exceptions on this architecture");
      flag_reorder_blocks_and_partition = 0;
      flag_reorder_blocks = 1;
    }

    /* FIXME: flag_objc_sjlj_exceptions is no longer needed since there is only
       one valid choice of exception scheme for each runtime.  */
    if (!OPTION_SET_P (flag_objc_sjlj_exceptions))
      global_options.x_flag_objc_sjlj_exceptions =
				flag_next_runtime && !TARGET_64BIT;

    /* FIXME: and this could be eliminated then too.  */
    if (!OPTION_SET_P (flag_exceptions)
	&& flag_objc_exceptions
	&& TARGET_64BIT)
      flag_exceptions = 1;

  if (flag_mkernel || flag_apple_kext)
    {
      /* -mkernel implies -fapple-kext for C++ */
      if (lang_GNU_CXX ())
	flag_apple_kext = 1;

      flag_no_common = 1;

      /* No EH in kexts.  */
      flag_exceptions = 0;
      /* No -fnon-call-exceptions data in kexts.  */
      flag_non_call_exceptions = 0;
      /* so no tables either.. */
      flag_unwind_tables = 0;
      flag_asynchronous_unwind_tables = 0;
    }

  if (flag_var_tracking_uninit == 0
      && generating_for_darwin_version >= 9
      && (flag_gtoggle ? (debug_info_level == DINFO_LEVEL_NONE)
      : (debug_info_level >= DINFO_LEVEL_NORMAL))
      && dwarf_debuginfo_p ())
    flag_var_tracking_uninit = flag_var_tracking;

  /* Final check on PCI options; for Darwin these are not dependent on the PIE
     ones, although PIE does require PIC to support it.  */
  if (MACHO_DYNAMIC_NO_PIC_P)
    {
      if (flag_pic)
	warning_at (UNKNOWN_LOCATION, 0,
		 "%<-mdynamic-no-pic%> overrides %<-fpic%>, %<-fPIC%>,"
		 " %<-fpie%> or %<-fPIE%>");
      flag_pic = 0;
    }
  else if (flag_pic == 1
	   || (flag_pic == 0 && !(flag_mkernel || flag_apple_kext)))
    {
      /* Darwin's -fpic is -fPIC.
	 We only support "static" code in the kernel and kernel exts.  */
      flag_pic = 2;
    }

  /* Linkers >= ld64-62.1 (at least) are capable of making the necessary PIC
     indirections and we no longer need to emit pic symbol stubs.
     However, if we are generating code for earlier ones (or for use in the
     kernel) the stubs might still be required, and this will be set true.
     If the user sets it on or off - then that takes precedence.

     Linkers that don't need stubs, don't need the EH symbol markers either.
  */

  if (!OPTION_SET_P (darwin_symbol_stubs))
    {
      if (darwin_target_linker)
	{
	  if (strverscmp (darwin_target_linker, MIN_LD64_OMIT_STUBS) < 0)
	    {
	      darwin_symbol_stubs = true;
	      ld_needs_eh_markers = true;
	    }
	}
      else if (generating_for_darwin_version < 9)
	{
	  /* If we don't know the linker version and we're targeting an old
	     system, we know no better than to assume the use of an earlier
	     linker.  */
	  darwin_symbol_stubs = true;
	  ld_needs_eh_markers = true;
	}
    }
  else if (DARWIN_X86 && darwin_symbol_stubs && TARGET_64BIT)
    {
      inform (input_location,
	      "%<-mpic-symbol-stubs%> is not required for 64-bit code "
	      "(ignored)");
      darwin_symbol_stubs = false;
    }

  if (generating_for_darwin_version >= 9)
    /* Later systems can support aligned common.  */
    emit_aligned_common = true;

  /* The c_dialect...() macros are not available to us here.  */
  darwin_running_cxx = (strstr (lang_hooks.name, "C++") != 0);
}

#if DARWIN_PPC
/* Add $LDBL128 suffix to long double builtins for ppc darwin.  */

static void
darwin_patch_builtin (enum built_in_function fncode)
{
  tree fn = builtin_decl_explicit (fncode);
  tree sym;
  char *newname;

  if (!fn)
    return;

  sym = DECL_ASSEMBLER_NAME (fn);
  newname = ACONCAT (("_", IDENTIFIER_POINTER (sym), "$LDBL128", NULL));

  set_user_assembler_name (fn, newname);

  fn = builtin_decl_implicit (fncode);
  if (fn)
    set_user_assembler_name (fn, newname);
}

void
darwin_patch_builtins (void)
{
  if (LONG_DOUBLE_TYPE_SIZE != 128)
    return;

#define PATCH_BUILTIN(fncode) darwin_patch_builtin (fncode);
#define PATCH_BUILTIN_NO64(fncode)		\
  if (!TARGET_64BIT)				\
    darwin_patch_builtin (fncode);
#define PATCH_BUILTIN_VARIADIC(fncode)				  \
  if (!TARGET_64BIT						  \
      && (strverscmp (darwin_macosx_version_min, "10.3.9") >= 0)) \
    darwin_patch_builtin (fncode);
#include "darwin-ppc-ldouble-patch.def"
#undef PATCH_BUILTIN
#undef PATCH_BUILTIN_NO64
#undef PATCH_BUILTIN_VARIADIC
}
#endif

/*  CFStrings implementation.  */
static GTY(()) tree cfstring_class_reference = NULL_TREE;
static GTY(()) tree cfstring_type_node = NULL_TREE;
static GTY(()) tree ccfstring_type_node = NULL_TREE;
static GTY(()) tree pccfstring_type_node = NULL_TREE;
static GTY(()) tree pcint_type_node = NULL_TREE;
static GTY(()) tree pcchar_type_node = NULL_TREE;

static enum built_in_function darwin_builtin_cfstring;

/* Store all constructed constant CFStrings in a hash table so that
   they get uniqued properly.  */

typedef struct GTY ((for_user)) cfstring_descriptor {
  /* The string literal.  */
  tree literal;
  /* The resulting constant CFString.  */
  tree constructor;
} cfstring_descriptor;

struct cfstring_hasher : ggc_ptr_hash<cfstring_descriptor>
{
  static hashval_t hash (cfstring_descriptor *);
  static bool equal (cfstring_descriptor *, cfstring_descriptor *);
};

static GTY (()) hash_table<cfstring_hasher> *cfstring_htab;

static tree
add_builtin_field_decl (tree type, const char *name, tree **chain)
{
  tree field = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			    get_identifier (name), type);

  if (*chain != NULL)
    **chain = field;
  *chain = &DECL_CHAIN (field);

  return field;
}

tree
darwin_init_cfstring_builtins (unsigned builtin_cfstring)
{
  tree cfsfun, fields, pccfstring_ftype_pcchar;
  tree *chain = NULL;

  darwin_builtin_cfstring =
    (enum built_in_function) builtin_cfstring;

  /* struct __builtin_CFString {
       const int *isa;		(will point at
       int flags;		 __CFConstantStringClassReference)
       const char *str;
       long length;
     };  */

  pcint_type_node = build_pointer_type
		   (build_qualified_type (integer_type_node, TYPE_QUAL_CONST));

  pcchar_type_node = build_pointer_type
		   (build_qualified_type (char_type_node, TYPE_QUAL_CONST));

  cfstring_type_node = (*lang_hooks.types.make_type) (RECORD_TYPE);

  /* Have to build backwards for finish struct.  */
  fields = add_builtin_field_decl (long_integer_type_node, "length", &chain);
  add_builtin_field_decl (pcchar_type_node, "str", &chain);
  add_builtin_field_decl (integer_type_node, "flags", &chain);
  add_builtin_field_decl (pcint_type_node, "isa", &chain);
  finish_builtin_struct (cfstring_type_node, "__builtin_CFString",
			 fields, NULL_TREE);

  /* const struct __builtin_CFstring *
     __builtin___CFStringMakeConstantString (const char *); */

  ccfstring_type_node = build_qualified_type
			(cfstring_type_node, TYPE_QUAL_CONST);
  pccfstring_type_node = build_pointer_type (ccfstring_type_node);
  pccfstring_ftype_pcchar = build_function_type_list
			(pccfstring_type_node, pcchar_type_node, NULL_TREE);

  cfsfun  = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
			get_identifier ("__builtin___CFStringMakeConstantString"),
			pccfstring_ftype_pcchar);

  TREE_PUBLIC (cfsfun) = 1;
  DECL_EXTERNAL (cfsfun) = 1;
  DECL_ARTIFICIAL (cfsfun) = 1;
  /* Make a lang-specific section - dup_lang_specific_decl makes a new node
     in place of the existing, which may be NULL.  */
  DECL_LANG_SPECIFIC (cfsfun) = NULL;
  (*lang_hooks.dup_lang_specific_decl) (cfsfun);
  set_decl_built_in_function (cfsfun, BUILT_IN_MD, darwin_builtin_cfstring);
  lang_hooks.builtin_function (cfsfun);

  /* extern int __CFConstantStringClassReference[];  */
  cfstring_class_reference = build_decl (BUILTINS_LOCATION, VAR_DECL,
		 get_identifier ("__CFConstantStringClassReference"),
		 build_array_type (integer_type_node, NULL_TREE));

  TREE_PUBLIC (cfstring_class_reference) = 1;
  DECL_ARTIFICIAL (cfstring_class_reference) = 1;
  (*lang_hooks.decls.pushdecl) (cfstring_class_reference);
  DECL_EXTERNAL (cfstring_class_reference) = 1;
  rest_of_decl_compilation (cfstring_class_reference, 0, 0);

  /* Initialize the hash table used to hold the constant CFString objects.  */
  cfstring_htab = hash_table<cfstring_hasher>::create_ggc (31);

  return cfstring_type_node;
}

tree
darwin_fold_builtin (tree fndecl, int n_args, tree *argp,
		     bool ARG_UNUSED (ignore))
{
  unsigned int fcode = DECL_MD_FUNCTION_CODE (fndecl);

  if (fcode == darwin_builtin_cfstring)
    {
      if (!darwin_constant_cfstrings)
	{
	  error ("built-in function %qD requires the"
		 " %<-mconstant-cfstrings%> flag", fndecl);
	  return error_mark_node;
	}

      if (n_args != 1)
	{
	  error ("built-in function %qD takes one argument only", fndecl);
	  return error_mark_node;
	}

      return darwin_build_constant_cfstring (*argp);
    }

  return NULL_TREE;
}

void
darwin_rename_builtins (void)
{
  /* The system ___divdc3 routine in libSystem on darwin10 is not
     accurate to 1ulp, ours is, so we avoid ever using the system name
     for this routine and instead install a non-conflicting name that
     is accurate.

     When -ffast-math or -funsafe-math-optimizations is given, we can
     use the faster version.  */
  if (!flag_unsafe_math_optimizations)
    {
      enum built_in_function dcode
	= (enum built_in_function)(BUILT_IN_COMPLEX_DIV_MIN
				   + DCmode - MIN_MODE_COMPLEX_FLOAT);
      tree fn = builtin_decl_explicit (dcode);
      /* Fortran and c call TARGET_INIT_BUILTINS and
	 TARGET_INIT_LIBFUNCS at different times, so we have to put a
	 call into each to ensure that at least one of them is called
	 after build_common_builtin_nodes.  A better fix is to add a
	 new hook to run after build_common_builtin_nodes runs.  */
      if (fn)
	set_user_assembler_name (fn, "___ieee_divdc3");
      fn = builtin_decl_implicit (dcode);
      if (fn)
	set_user_assembler_name (fn, "___ieee_divdc3");
    }
}

/* Implementation for the TARGET_LIBC_HAS_FUNCTION hook.  */

bool
darwin_libc_has_function (enum function_class fn_class,
			  tree type ATTRIBUTE_UNUSED)
{
  if (fn_class == function_sincos && darwin_macosx_version_min)
    return (strverscmp (darwin_macosx_version_min, "10.9") >= 0);
#if DARWIN_PPC && SUPPORT_DARWIN_LEGACY
  if (fn_class == function_c99_math_complex
      || fn_class == function_c99_misc)
    return (TARGET_64BIT
	    || (darwin_macosx_version_min &&
		strverscmp (darwin_macosx_version_min, "10.3") >= 0));
#endif
  return default_libc_has_function (fn_class, type);
}

hashval_t
cfstring_hasher::hash (cfstring_descriptor *ptr)
{
  tree str = ptr->literal;
  const unsigned char *p = (const unsigned char *) TREE_STRING_POINTER (str);
  int i, len = TREE_STRING_LENGTH (str);
  hashval_t h = len;

  for (i = 0; i < len; i++)
    h = ((h * 613) + p[i]);

  return h;
}

bool
cfstring_hasher::equal (cfstring_descriptor *ptr1, cfstring_descriptor *ptr2)
{
  tree str1 = ptr1->literal;
  tree str2 = ptr2->literal;
  int len1 = TREE_STRING_LENGTH (str1);

  return (len1 == TREE_STRING_LENGTH (str2)
	  && !memcmp (TREE_STRING_POINTER (str1), TREE_STRING_POINTER (str2),
		      len1));
}

tree
darwin_build_constant_cfstring (tree str)
{
  struct cfstring_descriptor *desc, key;
  tree addr;

  if (!str)
    {
      error ("CFString literal is missing");
      return error_mark_node;
    }

  STRIP_NOPS (str);

  if (TREE_CODE (str) == ADDR_EXPR)
    str = TREE_OPERAND (str, 0);

  if (TREE_CODE (str) != STRING_CST)
    {
      error ("CFString literal expression is not a string constant");
      return error_mark_node;
    }

  /* Perhaps we already constructed a constant CFString just like this one? */
  key.literal = str;
  cfstring_descriptor **loc = cfstring_htab->find_slot (&key, INSERT);
  desc = *loc;

  if (!desc)
    {
      tree var, constructor, field;
      vec<constructor_elt, va_gc> *v = NULL;
      int length = TREE_STRING_LENGTH (str) - 1;

      if (darwin_warn_nonportable_cfstrings)
	{
	  const char *s = TREE_STRING_POINTER (str);
	  int l = 0;

	  for (l = 0; l < length; l++)
	    if (!s[l] || !isascii (s[l]))
	      {
		warning (darwin_warn_nonportable_cfstrings,
			 s[l] ? G_("non-ASCII character in CFString literal")
			      : G_("embedded NUL in CFString literal"));
		break;
	      }
	}

      *loc = desc = ggc_cleared_alloc<cfstring_descriptor> ();
      desc->literal = str;

      /* isa *. */
      field = TYPE_FIELDS (ccfstring_type_node);
      CONSTRUCTOR_APPEND_ELT(v, NULL_TREE,
			     build1 (ADDR_EXPR,  TREE_TYPE (field),
				     cfstring_class_reference));
      /* flags */
      field = DECL_CHAIN (field);
      CONSTRUCTOR_APPEND_ELT(v, NULL_TREE,
			     build_int_cst (TREE_TYPE (field), 0x000007c8));
      /* string *. */
      field = DECL_CHAIN (field);
      CONSTRUCTOR_APPEND_ELT(v, NULL_TREE,
			     build1 (ADDR_EXPR, TREE_TYPE (field), str));
      /* length */
      field = DECL_CHAIN (field);
      CONSTRUCTOR_APPEND_ELT(v, NULL_TREE,
			     build_int_cst (TREE_TYPE (field), length));

      constructor = build_constructor (ccfstring_type_node, v);
      TREE_READONLY (constructor) = 1;
      TREE_CONSTANT (constructor) = 1;
      TREE_STATIC (constructor) = 1;

      /* Fromage: The C++ flavor of 'build_unary_op' expects constructor nodes
	 to have the TREE_HAS_CONSTRUCTOR (...) bit set.  However, this file is
	 being built without any knowledge of C++ tree accessors; hence, we shall
	 use the generic accessor that TREE_HAS_CONSTRUCTOR actually maps to!  */
      if (darwin_running_cxx)
	TREE_LANG_FLAG_4 (constructor) = 1;  /* TREE_HAS_CONSTRUCTOR  */

      /* Create an anonymous global variable for this CFString.  */
      var = build_decl (input_location, CONST_DECL,
			NULL, TREE_TYPE (constructor));
      DECL_ARTIFICIAL (var) = 1;
      TREE_STATIC (var) = 1;
      DECL_INITIAL (var) = constructor;
      /* FIXME: This should use a translation_unit_decl to indicate file scope.  */
      DECL_CONTEXT (var) = NULL_TREE;
      desc->constructor = var;
    }

  addr = build1 (ADDR_EXPR, pccfstring_type_node, desc->constructor);
  TREE_CONSTANT (addr) = 1;

  return addr;
}

bool
darwin_cfstring_p (tree str)
{
  struct cfstring_descriptor key;

  if (!str)
    return false;

  STRIP_NOPS (str);

  if (TREE_CODE (str) == ADDR_EXPR)
    str = TREE_OPERAND (str, 0);

  if (TREE_CODE (str) != STRING_CST)
    return false;

  key.literal = str;
  cfstring_descriptor **loc = cfstring_htab->find_slot (&key, NO_INSERT);

  if (loc)
    return true;

  return false;
}

void
darwin_enter_string_into_cfstring_table (tree str)
{
  struct cfstring_descriptor key;

  key.literal = str;
  cfstring_descriptor **loc = cfstring_htab->find_slot (&key, INSERT);

  if (!*loc)
    {
      *loc = ggc_cleared_alloc<cfstring_descriptor> ();
      ((struct cfstring_descriptor *)*loc)->literal = str;
    }
}

/* Choose named function section based on its frequency.  */

section *
darwin_function_section (tree decl, enum node_frequency freq,
			  bool startup, bool exit)
{
  /* Decide if we need to put this in a coalescable section.  */
  bool weak = (decl
	       && DECL_WEAK (decl)
	       && (!DECL_ATTRIBUTES (decl)
		   || !lookup_attribute ("weak_import",
					  DECL_ATTRIBUTES (decl))));

  bool use_coal = weak && ld_uses_coal_sects;
  /* If there is a specified section name, we should not be trying to
     override.  */
  if (decl && DECL_SECTION_NAME (decl) != NULL)
    return get_named_section (decl, NULL, 0);

  /* We always put unlikely executed stuff in the cold section.  */
  if (freq == NODE_FREQUENCY_UNLIKELY_EXECUTED)
    return (use_coal) ? darwin_sections[text_cold_coal_section]
		      : darwin_sections[text_cold_section];

  /* If we have LTO *and* feedback information, then let LTO handle
     the function ordering, it makes a better job (for normal, hot,
     startup and exit - hence the bailout for cold above).  */
  if (in_lto_p && flag_profile_values)
    goto default_function_sections;

  /* Non-cold startup code should go to startup subsection.  */
  if (startup)
    return (use_coal) ? darwin_sections[text_startup_coal_section]
		      : darwin_sections[text_startup_section];

  /* Similarly for exit.  */
  if (exit)
    return (use_coal) ? darwin_sections[text_exit_coal_section]
		      : darwin_sections[text_exit_section];

  /* Place hot code.  */
  if (freq == NODE_FREQUENCY_HOT)
    return (use_coal) ? darwin_sections[text_hot_coal_section]
		      : darwin_sections[text_hot_section];

  /* Otherwise, default to the 'normal' non-reordered sections.  */
default_function_sections:
  return (use_coal) ? darwin_sections[text_coal_section]
		    : text_section;
}

#include "gt-darwin.h"
