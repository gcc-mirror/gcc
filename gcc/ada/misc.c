/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 M I S C                                  *
 *                                                                          *
 *                           C Implementation File                          *
 *                                                                          *
 *                             $Revision: 1.17 $
 *                                                                          *
 *          Copyright (C) 1992-2001 Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file contains parts of the compiler that are required for interfacing
   with GCC but otherwise do nothing and parts of Gigi that need to know
   about RTL.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "errors.h"
#include "diagnostic.h"
#include "expr.h"
#include "ggc.h"
#include "flags.h"
#include "insn-codes.h"
#include "insn-flags.h"
#include "insn-config.h"
#include "optabs.h"
#include "recog.h"
#include "toplev.h"
#include "output.h"
#include "except.h"
#include "tm_p.h"
#include "langhooks.h"
#include "langhooks-def.h"

#include "ada.h"
#include "types.h"
#include "atree.h"
#include "elists.h"
#include "namet.h"
#include "nlists.h"
#include "stringt.h"
#include "uintp.h"
#include "fe.h"
#include "sinfo.h"
#include "einfo.h"
#include "ada-tree.h"
#include "gigi.h"

extern FILE *asm_out_file;
extern int save_argc;
extern char **save_argv;

/* Tables describing GCC tree codes used only by GNAT.  

   Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See cp-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

static char const gnat_tree_code_type[] = {
  'x',
#include "ada-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

static int const gnat_tree_code_length[] = {
  0,
#include "ada-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

static const char *gnat_tree_code_name[] = {
  "@@dummy",
#include "ada-tree.def"
};
#undef DEFTREECODE

static const char *gnat_init		PARAMS ((const char *));
static void gnat_init_options		PARAMS ((void));
static int gnat_decode_option		PARAMS ((int, char **));
static HOST_WIDE_INT gnat_get_alias_set	PARAMS ((tree));
static void gnat_print_decl		PARAMS ((FILE *, tree, int));
static void gnat_print_type		PARAMS ((FILE *, tree, int));
extern void gnat_init_decl_processing	PARAMS ((void));
static tree gnat_expand_constant	PARAMS ((tree));

/* Structure giving our language-specific hooks.  */

#undef  LANG_HOOKS_NAME
#define LANG_HOOKS_NAME			"GNU Ada"
#undef  LANG_HOOKS_IDENTIFIER_SIZE
#define LANG_HOOKS_IDENTIFIER_SIZE	sizeof (struct tree_identifier)
#undef  LANG_HOOKS_INIT
#define LANG_HOOKS_INIT			gnat_init
#undef  LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS		gnat_init_options
#undef  LANG_HOOKS_DECODE_OPTION
#define LANG_HOOKS_DECODE_OPTION	gnat_decode_option
#undef LANG_HOOKS_HONOR_READONLY
#define LANG_HOOKS_HONOR_READONLY	1
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET	gnat_get_alias_set
#undef LANG_HOOKS_PRINT_DECL
#define LANG_HOOKS_PRINT_DECL		gnat_print_decl
#undef LANG_HOOKS_PRINT_TYPE
#define LANG_HOOKS_PRINT_TYPE		gnat_print_type
#undef LANG_HOOKS_EXPAND_CONSTANT
#define LANG_HOOKS_EXPAND_CONSTANT	gnat_expand_constant

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* gnat standard argc argv */

extern int gnat_argc;
extern char **gnat_argv;

/* Global Variables Expected by gcc: */

int flag_traditional;		/* Used by dwarfout.c.  */
int ggc_p = 1;

static void internal_error_function	PARAMS ((const char *, va_list *));
static rtx gnat_expand_expr		PARAMS ((tree, rtx, enum machine_mode,
						 enum expand_modifier));
static void gnat_adjust_rli		PARAMS ((record_layout_info));

#if defined(MIPS_DEBUGGING_INFO) && defined(DWARF2_DEBUGGING_INFO)
static char *convert_ada_name_to_qualified_name PARAMS ((char *));
#endif

/* Routines Expected by gcc:  */

/* For most front-ends, this is the parser for the language.  For us, we
   process the GNAT tree.  */

/* Declare functions we use as part of startup.  */
extern void __gnat_initialize	PARAMS((void));
extern void adainit		PARAMS((void));
extern void _ada_gnat1drv	PARAMS((void));

int
yyparse ()
{
  /* call the target specific initializations */
  __gnat_initialize();

  /* Call the front-end elaboration procedures */
  adainit ();

  immediate_size_expand = 1;

  /* Call the front end */
  _ada_gnat1drv ();

  return 0;
}

/* Decode all the language specific options that cannot be decoded by GCC.
   The option decoding phase of GCC calls this routine on the flags that
   it cannot decode. This routine returns 1 if it is successful, otherwise
   it returns 0. */

static int
gnat_decode_option (argc, argv)
     int argc ATTRIBUTE_UNUSED;
     char **argv;
{
  char *p = argv[0];
  int i;

  if (!strncmp (p, "-I", 2))
    {
      /* Pass the -I switches as-is. */
      gnat_argv[gnat_argc] = p;
      gnat_argc ++;
      return 1;
    }

  else if (!strncmp (p, "-gant", 5))
    {
      char *q = (char *) xmalloc (strlen (p) + 1);

      warning ("`-gnat' misspelled as `-gant'");
      strcpy (q, p);
      q[2] = 'n', q[3] = 'a';
      p = q;
      return 1;
    }

  else if (!strncmp (p, "-gnat", 5))
    {
      /* Recopy the switches without the 'gnat' prefix */

      gnat_argv[gnat_argc] =  (char *) xmalloc (strlen (p) - 3);
      gnat_argv[gnat_argc][0] = '-';
      strcpy (gnat_argv[gnat_argc] + 1, p + 5);
      gnat_argc ++;
      if (p[5] == 'O')
	for (i = 1; i < save_argc - 1; i++) 
	  if (!strncmp (save_argv[i], "-gnatO", 6))
	    if (save_argv[++i][0] != '-')
	      {
		/* Preserve output filename as GCC doesn't save it for GNAT. */
		gnat_argv[gnat_argc] = save_argv[i];
		gnat_argc++;
		break;
	      }

      return 1;
    }

  /* Ignore -W flags since people may want to use the same flags for all
     languages.  */
  else if (p[0] == '-' && p[1] == 'W' && p[2] != 0)
    return 1;

  return 0;
}

/* Initialize for option processing.  */

static void
gnat_init_options ()
{
  /* Initialize gnat_argv with save_argv size */
  gnat_argv = (char **) xmalloc ((save_argc + 1) * sizeof (gnat_argv[0])); 
  gnat_argv [0] = save_argv[0];     /* name of the command */ 
  gnat_argc = 1;
}

void
lang_mark_tree (t)
     tree t;
{
  switch (TREE_CODE (t))
    {
    case FUNCTION_TYPE:
      ggc_mark_tree (TYPE_CI_CO_LIST (t));
      return;

    case INTEGER_TYPE:
      if (TYPE_MODULAR_P (t))
	ggc_mark_tree (TYPE_MODULUS (t));
      else if (TYPE_VAX_FLOATING_POINT_P (t))
	;
      else if (TYPE_HAS_ACTUAL_BOUNDS_P (t))
	ggc_mark_tree (TYPE_ACTUAL_BOUNDS (t));
      else
	ggc_mark_tree (TYPE_INDEX_TYPE (t));
      return;

    case ENUMERAL_TYPE:
      ggc_mark_tree (TYPE_RM_SIZE_ENUM (t));
      return;

    case ARRAY_TYPE:
      ggc_mark_tree (TYPE_ACTUAL_BOUNDS (t));
      return;

    case RECORD_TYPE:  case UNION_TYPE:  case QUAL_UNION_TYPE:
      /* This is really TYPE_UNCONSTRAINED_ARRAY for fat pointers.  */
      ggc_mark_tree (TYPE_ADA_SIZE (t));
      return;

    case CONST_DECL:
      ggc_mark_tree (DECL_CONST_CORRESPONDING_VAR (t));
      return;

    case FIELD_DECL:
      ggc_mark_tree (DECL_ORIGINAL_FIELD (t));
      return;

    default:
      return;
    }
}

/* Here we have the function to handle the compiler error processing in GCC.  */

static void
internal_error_function (msgid, ap)
     const char *msgid;
     va_list *ap;
{
  char buffer[1000];		/* Assume this is big enough.  */
  char *p;
  String_Template temp;
  Fat_Pointer fp;

  vsprintf (buffer, msgid, *ap);

  /* Go up to the first newline.  */
  for (p = buffer; *p != 0; p++)
    if (*p == '\n')
      {
	*p = '\0';
	break;
      }

  temp.Low_Bound = 1, temp.High_Bound = strlen (buffer);
  fp.Array = buffer, fp.Bounds = &temp;

  Current_Error_Node = error_gnat_node;
  Compiler_Abort (fp, -1);
}

/* Perform all the initialization steps that are language-specific.  */

static const char *
gnat_init (filename)
     const char *filename;
{
/* Performs whatever initialization steps needed by the language-dependent
   lexical analyzer.

   Define the additional tree codes here.  This isn't the best place to put
   it, but it's where g++ does it.  */

  lang_expand_expr = gnat_expand_expr;

  memcpy ((char *) (tree_code_type + (int) LAST_AND_UNUSED_TREE_CODE),
	  (char *) gnat_tree_code_type,
	  ((LAST_GNAT_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	   * sizeof (char *)));

  memcpy ((char *) (tree_code_length + (int) LAST_AND_UNUSED_TREE_CODE),
	  (char *) gnat_tree_code_length,
	  ((LAST_GNAT_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	   * sizeof (int)));

  memcpy ((char *) (tree_code_name + (int) LAST_AND_UNUSED_TREE_CODE),
	  (char *) gnat_tree_code_name,
	  ((LAST_GNAT_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	   * sizeof (char *)));

  gnat_init_decl_processing ();

  /* Add the input filename as the last argument.  */
  gnat_argv [gnat_argc] = (char *) filename;
  gnat_argc++;
  gnat_argv [gnat_argc] = 0;

  set_internal_error_function (internal_error_function);

  /* Show that REFERENCE_TYPEs are internal and should be Pmode.  */
  internal_reference_types ();

  /* Show we don't use the common language attributes.  */
  lang_attribute_common = 0;

  set_lang_adjust_rli (gnat_adjust_rli);

#if defined(MIPS_DEBUGGING_INFO) && defined(DWARF2_DEBUGGING_INFO)
  dwarf2out_set_demangle_name_func (convert_ada_name_to_qualified_name);
#endif

  if (filename == 0)
    filename = "";

  return filename;
}

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

tree
maybe_build_cleanup (decl)
     tree decl ATTRIBUTE_UNUSED;
{
  /* There are no cleanups in C.  */
  return NULL_TREE;
}

/* integrate_decl_tree calls this function, but since we don't use the
   DECL_LANG_SPECIFIC field, this is a no-op.  */

void
copy_lang_decl (node)
     tree node ATTRIBUTE_UNUSED;
{
}

/* Hooks for print-tree.c:  */

static void
gnat_print_decl (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  switch (TREE_CODE (node))
    {
    case CONST_DECL:
      print_node (file, "const_corresponding_var",
		  DECL_CONST_CORRESPONDING_VAR (node), indent + 4);
      break;

    case FIELD_DECL:
      print_node (file, "original field", DECL_ORIGINAL_FIELD (node),
		  indent + 4);
      break;

    default:
      break;
    }
}

static void
gnat_print_type (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  switch (TREE_CODE (node))
    {
    case FUNCTION_TYPE:
      print_node (file, "ci_co_list", TYPE_CI_CO_LIST (node), indent + 4);
      break;

    case ENUMERAL_TYPE:
      print_node (file, "RM size", TYPE_RM_SIZE_ENUM (node), indent + 4);
      break;

    case INTEGER_TYPE:
      if (TYPE_MODULAR_P (node))
	print_node (file, "modulus", TYPE_MODULUS (node), indent + 4);
      else if (TYPE_HAS_ACTUAL_BOUNDS_P (node))
	print_node (file, "actual bounds", TYPE_ACTUAL_BOUNDS (node),
		    indent + 4);
      else if (TYPE_VAX_FLOATING_POINT_P (node))
	;
      else
	print_node (file, "index type", TYPE_INDEX_TYPE (node), indent + 4);

      print_node (file, "RM size", TYPE_RM_SIZE_INT (node), indent + 4);
      break;

    case ARRAY_TYPE:
      print_node (file,"actual bounds", TYPE_ACTUAL_BOUNDS (node), indent + 4);
      break;

    case RECORD_TYPE:
      if (TYPE_IS_FAT_POINTER_P (node) || TYPE_CONTAINS_TEMPLATE_P (node))
	print_node (file, "unconstrained array",
		    TYPE_UNCONSTRAINED_ARRAY (node), indent + 4);
      else
	print_node (file, "Ada size", TYPE_ADA_SIZE (node), indent + 4);
      break;

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      print_node (file, "Ada size", TYPE_ADA_SIZE (node), indent + 4);
      break;

    default:
      break;
    }
}

/* Expands GNAT-specific GCC tree nodes.  The only ones we support
   here are TRANSFORM_EXPR, UNCHECKED_CONVERT_EXPR, ALLOCATE_EXPR,
   USE_EXPR and NULL_EXPR.  */

static rtx
gnat_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree type = TREE_TYPE (exp);
  tree inner_type;
  tree new;
  rtx result;
  int align_ok;

  /* Update EXP to be the new expression to expand.  */

  switch (TREE_CODE (exp))
    {
    case TRANSFORM_EXPR:
      gnat_to_code (TREE_COMPLEXITY (exp));
      return const0_rtx;
      break;

    case UNCHECKED_CONVERT_EXPR:
      inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));

      /* The alignment is OK if the flag saying it is OK is set in either
	 type, if the inner type is already maximally aligned, if the
	 new type is no more strictly aligned than the old type, or
	 if byte accesses are not slow.  */
      align_ok = (! SLOW_BYTE_ACCESS
		  || TYPE_ALIGN_OK_P (type) || TYPE_ALIGN_OK_P (inner_type)
		  || TYPE_ALIGN (inner_type) >= BIGGEST_ALIGNMENT
		  || TYPE_ALIGN (type) <= TYPE_ALIGN (inner_type));

      /* If we're converting between an aggregate and non-aggregate type
	 and we have a MEM TARGET, we can't use it, since MEM_IN_STRUCT_P
	 would be set incorrectly.  */
      if (target != 0 && GET_CODE (target) == MEM
	  && (MEM_IN_STRUCT_P (target) != AGGREGATE_TYPE_P (inner_type)))
	target = 0;

      /* If the input and output are both the same mode (usually BLKmode),
	 just return the expanded input since we want just the bits.  But
	 we can't do this if the output is more strictly aligned than
	 the input or if the type is BLKmode and the sizes differ.  */
      if (TYPE_MODE (type) == TYPE_MODE (inner_type)
	  && align_ok
	  && ! (TYPE_MODE (type) == BLKmode
		&& ! operand_equal_p (TYPE_SIZE (type),
				      TYPE_SIZE (inner_type), 0)))
	{
	  new = TREE_OPERAND (exp, 0);

	  /* If the new type is less strictly aligned than the inner type,
	     make a new type with the less strict alignment just for
	     code generation purposes of this node.  If it is a decl,
	     we can't change the type, so make a NOP_EXPR.  */
	  if (TYPE_ALIGN (type) != TYPE_ALIGN (inner_type))
	    {
	      tree copy_type = copy_node (inner_type);

	      TYPE_ALIGN (copy_type) = TYPE_ALIGN (type);
	      if (DECL_P (new))
		new = build1 (NOP_EXPR, copy_type, new);
	      else
		{
		  /* If NEW is a constant, it might be coming from a CONST_DECL
		     and hence shared.  */
		  if (TREE_CONSTANT (new))
		    new = copy_node (new);

		  TREE_TYPE (new) = copy_type;
		}
	    }
	}

      /* If either mode is BLKmode, memory will be involved, so do this
	 via pointer punning.  Likewise, this doesn't work if there
	 is an alignment issue.  But we must do it for types that are known
	 to be aligned properly.  */
      else if ((TYPE_MODE (type) == BLKmode
		|| TYPE_MODE (inner_type) == BLKmode)
	       && align_ok)
	new = build_unary_op (INDIRECT_REF, NULL_TREE,
			      convert
			      (build_pointer_type (type),
			       build_unary_op (ADDR_EXPR, NULL_TREE,
					       TREE_OPERAND (exp, 0))));

      /* Otherwise make a union of the two types, convert to the union, and
	 extract the other value.  */
      else
	{
	  tree union_type, in_field, out_field;

	  /* If this is inside the LHS of an assignment, this would generate
	     bad code, so abort.  */
	  if (TREE_ADDRESSABLE (exp))
	    gigi_abort (202);

	  union_type = make_node (UNION_TYPE);
	  in_field = create_field_decl (get_identifier ("in"),
					inner_type, union_type, 0, 0, 0, 0);
	  out_field = create_field_decl (get_identifier ("out"),
					 type, union_type, 0, 0, 0, 0);

	  TYPE_FIELDS (union_type) = chainon (in_field, out_field);
	  layout_type (union_type);

	  /* Though this is a "union", we can treat its size as that of
	     the output type in case the size of the input type is variable.
	     If the output size is a variable, use the input size.  */
	  TYPE_SIZE (union_type) = TYPE_SIZE (type);
	  TYPE_SIZE_UNIT (union_type) = TYPE_SIZE (type);
	  if (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	      && TREE_CODE (TYPE_SIZE (inner_type)) == INTEGER_CST)
	    {
	      TYPE_SIZE (union_type) = TYPE_SIZE (inner_type);
	      TYPE_SIZE_UNIT (union_type) = TYPE_SIZE_UNIT (inner_type);
	    }

	  new = build (COMPONENT_REF, type,
		       build1 (CONVERT_EXPR, union_type,
			       TREE_OPERAND (exp, 0)),
		       out_field);
	}

      result = expand_expr (new, target, tmode, modifier);

      if (GET_CODE (result) == MEM)
	{
	  /* Update so it looks like this is of the proper type.  */
	  set_mem_alias_set (result, 0);
	  set_mem_attributes (result, exp, 0);
	}
      return result;

    case NULL_EXPR:
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);

      /* We aren't going to be doing anything with this memory, but allocate
	 it anyway.  If it's variable size, make a bogus address.  */
      if (! host_integerp (TYPE_SIZE_UNIT (type), 1))
	result = gen_rtx_MEM (BLKmode, virtual_stack_vars_rtx);
      else
	result = assign_temp (type, 0, TREE_ADDRESSABLE (exp), 1);

      return result;

    case ALLOCATE_EXPR:
      return
	allocate_dynamic_stack_space
	  (expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, TYPE_MODE (sizetype),
			EXPAND_NORMAL),
	   NULL_RTX, tree_low_cst (TREE_OPERAND (exp, 1), 1));

    case USE_EXPR:
      if (target != const0_rtx)
	gigi_abort (203);

      /* First write a volatile ASM_INPUT to prevent anything from being
	 moved.  */
      result = gen_rtx_ASM_INPUT (VOIDmode, "");
      MEM_VOLATILE_P (result) = 1;
      emit_insn (result);

      result = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode,
			    modifier);
      emit_insn (gen_rtx_USE (VOIDmode, result));
      return target;

    case GNAT_NOP_EXPR:
      return expand_expr (build1 (NOP_EXPR, type, TREE_OPERAND (exp, 0)),
			  target, tmode, modifier);

    case UNCONSTRAINED_ARRAY_REF:
      /* If we are evaluating just for side-effects, just evaluate our
	 operand.  Otherwise, abort since this code should never appear
	 in a tree to be evaluated (objects aren't unconstrained).  */
      if (target == const0_rtx || TREE_CODE (type) == VOID_TYPE)
	return expand_expr (TREE_OPERAND (exp, 0), const0_rtx,
			    VOIDmode, modifier);

      /* ... fall through ... */

    default:
      gigi_abort (201);
    }

  return expand_expr (new, target, tmode, modifier);
}

/* Transform a constant into a form that the language-independent code
   can handle.  */

static tree
gnat_expand_constant (exp)
     tree exp;
{
  /* If this is an unchecked conversion that does not change the size of the
     object and the object is not a CONSTRUCTOR return the operand since the
     underlying constant is still the same.  Otherwise, return our operand.  */
  if (TREE_CODE (exp) == UNCHECKED_CONVERT_EXPR
      && operand_equal_p (TYPE_SIZE_UNIT (TREE_TYPE (exp)),
			  TYPE_SIZE_UNIT (TREE_TYPE (TREE_OPERAND (exp, 0))),
			  1)
      && TREE_CODE (TREE_OPERAND (exp, 0)) != CONSTRUCTOR)
    return TREE_OPERAND (exp, 0);

  return exp;
}

/* Adjusts the RLI used to layout a record after all the fields have been
   added.  We only handle the packed case and cause it to use the alignment
   that will pad the record at the end.  */

static void
gnat_adjust_rli (rli)
     record_layout_info rli;
{
  if (TYPE_PACKED (rli->t))
    rli->record_align = rli->unpadded_align;
}

/* Make a TRANSFORM_EXPR to later expand GNAT_NODE into code.  */

tree
make_transform_expr (gnat_node)
     Node_Id gnat_node;
{
  tree gnu_result = build (TRANSFORM_EXPR, void_type_node);

  TREE_SIDE_EFFECTS (gnu_result) = 1;
  TREE_COMPLEXITY (gnu_result) = gnat_node;
  return gnu_result;
}

/* Update the setjmp buffer BUF with the current stack pointer.  We assume
   here that a __builtin_setjmp was done to BUF.  */

void
update_setjmp_buf (buf)
     tree buf;
{
  enum machine_mode sa_mode = Pmode;
  rtx stack_save;

#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    sa_mode = insn_data [(int) CODE_FOR_save_stack_nonlocal].operand[0].mode;
#endif

#ifdef STACK_SAVEAREA_MODE
  sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);
#endif

  stack_save
    = gen_rtx_MEM (sa_mode,
		   memory_address
		   (sa_mode,
		    plus_constant (expand_expr
				   (build_unary_op (ADDR_EXPR, NULL_TREE, buf),
				    NULL_RTX, VOIDmode, 0),
				   2 * GET_MODE_SIZE (Pmode))));

#ifdef HAVE_setjmp
  if (HAVE_setjmp)
    emit_insn (gen_setjmp ());
#endif

  emit_stack_save (SAVE_NONLOCAL, &stack_save, NULL_RTX);
}

/* See if DECL has an RTL that is indirect via a pseudo-register or a
   memory location and replace it with an indirect reference if so.
   This improves the debugger's ability to display the value.  */

void
adjust_decl_rtl (decl)
     tree decl;
{
  tree new_type;

  /* If this decl is already indirect, don't do anything.  This should
     mean that the decl cannot be indirect, but there's no point in
     adding an abort to check that.  */
  if (TREE_CODE (decl) != CONST_DECL
      && ! DECL_BY_REF_P (decl)
      && (GET_CODE (DECL_RTL (decl)) == MEM
	  && (GET_CODE (XEXP (DECL_RTL (decl), 0)) == MEM
	      || (GET_CODE (XEXP (DECL_RTL (decl), 0)) == REG
		  && (REGNO (XEXP (DECL_RTL (decl), 0))
		      > LAST_VIRTUAL_REGISTER))))
      /* We can't do this if the reference type's mode is not the same
	 as the current mode, which means this may not work on mixed 32/64
	 bit systems.  */
      && (new_type = build_reference_type (TREE_TYPE (decl))) != 0
      && TYPE_MODE (new_type) == GET_MODE (XEXP (DECL_RTL (decl), 0))
      /* If this is a PARM_DECL, we can only do it if DECL_INCOMING_RTL
	 is also an indirect and of the same mode and if the object is
	 readonly, the latter condition because we don't want to upset the
	 handling of CICO_LIST.  */
      && (TREE_CODE (decl) != PARM_DECL
	  || (GET_CODE (DECL_INCOMING_RTL (decl)) == MEM
	      && (TYPE_MODE (new_type)
		  == GET_MODE (XEXP (DECL_INCOMING_RTL (decl), 0)))
	      && TREE_READONLY (decl))))
    {
      new_type
	= build_qualified_type (new_type,
				(TYPE_QUALS (new_type) | TYPE_QUAL_CONST));

      DECL_POINTS_TO_READONLY_P (decl) = TREE_READONLY (decl);
      DECL_BY_REF_P (decl) = 1;
      SET_DECL_RTL (decl, XEXP (DECL_RTL (decl), 0));
      TREE_TYPE (decl) = new_type;
      DECL_MODE (decl) = TYPE_MODE (new_type);
      DECL_ALIGN (decl) = TYPE_ALIGN (new_type);
      DECL_SIZE (decl) = TYPE_SIZE (new_type);

      if (TREE_CODE (decl) == PARM_DECL)
	DECL_INCOMING_RTL (decl) = XEXP (DECL_INCOMING_RTL (decl), 0);

      /* If DECL_INITIAL was set, it should be updated to show that
	 the decl is initialized to the address of that thing.
	 Otherwise, just set it to the address of this decl.
	 It needs to be set so that GCC does not think the decl is
	 unused.  */
      DECL_INITIAL (decl)
	= build1 (ADDR_EXPR, new_type,
		  DECL_INITIAL (decl) != 0 ? DECL_INITIAL (decl) : decl);
    }
}

/* Record the current code position in GNAT_NODE.  */

void
record_code_position (gnat_node)
     Node_Id gnat_node;
{
  if (global_bindings_p ())
    {
      /* Make a dummy entry so multiple things at the same location don't
	 end up in the same place.  */
      add_pending_elaborations (NULL_TREE, NULL_TREE);
      save_gnu_tree (gnat_node, get_elaboration_location (), 1);
    }
  else
    /* Always emit another insn in case marking the last insn
       addressable needs some fixups and also for above reason.  */
    save_gnu_tree (gnat_node,
		   build (RTL_EXPR, void_type_node, NULL_TREE,
			  (tree) emit_note (0, NOTE_INSN_DELETED)),
		   1);
}

/* Insert the code for GNAT_NODE at the position saved for that node.  */

void
insert_code_for (gnat_node)
     Node_Id gnat_node;
{
  if (global_bindings_p ())
    {
      push_pending_elaborations ();
      gnat_to_code (gnat_node);
      Check_Elaboration_Code_Allowed (gnat_node);
      insert_elaboration_list (get_gnu_tree (gnat_node));
      pop_pending_elaborations ();
    }
  else
    {
      rtx insns;

      start_sequence ();
      mark_all_temps_used ();
      gnat_to_code (gnat_node);
      insns = get_insns ();
      end_sequence ();
      emit_insns_after (insns, RTL_EXPR_RTL (get_gnu_tree (gnat_node)));
    }
}

#if 0

/* Return the alignment for GNAT_TYPE.  */

unsigned int
get_type_alignment (gnat_type)
     Entity_Id gnat_type;
{
  return TYPE_ALIGN (gnat_to_gnu_type (gnat_type)) / BITS_PER_UNIT;
}
#endif

/* Get the alias set corresponding to a type or expression.  */

static HOST_WIDE_INT
gnat_get_alias_set (type)
     tree type;
{
  /* If this is a padding type, use the type of the first field.  */
  if (TREE_CODE (type) == RECORD_TYPE
      && TYPE_IS_PADDING_P (type))
    return get_alias_set (TREE_TYPE (TYPE_FIELDS (type)));

  return -1;
}

/* Set default attributes for functions.  We do nothing.  */

void
insert_default_attributes (decl)
     tree decl ATTRIBUTE_UNUSED;
{
}

/* GNU_TYPE is a type. Determine if it should be passed by reference by
   default.  */

int
default_pass_by_ref (gnu_type)
     tree gnu_type;
{
  CUMULATIVE_ARGS cum;

  INIT_CUMULATIVE_ARGS (cum, NULL_TREE, NULL_RTX, 0);

  /* We pass aggregates by reference if they are sufficiently large.  The
     choice of constant here is somewhat arbitrary.  We also pass by
     reference if the target machine would either pass or return by
     reference.  Strictly speaking, we need only check the return if this
     is an In Out parameter, but it's probably best to err on the side of
     passing more things by reference.  */
  return (0
#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
	  || FUNCTION_ARG_PASS_BY_REFERENCE (cum, TYPE_MODE (gnu_type),
					     gnu_type, 1)
#endif
	  || RETURN_IN_MEMORY (gnu_type)
	  || (AGGREGATE_TYPE_P (gnu_type)
	      && (! host_integerp (TYPE_SIZE (gnu_type), 1)
		  || 0 < compare_tree_int (TYPE_SIZE (gnu_type),
					   8 * TYPE_ALIGN (gnu_type)))));
}

/* GNU_TYPE is the type of a subprogram parameter.  Determine from the type if
   it should be passed by reference. */

int
must_pass_by_ref (gnu_type)
     tree gnu_type;
{
  /* We pass only unconstrained objects, those required by the language
     to be passed by reference, and objects of variable size.  The latter
     is more efficient, avoids problems with variable size temporaries,
     and does not produce compatibility problems with C, since C does
     not have such objects.  */
  return (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE
	  || (AGGREGATE_TYPE_P (gnu_type) && TYPE_BY_REFERENCE_P (gnu_type))
	  || (TYPE_SIZE (gnu_type) != 0
	      && TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST));
}

#if defined(MIPS_DEBUGGING_INFO) && defined(DWARF2_DEBUGGING_INFO)

/* Convert NAME, which is possibly an Ada name, back to standard Ada
   notation for SGI Workshop.  */

static char *
convert_ada_name_to_qualified_name (name)
     char *name;
{
  int len = strlen (name);
  char *new_name = xstrdup (name);
  char *buf;
  int i, start;
  char *qual_name_suffix = 0;
  char *p;

  if (len <= 3 || use_gnu_debug_info_extensions)
    {
      free (new_name);
      return name;
    }

  /* Find the position of the first "__" after the first character of
     NAME.  This is the same as calling strstr except that we can't assume
     the host has that function. We start after the first character so
     we don't eliminate leading "__": these are emitted only by C
     programs and are not qualified names */
  for (p = (char *) index (&name[1], '_'); p != 0;
       p = (char *) index (p+1, '_'))
    if (p[1] == '_')
      {
	qual_name_suffix = p;
	break;
      }

  if (qual_name_suffix == 0)
    {
      free (new_name);
      return name;
    }

  start = qual_name_suffix - name;
  buf = new_name + start;

  for (i = start; i < len; i++)
    {
      if (name[i] == '_' && name[i + 1] == '_')
	{
	  if (islower (name[i + 2]))
	    {
	      *buf++ = '.';
	      *buf++ = name[i + 2];
	      i += 2;
	    }
	  else if (name[i + 2] == '_' && islower (name[i + 3]))
	    { 
	      /* convert foo___c___XVN to foo.c___XVN */
	      *buf++ = '.';
	      *buf++ = name[i + 3];
	      i += 3;
	    }
	  else if (name[i + 2] == 'T')
	    {
	      /* convert foo__TtypeS to foo.__TTypeS */
	      *buf++ = '.';
	      *buf++ = '_';
	      *buf++ = '_';
	      *buf++ = 'T';
	      i += 3;
	    }
	  else
	    *buf++ = name[i];
	}
      else
	*buf++ = name[i];
    }

  *buf = 0;
  return new_name;
}
#endif

/* Emit a label UNITNAME_LABEL and specify that it is part of source
   file FILENAME.  If this is being written for SGI's Workshop
   debugger, and we are writing Dwarf2 debugging information, add
   additional debug info.  */

void
emit_unit_label (unitname_label, filename)
     char *unitname_label;
     char *filename ATTRIBUTE_UNUSED;
{
  ASM_GLOBALIZE_LABEL (asm_out_file, unitname_label);
  ASM_OUTPUT_LABEL (asm_out_file, unitname_label); 
}
