/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 M I S C                                  *
 *                                                                          *
 *                           C Implementation File                          *
 *                                                                          *
 *                                                                          *
 *          Copyright (C) 1992-2002 Free Software Foundation, Inc.          *
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
#include "libfuncs.h"
#include "ggc.h"
#include "flags.h"
#include "debug.h"
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
#include "adadecode.h"

extern FILE *asm_out_file;
extern int save_argc;
extern char **save_argv;

static const char *gnat_init		PARAMS ((const char *));
static void gnat_init_options		PARAMS ((void));
static int gnat_decode_option		PARAMS ((int, char **));
static HOST_WIDE_INT gnat_get_alias_set	PARAMS ((tree));
static void gnat_print_decl		PARAMS ((FILE *, tree, int));
static void gnat_print_type		PARAMS ((FILE *, tree, int));
static const char *gnat_printable_name	PARAMS  ((tree, int));
static tree gnat_eh_runtime_type	PARAMS ((tree));
static int gnat_eh_type_covers		PARAMS ((tree, tree));
static void gnat_parse_file		PARAMS ((int));
static rtx gnat_expand_expr		PARAMS ((tree, rtx, enum machine_mode,
						 int));

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
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE		gnat_parse_file
#undef LANG_HOOKS_HONOR_READONLY
#define LANG_HOOKS_HONOR_READONLY	1
#undef LANG_HOOKS_FINISH_INCOMPLETE_DECL
#define LANG_HOOKS_FINISH_INCOMPLETE_DECL gnat_finish_incomplete_decl
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET	gnat_get_alias_set
#undef LANG_HOOKS_EXPAND_EXPR
#define LANG_HOOKS_EXPAND_EXPR		gnat_expand_expr
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE	gnat_mark_addressable
#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION gnat_truthvalue_conversion
#undef LANG_HOOKS_PRINT_DECL
#define LANG_HOOKS_PRINT_DECL		gnat_print_decl
#undef LANG_HOOKS_PRINT_TYPE
#define LANG_HOOKS_PRINT_TYPE		gnat_print_type
#undef LANG_HOOKS_DECL_PRINTABLE_NAME
#define LANG_HOOKS_DECL_PRINTABLE_NAME	gnat_printable_name
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE	gnat_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE	gnat_type_for_size
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE		gnat_signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE	gnat_unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE gnat_signed_or_unsigned_type

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Tables describing GCC tree codes used only by GNAT.  

   Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See cp-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const char tree_code_type[] = {
#include "tree.def"
  'x',
#include "ada-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
  0,
#include "ada-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *const tree_code_name[] = {
#include "tree.def"
  "@@dummy",
#include "ada-tree.def"
};
#undef DEFTREECODE

/* gnat standard argc argv */

extern int gnat_argc;
extern char **gnat_argv;

static void internal_error_function	PARAMS ((const char *, va_list *));
static void gnat_adjust_rli		PARAMS ((record_layout_info));

/* Declare functions we use as part of startup.  */
extern void __gnat_initialize	PARAMS((void));
extern void adainit		PARAMS((void));
extern void _ada_gnat1drv	PARAMS((void));

/* The parser for the language.  For us, we process the GNAT tree.  */

static void
gnat_parse_file (set_yydebug)
     int set_yydebug ATTRIBUTE_UNUSED;
{
  /* call the target specific initializations */
  __gnat_initialize();

  /* Call the front-end elaboration procedures */
  adainit ();

  immediate_size_expand = 1;

  /* Call the front end */
  _ada_gnat1drv ();
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
      char *q = xstrdup (p);

      warning ("`-gnat' misspelled as `-gant'");
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

  /* Handle the --RTS switch.  The real option we get is -fRTS. This
     modification is done by the driver program.  */
  if (!strncmp (p, "-fRTS", 5))
    {
      gnat_argv[gnat_argc] = p;
      gnat_argc ++;
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
  gnat_argv[0] = save_argv[0];     /* name of the command */ 
  gnat_argc = 1;
}

/* Here is the function to handle the compiler error processing in GCC.  */

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

  gnat_init_decl_processing ();

  /* Add the input filename as the last argument.  */
  gnat_argv[gnat_argc] = (char *) filename;
  gnat_argc++;
  gnat_argv[gnat_argc] = 0;

  global_dc->internal_error = &internal_error_function;

  /* Show that REFERENCE_TYPEs are internal and should be Pmode.  */
  internal_reference_types ();

  set_lang_adjust_rli (gnat_adjust_rli);

  if (filename == 0)
    filename = "";

  return filename;
}

/* If we are using the GCC mechanism for to process exception handling, we
   have to register the personality routine for Ada and to initialize
   various language dependent hooks.  */

void
gnat_init_gcc_eh ()
{
  /* We shouldn't do anything if the No_Exceptions_Handler pragma is set,
     though. This could for instance lead to the emission of tables with
     references to symbols (such as the Ada eh personality routine) within
     libraries we won't link against.  */
  if (No_Exception_Handlers_Set ())
    return;

  eh_personality_libfunc = init_one_libfunc ("__gnat_eh_personality");
  lang_eh_type_covers = gnat_eh_type_covers;
  lang_eh_runtime_type = gnat_eh_runtime_type;
  flag_exceptions = 1;

  init_eh ();
#ifdef DWARF2_UNWIND_INFO
  if (dwarf2out_do_frame ())
    dwarf2out_frame_init ();
#endif
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

static const char *
gnat_printable_name (decl, verbosity)
     tree decl;
     int verbosity ATTRIBUTE_UNUSED;
{
  const char *coded_name = IDENTIFIER_POINTER (DECL_NAME (decl));
  char *ada_name = (char *) ggc_alloc (strlen (coded_name) * 2 + 60);

  __gnat_decode (coded_name, ada_name, 0);

  return (const char *) ada_name;
}

/* Expands GNAT-specific GCC tree nodes.  The only ones we support
   here are TRANSFORM_EXPR, ALLOCATE_EXPR, USE_EXPR and NULL_EXPR.  */

static rtx
gnat_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     int modifier;  /* Actually an enum expand_modifier.  */
{
  tree type = TREE_TYPE (exp);
  tree new;
  rtx result;

  /* Update EXP to be the new expression to expand.  */

  switch (TREE_CODE (exp))
    {
    case TRANSFORM_EXPR:
      gnat_to_code (TREE_COMPLEXITY (exp));
      return const0_rtx;
      break;

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

/* Adjusts the RLI used to layout a record after all the fields have been
   added.  We only handle the packed case and cause it to use the alignment
   that will pad the record at the end.  */

static void
gnat_adjust_rli (rli)
     record_layout_info rli;
{
  unsigned int record_align = rli->unpadded_align;
  tree field;

  /* If any fields have variable size, we need to force the record to be at
     least as aligned as the alignment of that type.  */
  for (field = TYPE_FIELDS (rli->t); field; field = TREE_CHAIN (field))
    if (TREE_CODE (DECL_SIZE_UNIT (field)) != INTEGER_CST)
      record_align = MAX (record_align, DECL_ALIGN (field));

  if (TYPE_PACKED (rli->t))
    rli->record_align = record_align;
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
    sa_mode = insn_data[(int) CODE_FOR_save_stack_nonlocal].operand[0].mode;
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

/* These routines are used in conjunction with GCC exception handling.  */

/* Map compile-time to run-time tree for GCC exception handling scheme.  */

static tree
gnat_eh_runtime_type (type)
     tree type;
{
  return type;
}

/* Return true if type A catches type B. Callback for flow analysis from
   the exception handling part of the back-end.  */

static int
gnat_eh_type_covers (a, b)
     tree a, b;
{
  /* a catches b if they represent the same exception id or if a
     is an "others". 

     ??? integer_zero_node for "others" is hardwired in too many places
     currently.  */
  return (a == b || a == integer_zero_node);
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

      do_pending_stack_adjust ();
      start_sequence ();
      mark_all_temps_used ();
      gnat_to_code (gnat_node);
      do_pending_stack_adjust ();
      insns = get_insns ();
      end_sequence ();
      emit_insn_after (insns, RTL_EXPR_RTL (get_gnu_tree (gnat_node)));
    }
}

/* Get the alias set corresponding to a type or expression.  */

static HOST_WIDE_INT
gnat_get_alias_set (type)
     tree type;
{
  /* If this is a padding type, use the type of the first field.  */
  if (TREE_CODE (type) == RECORD_TYPE
      && TYPE_IS_PADDING_P (type))
    return get_alias_set (TREE_TYPE (TYPE_FIELDS (type)));

  /* If the type is an unconstrained array, use the type of the
     self-referential array we make.  */
  else if (TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
    return
      get_alias_set (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (type)))));


  return -1;
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

/* This function returns the version of GCC being used.  Here it's GCC 3.  */

int
gcc_version ()
{
  return 3;
}
