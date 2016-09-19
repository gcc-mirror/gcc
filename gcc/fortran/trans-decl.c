/* Backend function setup
   Copyright (C) 2002-2016 Free Software Foundation, Inc.
   Contributed by Paul Brook

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* trans-decl.c -- Handling of backend function and variable decls, etc */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "tree.h"
#include "gfortran.h"
#include "gimple-expr.h"	/* For create_tmp_var_raw.  */
#include "trans.h"
#include "stringpool.h"
#include "cgraph.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "varasm.h"
#include "attribs.h"
#include "tree-dump.h"
#include "toplev.h"	/* For announce_function.  */
#include "debug.h"
#include "constructor.h"
#include "trans-types.h"
#include "trans-array.h"
#include "trans-const.h"
/* Only for gfc_trans_code.  Shouldn't need to include this.  */
#include "trans-stmt.h"
#include "gomp-constants.h"
#include "gimplify.h"

#define MAX_LABEL_VALUE 99999


/* Holds the result of the function if no result variable specified.  */

static GTY(()) tree current_fake_result_decl;
static GTY(()) tree parent_fake_result_decl;


/* Holds the variable DECLs for the current function.  */

static GTY(()) tree saved_function_decls;
static GTY(()) tree saved_parent_function_decls;

static hash_set<tree> *nonlocal_dummy_decl_pset;
static GTY(()) tree nonlocal_dummy_decls;

/* Holds the variable DECLs that are locals.  */

static GTY(()) tree saved_local_decls;

/* The namespace of the module we're currently generating.  Only used while
   outputting decls for module variables.  Do not rely on this being set.  */

static gfc_namespace *module_namespace;

/* The currently processed procedure symbol.  */
static gfc_symbol* current_procedure_symbol = NULL;

/* The currently processed module.  */
static struct module_htab_entry *cur_module;

/* With -fcoarray=lib: For generating the registering call
   of static coarrays.  */
static bool has_coarray_vars;
static stmtblock_t caf_init_block;


/* List of static constructor functions.  */

tree gfc_static_ctors;


/* Whether we've seen a symbol from an IEEE module in the namespace.  */
static int seen_ieee_symbol;

/* Function declarations for builtin library functions.  */

tree gfor_fndecl_pause_numeric;
tree gfor_fndecl_pause_string;
tree gfor_fndecl_stop_numeric;
tree gfor_fndecl_stop_numeric_f08;
tree gfor_fndecl_stop_string;
tree gfor_fndecl_error_stop_numeric;
tree gfor_fndecl_error_stop_string;
tree gfor_fndecl_runtime_error;
tree gfor_fndecl_runtime_error_at;
tree gfor_fndecl_runtime_warning_at;
tree gfor_fndecl_os_error;
tree gfor_fndecl_generate_error;
tree gfor_fndecl_set_args;
tree gfor_fndecl_set_fpe;
tree gfor_fndecl_set_options;
tree gfor_fndecl_set_convert;
tree gfor_fndecl_set_record_marker;
tree gfor_fndecl_set_max_subrecord_length;
tree gfor_fndecl_ctime;
tree gfor_fndecl_fdate;
tree gfor_fndecl_ttynam;
tree gfor_fndecl_in_pack;
tree gfor_fndecl_in_unpack;
tree gfor_fndecl_associated;
tree gfor_fndecl_system_clock4;
tree gfor_fndecl_system_clock8;
tree gfor_fndecl_ieee_procedure_entry;
tree gfor_fndecl_ieee_procedure_exit;


/* Coarray run-time library function decls.  */
tree gfor_fndecl_caf_init;
tree gfor_fndecl_caf_finalize;
tree gfor_fndecl_caf_this_image;
tree gfor_fndecl_caf_num_images;
tree gfor_fndecl_caf_register;
tree gfor_fndecl_caf_deregister;
tree gfor_fndecl_caf_get;
tree gfor_fndecl_caf_send;
tree gfor_fndecl_caf_sendget;
tree gfor_fndecl_caf_get_by_ref;
tree gfor_fndecl_caf_send_by_ref;
tree gfor_fndecl_caf_sendget_by_ref;
tree gfor_fndecl_caf_sync_all;
tree gfor_fndecl_caf_sync_memory;
tree gfor_fndecl_caf_sync_images;
tree gfor_fndecl_caf_stop_str;
tree gfor_fndecl_caf_stop_numeric;
tree gfor_fndecl_caf_error_stop;
tree gfor_fndecl_caf_error_stop_str;
tree gfor_fndecl_caf_atomic_def;
tree gfor_fndecl_caf_atomic_ref;
tree gfor_fndecl_caf_atomic_cas;
tree gfor_fndecl_caf_atomic_op;
tree gfor_fndecl_caf_lock;
tree gfor_fndecl_caf_unlock;
tree gfor_fndecl_caf_event_post;
tree gfor_fndecl_caf_event_wait;
tree gfor_fndecl_caf_event_query;
tree gfor_fndecl_co_broadcast;
tree gfor_fndecl_co_max;
tree gfor_fndecl_co_min;
tree gfor_fndecl_co_reduce;
tree gfor_fndecl_co_sum;


/* Math functions.  Many other math functions are handled in
   trans-intrinsic.c.  */

gfc_powdecl_list gfor_fndecl_math_powi[4][3];
tree gfor_fndecl_math_ishftc4;
tree gfor_fndecl_math_ishftc8;
tree gfor_fndecl_math_ishftc16;


/* String functions.  */

tree gfor_fndecl_compare_string;
tree gfor_fndecl_concat_string;
tree gfor_fndecl_string_len_trim;
tree gfor_fndecl_string_index;
tree gfor_fndecl_string_scan;
tree gfor_fndecl_string_verify;
tree gfor_fndecl_string_trim;
tree gfor_fndecl_string_minmax;
tree gfor_fndecl_adjustl;
tree gfor_fndecl_adjustr;
tree gfor_fndecl_select_string;
tree gfor_fndecl_compare_string_char4;
tree gfor_fndecl_concat_string_char4;
tree gfor_fndecl_string_len_trim_char4;
tree gfor_fndecl_string_index_char4;
tree gfor_fndecl_string_scan_char4;
tree gfor_fndecl_string_verify_char4;
tree gfor_fndecl_string_trim_char4;
tree gfor_fndecl_string_minmax_char4;
tree gfor_fndecl_adjustl_char4;
tree gfor_fndecl_adjustr_char4;
tree gfor_fndecl_select_string_char4;


/* Conversion between character kinds.  */
tree gfor_fndecl_convert_char1_to_char4;
tree gfor_fndecl_convert_char4_to_char1;


/* Other misc. runtime library functions.  */
tree gfor_fndecl_size0;
tree gfor_fndecl_size1;
tree gfor_fndecl_iargc;

/* Intrinsic functions implemented in Fortran.  */
tree gfor_fndecl_sc_kind;
tree gfor_fndecl_si_kind;
tree gfor_fndecl_sr_kind;

/* BLAS gemm functions.  */
tree gfor_fndecl_sgemm;
tree gfor_fndecl_dgemm;
tree gfor_fndecl_cgemm;
tree gfor_fndecl_zgemm;


static void
gfc_add_decl_to_parent_function (tree decl)
{
  gcc_assert (decl);
  DECL_CONTEXT (decl) = DECL_CONTEXT (current_function_decl);
  DECL_NONLOCAL (decl) = 1;
  DECL_CHAIN (decl) = saved_parent_function_decls;
  saved_parent_function_decls = decl;
}

void
gfc_add_decl_to_function (tree decl)
{
  gcc_assert (decl);
  TREE_USED (decl) = 1;
  DECL_CONTEXT (decl) = current_function_decl;
  DECL_CHAIN (decl) = saved_function_decls;
  saved_function_decls = decl;
}

static void
add_decl_as_local (tree decl)
{
  gcc_assert (decl);
  TREE_USED (decl) = 1;
  DECL_CONTEXT (decl) = current_function_decl;
  DECL_CHAIN (decl) = saved_local_decls;
  saved_local_decls = decl;
}


/* Build a  backend label declaration.  Set TREE_USED for named labels.
   The context of the label is always the current_function_decl.  All
   labels are marked artificial.  */

tree
gfc_build_label_decl (tree label_id)
{
  /* 2^32 temporaries should be enough.  */
  static unsigned int tmp_num = 1;
  tree label_decl;
  char *label_name;

  if (label_id == NULL_TREE)
    {
      /* Build an internal label name.  */
      ASM_FORMAT_PRIVATE_NAME (label_name, "L", tmp_num++);
      label_id = get_identifier (label_name);
    }
  else
    label_name = NULL;

  /* Build the LABEL_DECL node. Labels have no type.  */
  label_decl = build_decl (input_location,
			   LABEL_DECL, label_id, void_type_node);
  DECL_CONTEXT (label_decl) = current_function_decl;
  DECL_MODE (label_decl) = VOIDmode;

  /* We always define the label as used, even if the original source
     file never references the label.  We don't want all kinds of
     spurious warnings for old-style Fortran code with too many
     labels.  */
  TREE_USED (label_decl) = 1;

  DECL_ARTIFICIAL (label_decl) = 1;
  return label_decl;
}


/* Set the backend source location of a decl.  */

void
gfc_set_decl_location (tree decl, locus * loc)
{
  DECL_SOURCE_LOCATION (decl) = loc->lb->location;
}


/* Return the backend label declaration for a given label structure,
   or create it if it doesn't exist yet.  */

tree
gfc_get_label_decl (gfc_st_label * lp)
{
  if (lp->backend_decl)
    return lp->backend_decl;
  else
    {
      char label_name[GFC_MAX_SYMBOL_LEN + 1];
      tree label_decl;

      /* Validate the label declaration from the front end.  */
      gcc_assert (lp != NULL && lp->value <= MAX_LABEL_VALUE);

      /* Build a mangled name for the label.  */
      sprintf (label_name, "__label_%.6d", lp->value);

      /* Build the LABEL_DECL node.  */
      label_decl = gfc_build_label_decl (get_identifier (label_name));

      /* Tell the debugger where the label came from.  */
      if (lp->value <= MAX_LABEL_VALUE)	/* An internal label.  */
	gfc_set_decl_location (label_decl, &lp->where);
      else
	DECL_ARTIFICIAL (label_decl) = 1;

      /* Store the label in the label list and return the LABEL_DECL.  */
      lp->backend_decl = label_decl;
      return label_decl;
    }
}


/* Convert a gfc_symbol to an identifier of the same name.  */

static tree
gfc_sym_identifier (gfc_symbol * sym)
{
  if (sym->attr.is_main_program && strcmp (sym->name, "main") == 0)
    return (get_identifier ("MAIN__"));
  else
    return (get_identifier (sym->name));
}


/* Construct mangled name from symbol name.  */

static tree
gfc_sym_mangled_identifier (gfc_symbol * sym)
{
  char name[GFC_MAX_MANGLED_SYMBOL_LEN + 1];

  /* Prevent the mangling of identifiers that have an assigned
     binding label (mainly those that are bind(c)).  */
  if (sym->attr.is_bind_c == 1 && sym->binding_label)
    return get_identifier (sym->binding_label);

  if (sym->module == NULL)
    return gfc_sym_identifier (sym);
  else
    {
      snprintf (name, sizeof name, "__%s_MOD_%s", sym->module, sym->name);
      return get_identifier (name);
    }
}


/* Construct mangled function name from symbol name.  */

static tree
gfc_sym_mangled_function_id (gfc_symbol * sym)
{
  int has_underscore;
  char name[GFC_MAX_MANGLED_SYMBOL_LEN + 1];

  /* It may be possible to simply use the binding label if it's
     provided, and remove the other checks.  Then we could use it
     for other things if we wished.  */
  if ((sym->attr.is_bind_c == 1 || sym->attr.is_iso_c == 1) &&
      sym->binding_label)
    /* use the binding label rather than the mangled name */
    return get_identifier (sym->binding_label);

  if ((sym->module == NULL || sym->attr.proc == PROC_EXTERNAL
      || (sym->module != NULL && (sym->attr.external
	    || sym->attr.if_source == IFSRC_IFBODY)))
      && !sym->attr.module_procedure)
    {
      /* Main program is mangled into MAIN__.  */
      if (sym->attr.is_main_program)
	return get_identifier ("MAIN__");

      /* Intrinsic procedures are never mangled.  */
      if (sym->attr.proc == PROC_INTRINSIC)
	return get_identifier (sym->name);

      if (flag_underscoring)
	{
	  has_underscore = strchr (sym->name, '_') != 0;
	  if (flag_second_underscore && has_underscore)
	    snprintf (name, sizeof name, "%s__", sym->name);
	  else
	    snprintf (name, sizeof name, "%s_", sym->name);
	  return get_identifier (name);
	}
      else
	return get_identifier (sym->name);
    }
  else
    {
      snprintf (name, sizeof name, "__%s_MOD_%s", sym->module, sym->name);
      return get_identifier (name);
    }
}


void
gfc_set_decl_assembler_name (tree decl, tree name)
{
  tree target_mangled = targetm.mangle_decl_assembler_name (decl, name);
  SET_DECL_ASSEMBLER_NAME (decl, target_mangled);
}


/* Returns true if a variable of specified size should go on the stack.  */

int
gfc_can_put_var_on_stack (tree size)
{
  unsigned HOST_WIDE_INT low;

  if (!INTEGER_CST_P (size))
    return 0;

  if (flag_max_stack_var_size < 0)
    return 1;

  if (!tree_fits_uhwi_p (size))
    return 0;

  low = TREE_INT_CST_LOW (size);
  if (low > (unsigned HOST_WIDE_INT) flag_max_stack_var_size)
    return 0;

/* TODO: Set a per-function stack size limit.  */

  return 1;
}


/* gfc_finish_cray_pointee sets DECL_VALUE_EXPR for a Cray pointee to
   an expression involving its corresponding pointer.  There are
   2 cases; one for variable size arrays, and one for everything else,
   because variable-sized arrays require one fewer level of
   indirection.  */

static void
gfc_finish_cray_pointee (tree decl, gfc_symbol *sym)
{
  tree ptr_decl = gfc_get_symbol_decl (sym->cp_pointer);
  tree value;

  /* Parameters need to be dereferenced.  */
  if (sym->cp_pointer->attr.dummy)
    ptr_decl = build_fold_indirect_ref_loc (input_location,
					ptr_decl);

  /* Check to see if we're dealing with a variable-sized array.  */
  if (sym->attr.dimension
      && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE)
    {
      /* These decls will be dereferenced later, so we don't dereference
	 them here.  */
      value = convert (TREE_TYPE (decl), ptr_decl);
    }
  else
    {
      ptr_decl = convert (build_pointer_type (TREE_TYPE (decl)),
			  ptr_decl);
      value = build_fold_indirect_ref_loc (input_location,
				       ptr_decl);
    }

  SET_DECL_VALUE_EXPR (decl, value);
  DECL_HAS_VALUE_EXPR_P (decl) = 1;
  GFC_DECL_CRAY_POINTEE (decl) = 1;
}


/* Finish processing of a declaration without an initial value.  */

static void
gfc_finish_decl (tree decl)
{
  gcc_assert (TREE_CODE (decl) == PARM_DECL
	      || DECL_INITIAL (decl) == NULL_TREE);

  if (TREE_CODE (decl) != VAR_DECL)
    return;

  if (DECL_SIZE (decl) == NULL_TREE
      && TYPE_SIZE (TREE_TYPE (decl)) != NULL_TREE)
    layout_decl (decl, 0);

  /* A few consistency checks.  */
  /* A static variable with an incomplete type is an error if it is
     initialized. Also if it is not file scope. Otherwise, let it
     through, but if it is not `extern' then it may cause an error
     message later.  */
  /* An automatic variable with an incomplete type is an error.  */

  /* We should know the storage size.  */
  gcc_assert (DECL_SIZE (decl) != NULL_TREE
	      || (TREE_STATIC (decl)
		  ? (!DECL_INITIAL (decl) || !DECL_CONTEXT (decl))
		  : DECL_EXTERNAL (decl)));

  /* The storage size should be constant.  */
  gcc_assert ((!DECL_EXTERNAL (decl) && !TREE_STATIC (decl))
	      || !DECL_SIZE (decl)
	      || TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST);
}


/* Handle setting of GFC_DECL_SCALAR* on DECL.  */

void
gfc_finish_decl_attrs (tree decl, symbol_attribute *attr)
{
  if (!attr->dimension && !attr->codimension)
    {
      /* Handle scalar allocatable variables.  */
      if (attr->allocatable)
	{
	  gfc_allocate_lang_decl (decl);
	  GFC_DECL_SCALAR_ALLOCATABLE (decl) = 1;
	}
      /* Handle scalar pointer variables.  */
      if (attr->pointer)
	{
	  gfc_allocate_lang_decl (decl);
	  GFC_DECL_SCALAR_POINTER (decl) = 1;
	}
    }
}


/* Apply symbol attributes to a variable, and add it to the function scope.  */

static void
gfc_finish_var_decl (tree decl, gfc_symbol * sym)
{
  tree new_type;

  /* Set DECL_VALUE_EXPR for Cray Pointees.  */
  if (sym->attr.cray_pointee)
    gfc_finish_cray_pointee (decl, sym);

  /* TREE_ADDRESSABLE means the address of this variable is actually needed.
     This is the equivalent of the TARGET variables.
     We also need to set this if the variable is passed by reference in a
     CALL statement.  */
  if (sym->attr.target)
    TREE_ADDRESSABLE (decl) = 1;

  /* If it wasn't used we wouldn't be getting it.  */
  TREE_USED (decl) = 1;

  if (sym->attr.flavor == FL_PARAMETER
      && (sym->attr.dimension || sym->ts.type == BT_DERIVED))
    TREE_READONLY (decl) = 1;

  /* Chain this decl to the pending declarations.  Don't do pushdecl()
     because this would add them to the current scope rather than the
     function scope.  */
  if (current_function_decl != NULL_TREE)
    {
      if (sym->ns->proc_name->backend_decl == current_function_decl
	  || sym->result == sym)
	gfc_add_decl_to_function (decl);
      else if (sym->ns->proc_name->attr.flavor == FL_LABEL)
	/* This is a BLOCK construct.  */
	add_decl_as_local (decl);
      else
	gfc_add_decl_to_parent_function (decl);
    }

  if (sym->attr.cray_pointee)
    return;

  if(sym->attr.is_bind_c == 1 && sym->binding_label)
    {
      /* We need to put variables that are bind(c) into the common
	 segment of the object file, because this is what C would do.
	 gfortran would typically put them in either the BSS or
	 initialized data segments, and only mark them as common if
	 they were part of common blocks.  However, if they are not put
	 into common space, then C cannot initialize global Fortran
	 variables that it interoperates with and the draft says that
	 either Fortran or C should be able to initialize it (but not
	 both, of course.) (J3/04-007, section 15.3).  */
      TREE_PUBLIC(decl) = 1;
      DECL_COMMON(decl) = 1;
      if (sym->attr.access == ACCESS_PRIVATE && !sym->attr.public_used)
	{
	  DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
	  DECL_VISIBILITY_SPECIFIED (decl) = true;
	}
    }

  /* If a variable is USE associated, it's always external.  */
  if (sym->attr.use_assoc || sym->attr.used_in_submodule)
    {
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
    }
  else if (sym->module && !sym->attr.result && !sym->attr.dummy)
    {
      /* TODO: Don't set sym->module for result or dummy variables.  */
      gcc_assert (current_function_decl == NULL_TREE || sym->result == sym);

      TREE_PUBLIC (decl) = 1;
      TREE_STATIC (decl) = 1;
      if (sym->attr.access == ACCESS_PRIVATE && !sym->attr.public_used)
	{
	  DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
	  DECL_VISIBILITY_SPECIFIED (decl) = true;
	}
    }

  /* Derived types are a bit peculiar because of the possibility of
     a default initializer; this must be applied each time the variable
     comes into scope it therefore need not be static.  These variables
     are SAVE_NONE but have an initializer.  Otherwise explicitly
     initialized variables are SAVE_IMPLICIT and explicitly saved are
     SAVE_EXPLICIT.  */
  if (!sym->attr.use_assoc
	&& (sym->attr.save != SAVE_NONE || sym->attr.data
	    || (sym->value && sym->ns->proc_name->attr.is_main_program)
	    || (flag_coarray == GFC_FCOARRAY_LIB
		&& sym->attr.codimension && !sym->attr.allocatable)))
    TREE_STATIC (decl) = 1;

  /* If derived-type variables with DTIO procedures are not made static
     some bits of code referencing them get optimized away.
     TODO Understand why this is so and fix it.  */
  if (!sym->attr.use_assoc
      && ((sym->ts.type == BT_DERIVED
           && sym->ts.u.derived->attr.has_dtio_procs)
	  || (sym->ts.type == BT_CLASS
	      && CLASS_DATA (sym)->ts.u.derived->attr.has_dtio_procs)))
    TREE_STATIC (decl) = 1;

  if (sym->attr.volatile_)
    {
      TREE_THIS_VOLATILE (decl) = 1;
      TREE_SIDE_EFFECTS (decl) = 1;
      new_type = build_qualified_type (TREE_TYPE (decl), TYPE_QUAL_VOLATILE);
      TREE_TYPE (decl) = new_type;
    }

  /* Keep variables larger than max-stack-var-size off stack.  */
  if (!sym->ns->proc_name->attr.recursive
      && INTEGER_CST_P (DECL_SIZE_UNIT (decl))
      && !gfc_can_put_var_on_stack (DECL_SIZE_UNIT (decl))
	 /* Put variable length auto array pointers always into stack.  */
      && (TREE_CODE (TREE_TYPE (decl)) != POINTER_TYPE
	  || sym->attr.dimension == 0
	  || sym->as->type != AS_EXPLICIT
	  || sym->attr.pointer
	  || sym->attr.allocatable)
      && !DECL_ARTIFICIAL (decl))
    {
      TREE_STATIC (decl) = 1;

      /* Because the size of this variable isn't known until now, we may have
         greedily added an initializer to this variable (in build_init_assign)
         even though the max-stack-var-size indicates the variable should be
         static. Therefore we rip out the automatic initializer here and
         replace it with a static one.  */
      gfc_symtree *st = gfc_find_symtree (sym->ns->sym_root, sym->name);
      gfc_code *prev = NULL;
      gfc_code *code = sym->ns->code;
      while (code && code->op == EXEC_INIT_ASSIGN)
        {
          /* Look for an initializer meant for this symbol.  */
          if (code->expr1->symtree == st)
            {
              if (prev)
                prev->next = code->next;
              else
                sym->ns->code = code->next;

              break;
            }

          prev = code;
          code = code->next;
        }
      if (code && code->op == EXEC_INIT_ASSIGN)
        {
          /* Keep the init expression for a static initializer.  */
          sym->value = code->expr2;
          /* Cleanup the defunct code object, without freeing the init expr.  */
          code->expr2 = NULL;
          gfc_free_statement (code);
          free (code);
        }
    }

  /* Handle threadprivate variables.  */
  if (sym->attr.threadprivate
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
    set_decl_tls_model (decl, decl_default_tls_model (decl));

  gfc_finish_decl_attrs (decl, &sym->attr);
}


/* Allocate the lang-specific part of a decl.  */

void
gfc_allocate_lang_decl (tree decl)
{
  if (DECL_LANG_SPECIFIC (decl) == NULL)
    DECL_LANG_SPECIFIC (decl) = ggc_cleared_alloc<struct lang_decl> ();
}

/* Remember a symbol to generate initialization/cleanup code at function
   entry/exit.  */

static void
gfc_defer_symbol_init (gfc_symbol * sym)
{
  gfc_symbol *p;
  gfc_symbol *last;
  gfc_symbol *head;

  /* Don't add a symbol twice.  */
  if (sym->tlink)
    return;

  last = head = sym->ns->proc_name;
  p = last->tlink;

  /* Make sure that setup code for dummy variables which are used in the
     setup of other variables is generated first.  */
  if (sym->attr.dummy)
    {
      /* Find the first dummy arg seen after us, or the first non-dummy arg.
         This is a circular list, so don't go past the head.  */
      while (p != head
             && (!p->attr.dummy || p->dummy_order > sym->dummy_order))
        {
          last = p;
          p = p->tlink;
        }
    }
  /* Insert in between last and p.  */
  last->tlink = sym;
  sym->tlink = p;
}


/* Used in gfc_get_symbol_decl and gfc_get_derived_type to obtain the
   backend_decl for a module symbol, if it all ready exists.  If the
   module gsymbol does not exist, it is created.  If the symbol does
   not exist, it is added to the gsymbol namespace.  Returns true if
   an existing backend_decl is found.  */

bool
gfc_get_module_backend_decl (gfc_symbol *sym)
{
  gfc_gsymbol *gsym;
  gfc_symbol *s;
  gfc_symtree *st;

  gsym =  gfc_find_gsymbol (gfc_gsym_root, sym->module);

  if (!gsym || (gsym->ns && gsym->type == GSYM_MODULE))
    {
      st = NULL;
      s = NULL;

      /* Check for a symbol with the same name. */
      if (gsym)
	gfc_find_symbol (sym->name, gsym->ns, 0, &s);

      if (!s)
	{
	  if (!gsym)
	    {
	      gsym = gfc_get_gsymbol (sym->module);
	      gsym->type = GSYM_MODULE;
	      gsym->ns = gfc_get_namespace (NULL, 0);
	    }

	  st = gfc_new_symtree (&gsym->ns->sym_root, sym->name);
	  st->n.sym = sym;
	  sym->refs++;
	}
      else if (gfc_fl_struct (sym->attr.flavor))
	{
	  if (s && s->attr.flavor == FL_PROCEDURE)
	    {
	      gfc_interface *intr;
	      gcc_assert (s->attr.generic);
	      for (intr = s->generic; intr; intr = intr->next)
		if (gfc_fl_struct (intr->sym->attr.flavor))
		  {
		    s = intr->sym;
		    break;
		  }
    	    }

          /* Normally we can assume that s is a derived-type symbol since it
             shares a name with the derived-type sym. However if sym is a
             STRUCTURE, it may in fact share a name with any other basic type
             variable. If s is in fact of derived type then we can continue
             looking for a duplicate type declaration.  */
          if (sym->attr.flavor == FL_STRUCT && s->ts.type == BT_DERIVED)
            {
              s = s->ts.u.derived;
            }

	  if (gfc_fl_struct (s->attr.flavor) && !s->backend_decl)
            {
              if (s->attr.flavor == FL_UNION)
                s->backend_decl = gfc_get_union_type (s);
              else
                s->backend_decl = gfc_get_derived_type (s);
            }
	  gfc_copy_dt_decls_ifequal (s, sym, true);
	  return true;
	}
      else if (s->backend_decl)
	{
	  if (sym->ts.type == BT_DERIVED || sym->ts.type == BT_CLASS)
	    gfc_copy_dt_decls_ifequal (s->ts.u.derived, sym->ts.u.derived,
				       true);
	  else if (sym->ts.type == BT_CHARACTER)
	    sym->ts.u.cl->backend_decl = s->ts.u.cl->backend_decl;
	  sym->backend_decl = s->backend_decl;
	  return true;
	}
    }
  return false;
}


/* Create an array index type variable with function scope.  */

static tree
create_index_var (const char * pfx, int nest)
{
  tree decl;

  decl = gfc_create_var_np (gfc_array_index_type, pfx);
  if (nest)
    gfc_add_decl_to_parent_function (decl);
  else
    gfc_add_decl_to_function (decl);
  return decl;
}


/* Create variables to hold all the non-constant bits of info for a
   descriptorless array.  Remember these in the lang-specific part of the
   type.  */

static void
gfc_build_qualified_array (tree decl, gfc_symbol * sym)
{
  tree type;
  int dim;
  int nest;
  gfc_namespace* procns;
  symbol_attribute *array_attr;
  gfc_array_spec *as;
  bool is_classarray = IS_CLASS_ARRAY (sym);

  type = TREE_TYPE (decl);
  array_attr = is_classarray ? &CLASS_DATA (sym)->attr : &sym->attr;
  as = is_classarray ? CLASS_DATA (sym)->as : sym->as;

  /* We just use the descriptor, if there is one.  */
  if (GFC_DESCRIPTOR_TYPE_P (type))
    return;

  gcc_assert (GFC_ARRAY_TYPE_P (type));
  procns = gfc_find_proc_namespace (sym->ns);
  nest = (procns->proc_name->backend_decl != current_function_decl)
	 && !sym->attr.contained;

  if (array_attr->codimension && flag_coarray == GFC_FCOARRAY_LIB
      && as->type != AS_ASSUMED_SHAPE
      && GFC_TYPE_ARRAY_CAF_TOKEN (type) == NULL_TREE)
    {
      tree token;
      tree token_type = build_qualified_type (pvoid_type_node,
					      TYPE_QUAL_RESTRICT);

      if (sym->module && (sym->attr.use_assoc
			  || sym->ns->proc_name->attr.flavor == FL_MODULE))
	{
	  tree token_name
		= get_identifier (gfc_get_string (GFC_PREFIX ("caf_token%s"),
			IDENTIFIER_POINTER (gfc_sym_mangled_identifier (sym))));
	  token = build_decl (DECL_SOURCE_LOCATION (decl), VAR_DECL, token_name,
			      token_type);
	  if (sym->attr.use_assoc)
	    DECL_EXTERNAL (token) = 1;
	  else
	    TREE_STATIC (token) = 1;

	  TREE_PUBLIC (token) = 1;

	  if (sym->attr.access == ACCESS_PRIVATE && !sym->attr.public_used)
	    {
	      DECL_VISIBILITY (token) = VISIBILITY_HIDDEN;
	      DECL_VISIBILITY_SPECIFIED (token) = true;
	    }
	}
      else
	{
	  token = gfc_create_var_np (token_type, "caf_token");
	  TREE_STATIC (token) = 1;
	}

      GFC_TYPE_ARRAY_CAF_TOKEN (type) = token;
      DECL_ARTIFICIAL (token) = 1;
      DECL_NONALIASED (token) = 1;

      if (sym->module && !sym->attr.use_assoc)
	{
	  pushdecl (token);
	  DECL_CONTEXT (token) = sym->ns->proc_name->backend_decl;
	  gfc_module_add_decl (cur_module, token);
	}
      else
	gfc_add_decl_to_function (token);
    }

  for (dim = 0; dim < GFC_TYPE_ARRAY_RANK (type); dim++)
    {
      if (GFC_TYPE_ARRAY_LBOUND (type, dim) == NULL_TREE)
	{
	  GFC_TYPE_ARRAY_LBOUND (type, dim) = create_index_var ("lbound", nest);
	  TREE_NO_WARNING (GFC_TYPE_ARRAY_LBOUND (type, dim)) = 1;
	}
      /* Don't try to use the unknown bound for assumed shape arrays.  */
      if (GFC_TYPE_ARRAY_UBOUND (type, dim) == NULL_TREE
	  && (as->type != AS_ASSUMED_SIZE
	      || dim < GFC_TYPE_ARRAY_RANK (type) - 1))
	{
	  GFC_TYPE_ARRAY_UBOUND (type, dim) = create_index_var ("ubound", nest);
	  TREE_NO_WARNING (GFC_TYPE_ARRAY_UBOUND (type, dim)) = 1;
	}

      if (GFC_TYPE_ARRAY_STRIDE (type, dim) == NULL_TREE)
	{
	  GFC_TYPE_ARRAY_STRIDE (type, dim) = create_index_var ("stride", nest);
	  TREE_NO_WARNING (GFC_TYPE_ARRAY_STRIDE (type, dim)) = 1;
	}
    }
  for (dim = GFC_TYPE_ARRAY_RANK (type);
       dim < GFC_TYPE_ARRAY_RANK (type) + GFC_TYPE_ARRAY_CORANK (type); dim++)
    {
      if (GFC_TYPE_ARRAY_LBOUND (type, dim) == NULL_TREE)
	{
	  GFC_TYPE_ARRAY_LBOUND (type, dim) = create_index_var ("lbound", nest);
	  TREE_NO_WARNING (GFC_TYPE_ARRAY_LBOUND (type, dim)) = 1;
	}
      /* Don't try to use the unknown ubound for the last coarray dimension.  */
      if (GFC_TYPE_ARRAY_UBOUND (type, dim) == NULL_TREE
          && dim < GFC_TYPE_ARRAY_RANK (type) + GFC_TYPE_ARRAY_CORANK (type) - 1)
	{
	  GFC_TYPE_ARRAY_UBOUND (type, dim) = create_index_var ("ubound", nest);
	  TREE_NO_WARNING (GFC_TYPE_ARRAY_UBOUND (type, dim)) = 1;
	}
    }
  if (GFC_TYPE_ARRAY_OFFSET (type) == NULL_TREE)
    {
      GFC_TYPE_ARRAY_OFFSET (type) = gfc_create_var_np (gfc_array_index_type,
							"offset");
      TREE_NO_WARNING (GFC_TYPE_ARRAY_OFFSET (type)) = 1;

      if (nest)
	gfc_add_decl_to_parent_function (GFC_TYPE_ARRAY_OFFSET (type));
      else
	gfc_add_decl_to_function (GFC_TYPE_ARRAY_OFFSET (type));
    }

  if (GFC_TYPE_ARRAY_SIZE (type) == NULL_TREE
      && as->type != AS_ASSUMED_SIZE)
    {
      GFC_TYPE_ARRAY_SIZE (type) = create_index_var ("size", nest);
      TREE_NO_WARNING (GFC_TYPE_ARRAY_SIZE (type)) = 1;
    }

  if (POINTER_TYPE_P (type))
    {
      gcc_assert (GFC_ARRAY_TYPE_P (TREE_TYPE (type)));
      gcc_assert (TYPE_LANG_SPECIFIC (type)
		  == TYPE_LANG_SPECIFIC (TREE_TYPE (type)));
      type = TREE_TYPE (type);
    }

  if (! COMPLETE_TYPE_P (type) && GFC_TYPE_ARRAY_SIZE (type))
    {
      tree size, range;

      size = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			      GFC_TYPE_ARRAY_SIZE (type), gfc_index_one_node);
      range = build_range_type (gfc_array_index_type, gfc_index_zero_node,
				size);
      TYPE_DOMAIN (type) = range;
      layout_type (type);
    }

  if (TYPE_NAME (type) != NULL_TREE
      && GFC_TYPE_ARRAY_UBOUND (type, as->rank - 1) != NULL_TREE
      && TREE_CODE (GFC_TYPE_ARRAY_UBOUND (type, as->rank - 1)) == VAR_DECL)
    {
      tree gtype = DECL_ORIGINAL_TYPE (TYPE_NAME (type));

      for (dim = 0; dim < as->rank - 1; dim++)
	{
	  gcc_assert (TREE_CODE (gtype) == ARRAY_TYPE);
	  gtype = TREE_TYPE (gtype);
	}
      gcc_assert (TREE_CODE (gtype) == ARRAY_TYPE);
      if (TYPE_MAX_VALUE (TYPE_DOMAIN (gtype)) == NULL)
	TYPE_NAME (type) = NULL_TREE;
    }

  if (TYPE_NAME (type) == NULL_TREE)
    {
      tree gtype = TREE_TYPE (type), rtype, type_decl;

      for (dim = as->rank - 1; dim >= 0; dim--)
	{
	  tree lbound, ubound;
	  lbound = GFC_TYPE_ARRAY_LBOUND (type, dim);
	  ubound = GFC_TYPE_ARRAY_UBOUND (type, dim);
	  rtype = build_range_type (gfc_array_index_type, lbound, ubound);
	  gtype = build_array_type (gtype, rtype);
	  /* Ensure the bound variables aren't optimized out at -O0.
	     For -O1 and above they often will be optimized out, but
	     can be tracked by VTA.  Also set DECL_NAMELESS, so that
	     the artificial lbound.N or ubound.N DECL_NAME doesn't
	     end up in debug info.  */
	  if (lbound && TREE_CODE (lbound) == VAR_DECL
	      && DECL_ARTIFICIAL (lbound) && DECL_IGNORED_P (lbound))
	    {
	      if (DECL_NAME (lbound)
		  && strstr (IDENTIFIER_POINTER (DECL_NAME (lbound)),
			     "lbound") != 0)
		DECL_NAMELESS (lbound) = 1;
	      DECL_IGNORED_P (lbound) = 0;
	    }
	  if (ubound && TREE_CODE (ubound) == VAR_DECL
	      && DECL_ARTIFICIAL (ubound) && DECL_IGNORED_P (ubound))
	    {
	      if (DECL_NAME (ubound)
		  && strstr (IDENTIFIER_POINTER (DECL_NAME (ubound)),
			     "ubound") != 0)
		DECL_NAMELESS (ubound) = 1;
	      DECL_IGNORED_P (ubound) = 0;
	    }
	}
      TYPE_NAME (type) = type_decl = build_decl (input_location,
						 TYPE_DECL, NULL, gtype);
      DECL_ORIGINAL_TYPE (type_decl) = gtype;
    }
}


/* For some dummy arguments we don't use the actual argument directly.
   Instead we create a local decl and use that.  This allows us to perform
   initialization, and construct full type information.  */

static tree
gfc_build_dummy_array_decl (gfc_symbol * sym, tree dummy)
{
  tree decl;
  tree type;
  gfc_array_spec *as;
  symbol_attribute *array_attr;
  char *name;
  gfc_packed packed;
  int n;
  bool known_size;
  bool is_classarray = IS_CLASS_ARRAY (sym);

  /* Use the array as and attr.  */
  as = is_classarray ? CLASS_DATA (sym)->as : sym->as;
  array_attr = is_classarray ? &CLASS_DATA (sym)->attr : &sym->attr;

  /* The dummy is returned for pointer, allocatable or assumed rank arrays.
     For class arrays the information if sym is an allocatable or pointer
     object needs to be checked explicitly (IS_CLASS_ARRAY can be false for
     too many reasons to be of use here).  */
  if ((sym->ts.type != BT_CLASS && sym->attr.pointer)
      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)->attr.class_pointer)
      || array_attr->allocatable
      || (as && as->type == AS_ASSUMED_RANK))
    return dummy;

  /* Add to list of variables if not a fake result variable.
     These symbols are set on the symbol only, not on the class component.  */
  if (sym->attr.result || sym->attr.dummy)
    gfc_defer_symbol_init (sym);

  /* For a class array the array descriptor is in the _data component, while
     for a regular array the TREE_TYPE of the dummy is a pointer to the
     descriptor.  */
  type = TREE_TYPE (is_classarray ? gfc_class_data_get (dummy)
				  : TREE_TYPE (dummy));
  /* type now is the array descriptor w/o any indirection.  */
  gcc_assert (TREE_CODE (dummy) == PARM_DECL
	  && POINTER_TYPE_P (TREE_TYPE (dummy)));

  /* Do we know the element size?  */
  known_size = sym->ts.type != BT_CHARACTER
	  || INTEGER_CST_P (sym->ts.u.cl->backend_decl);

  if (known_size && !GFC_DESCRIPTOR_TYPE_P (type))
    {
      /* For descriptorless arrays with known element size the actual
         argument is sufficient.  */
      gfc_build_qualified_array (dummy, sym);
      return dummy;
    }

  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      /* Create a descriptorless array pointer.  */
      packed = PACKED_NO;

      /* Even when -frepack-arrays is used, symbols with TARGET attribute
	 are not repacked.  */
      if (!flag_repack_arrays || sym->attr.target)
	{
	  if (as->type == AS_ASSUMED_SIZE)
	    packed = PACKED_FULL;
	}
      else
	{
	  if (as->type == AS_EXPLICIT)
	    {
	      packed = PACKED_FULL;
	      for (n = 0; n < as->rank; n++)
		{
		  if (!(as->upper[n]
			&& as->lower[n]
			&& as->upper[n]->expr_type == EXPR_CONSTANT
			&& as->lower[n]->expr_type == EXPR_CONSTANT))
		    {
		      packed = PACKED_PARTIAL;
		      break;
		    }
		}
	    }
	  else
	    packed = PACKED_PARTIAL;
	}

      /* For classarrays the element type is required, but
	 gfc_typenode_for_spec () returns the array descriptor.  */
      type = is_classarray ? gfc_get_element_type (type)
			   : gfc_typenode_for_spec (&sym->ts);
      type = gfc_get_nodesc_array_type (type, as, packed,
					!sym->attr.target);
    }
  else
    {
      /* We now have an expression for the element size, so create a fully
	 qualified type.  Reset sym->backend decl or this will just return the
	 old type.  */
      DECL_ARTIFICIAL (sym->backend_decl) = 1;
      sym->backend_decl = NULL_TREE;
      type = gfc_sym_type (sym);
      packed = PACKED_FULL;
    }

  ASM_FORMAT_PRIVATE_NAME (name, IDENTIFIER_POINTER (DECL_NAME (dummy)), 0);
  decl = build_decl (input_location,
		     VAR_DECL, get_identifier (name), type);

  DECL_ARTIFICIAL (decl) = 1;
  DECL_NAMELESS (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  TREE_STATIC (decl) = 0;
  DECL_EXTERNAL (decl) = 0;

  /* Avoid uninitialized warnings for optional dummy arguments.  */
  if (sym->attr.optional)
    TREE_NO_WARNING (decl) = 1;

  /* We should never get deferred shape arrays here.  We used to because of
     frontend bugs.  */
  gcc_assert (as->type != AS_DEFERRED);

  if (packed == PACKED_PARTIAL)
    GFC_DECL_PARTIAL_PACKED_ARRAY (decl) = 1;
  else if (packed == PACKED_FULL)
    GFC_DECL_PACKED_ARRAY (decl) = 1;

  gfc_build_qualified_array (decl, sym);

  if (DECL_LANG_SPECIFIC (dummy))
    DECL_LANG_SPECIFIC (decl) = DECL_LANG_SPECIFIC (dummy);
  else
    gfc_allocate_lang_decl (decl);

  GFC_DECL_SAVED_DESCRIPTOR (decl) = dummy;

  if (sym->ns->proc_name->backend_decl == current_function_decl
      || sym->attr.contained)
    gfc_add_decl_to_function (decl);
  else
    gfc_add_decl_to_parent_function (decl);

  return decl;
}

/* For symbol SYM with GFC_DECL_SAVED_DESCRIPTOR used in contained
   function add a VAR_DECL to the current function with DECL_VALUE_EXPR
   pointing to the artificial variable for debug info purposes.  */

static void
gfc_nonlocal_dummy_array_decl (gfc_symbol *sym)
{
  tree decl, dummy;

  if (! nonlocal_dummy_decl_pset)
    nonlocal_dummy_decl_pset = new hash_set<tree>;

  if (nonlocal_dummy_decl_pset->add (sym->backend_decl))
    return;

  dummy = GFC_DECL_SAVED_DESCRIPTOR (sym->backend_decl);
  decl = build_decl (input_location, VAR_DECL, DECL_NAME (dummy),
		     TREE_TYPE (sym->backend_decl));
  DECL_ARTIFICIAL (decl) = 0;
  TREE_USED (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  TREE_STATIC (decl) = 0;
  DECL_EXTERNAL (decl) = 0;
  if (DECL_BY_REFERENCE (dummy))
    DECL_BY_REFERENCE (decl) = 1;
  DECL_LANG_SPECIFIC (decl) = DECL_LANG_SPECIFIC (sym->backend_decl);
  SET_DECL_VALUE_EXPR (decl, sym->backend_decl);
  DECL_HAS_VALUE_EXPR_P (decl) = 1;
  DECL_CONTEXT (decl) = DECL_CONTEXT (sym->backend_decl);
  DECL_CHAIN (decl) = nonlocal_dummy_decls;
  nonlocal_dummy_decls = decl;
}

/* Return a constant or a variable to use as a string length.  Does not
   add the decl to the current scope.  */

static tree
gfc_create_string_length (gfc_symbol * sym)
{
  gcc_assert (sym->ts.u.cl);
  gfc_conv_const_charlen (sym->ts.u.cl);

  if (sym->ts.u.cl->backend_decl == NULL_TREE)
    {
      tree length;
      const char *name;

      /* The string length variable shall be in static memory if it is either
	 explicitly SAVED, a module variable or with -fno-automatic. Only
	 relevant is "len=:" - otherwise, it is either a constant length or
	 it is an automatic variable.  */
      bool static_length = sym->attr.save
			   || sym->ns->proc_name->attr.flavor == FL_MODULE
			   || (flag_max_stack_var_size == 0
			       && sym->ts.deferred && !sym->attr.dummy
			       && !sym->attr.result && !sym->attr.function);

      /* Also prefix the mangled name. We need to call GFC_PREFIX for static
	 variables as some systems do not support the "." in the assembler name.
	 For nonstatic variables, the "." does not appear in assembler.  */
      if (static_length)
	{
	  if (sym->module)
	    name = gfc_get_string (GFC_PREFIX ("%s_MOD_%s"), sym->module,
				   sym->name);
	  else
	    name = gfc_get_string (GFC_PREFIX ("%s"), sym->name);
	}
      else if (sym->module)
	name = gfc_get_string (".__%s_MOD_%s", sym->module, sym->name);
      else
	name = gfc_get_string (".%s", sym->name);

      length = build_decl (input_location,
			   VAR_DECL, get_identifier (name),
			   gfc_charlen_type_node);
      DECL_ARTIFICIAL (length) = 1;
      TREE_USED (length) = 1;
      if (sym->ns->proc_name->tlink != NULL)
	gfc_defer_symbol_init (sym);

      sym->ts.u.cl->backend_decl = length;

      if (static_length)
	TREE_STATIC (length) = 1;

      if (sym->ns->proc_name->attr.flavor == FL_MODULE
	  && (sym->attr.access != ACCESS_PRIVATE || sym->attr.public_used))
	TREE_PUBLIC (length) = 1;
    }

  gcc_assert (sym->ts.u.cl->backend_decl != NULL_TREE);
  return sym->ts.u.cl->backend_decl;
}

/* If a variable is assigned a label, we add another two auxiliary
   variables.  */

static void
gfc_add_assign_aux_vars (gfc_symbol * sym)
{
  tree addr;
  tree length;
  tree decl;

  gcc_assert (sym->backend_decl);

  decl = sym->backend_decl;
  gfc_allocate_lang_decl (decl);
  GFC_DECL_ASSIGN (decl) = 1;
  length = build_decl (input_location,
		       VAR_DECL, create_tmp_var_name (sym->name),
		       gfc_charlen_type_node);
  addr = build_decl (input_location,
		     VAR_DECL, create_tmp_var_name (sym->name),
		     pvoid_type_node);
  gfc_finish_var_decl (length, sym);
  gfc_finish_var_decl (addr, sym);
  /*  STRING_LENGTH is also used as flag. Less than -1 means that
      ASSIGN_ADDR can not be used. Equal -1 means that ASSIGN_ADDR is the
      target label's address. Otherwise, value is the length of a format string
      and ASSIGN_ADDR is its address.  */
  if (TREE_STATIC (length))
    DECL_INITIAL (length) = build_int_cst (gfc_charlen_type_node, -2);
  else
    gfc_defer_symbol_init (sym);

  GFC_DECL_STRING_LEN (decl) = length;
  GFC_DECL_ASSIGN_ADDR (decl) = addr;
}


static tree
add_attributes_to_decl (symbol_attribute sym_attr, tree list)
{
  unsigned id;
  tree attr;

  for (id = 0; id < EXT_ATTR_NUM; id++)
    if (sym_attr.ext_attr & (1 << id))
      {
	attr = build_tree_list (
		 get_identifier (ext_attr_list[id].middle_end_name),
				 NULL_TREE);
	list = chainon (list, attr);
      }

  if (sym_attr.omp_declare_target)
    list = tree_cons (get_identifier ("omp declare target"),
		      NULL_TREE, list);

  if (sym_attr.oacc_function)
    {
      tree dims = NULL_TREE;
      int ix;
      int level = sym_attr.oacc_function - 1;

      for (ix = GOMP_DIM_MAX; ix--;)
	dims = tree_cons (build_int_cst (boolean_type_node, ix >= level),
			  integer_zero_node, dims);

      list = tree_cons (get_identifier ("oacc function"),
			dims, list);
    }

  return list;
}


static void build_function_decl (gfc_symbol * sym, bool global);


/* Return the decl for a gfc_symbol, create it if it doesn't already
   exist.  */

tree
gfc_get_symbol_decl (gfc_symbol * sym)
{
  tree decl;
  tree length = NULL_TREE;
  tree attributes;
  int byref;
  bool intrinsic_array_parameter = false;
  bool fun_or_res;

  gcc_assert (sym->attr.referenced
	      || sym->attr.flavor == FL_PROCEDURE
	      || sym->attr.use_assoc
	      || sym->attr.used_in_submodule
	      || sym->ns->proc_name->attr.if_source == IFSRC_IFBODY
	      || (sym->module && sym->attr.if_source != IFSRC_DECL
		  && sym->backend_decl));

  if (sym->ns && sym->ns->proc_name && sym->ns->proc_name->attr.function)
    byref = gfc_return_by_reference (sym->ns->proc_name);
  else
    byref = 0;

  /* Make sure that the vtab for the declared type is completed.  */
  if (sym->ts.type == BT_CLASS)
    {
      gfc_component *c = CLASS_DATA (sym);
      if (!c->ts.u.derived->backend_decl)
	{
	  gfc_find_derived_vtab (c->ts.u.derived);
	  gfc_get_derived_type (sym->ts.u.derived);
	}
    }

  /* All deferred character length procedures need to retain the backend
     decl, which is a pointer to the character length in the caller's
     namespace and to declare a local character length.  */
  if (!byref && sym->attr.function
	&& sym->ts.type == BT_CHARACTER
	&& sym->ts.deferred
	&& sym->ts.u.cl->passed_length == NULL
	&& sym->ts.u.cl->backend_decl
	&& TREE_CODE (sym->ts.u.cl->backend_decl) == PARM_DECL)
    {
      sym->ts.u.cl->passed_length = sym->ts.u.cl->backend_decl;
      gcc_assert (POINTER_TYPE_P (TREE_TYPE (sym->ts.u.cl->passed_length)));
      sym->ts.u.cl->backend_decl = build_fold_indirect_ref (sym->ts.u.cl->backend_decl);
    }

  fun_or_res = byref && (sym->attr.result
			 || (sym->attr.function && sym->ts.deferred));
  if ((sym->attr.dummy && ! sym->attr.function) || fun_or_res)
    {
      /* Return via extra parameter.  */
      if (sym->attr.result && byref
	  && !sym->backend_decl)
	{
	  sym->backend_decl =
	    DECL_ARGUMENTS (sym->ns->proc_name->backend_decl);
	  /* For entry master function skip over the __entry
	     argument.  */
	  if (sym->ns->proc_name->attr.entry_master)
	    sym->backend_decl = DECL_CHAIN (sym->backend_decl);
	}

      /* Dummy variables should already have been created.  */
      gcc_assert (sym->backend_decl);

      /* Create a character length variable.  */
      if (sym->ts.type == BT_CHARACTER)
	{
	  /* For a deferred dummy, make a new string length variable.  */
	  if (sym->ts.deferred
		&&
	     (sym->ts.u.cl->passed_length == sym->ts.u.cl->backend_decl))
	    sym->ts.u.cl->backend_decl = NULL_TREE;

	  if (sym->ts.deferred && byref)
	    {
	      /* The string length of a deferred char array is stored in the
		 parameter at sym->ts.u.cl->backend_decl as a reference and
		 marked as a result.  Exempt this variable from generating a
		 temporary for it.  */
	      if (sym->attr.result)
		{
		  /* We need to insert a indirect ref for param decls.  */
		  if (sym->ts.u.cl->backend_decl
		      && TREE_CODE (sym->ts.u.cl->backend_decl) == PARM_DECL)
		    {
		      sym->ts.u.cl->passed_length = sym->ts.u.cl->backend_decl;
		      sym->ts.u.cl->backend_decl =
			build_fold_indirect_ref (sym->ts.u.cl->backend_decl);
		    }
		}
	      /* For all other parameters make sure, that they are copied so
		 that the value and any modifications are local to the routine
		 by generating a temporary variable.  */
	      else if (sym->attr.function
		       && sym->ts.u.cl->passed_length == NULL
		       && sym->ts.u.cl->backend_decl)
		{
		  sym->ts.u.cl->passed_length = sym->ts.u.cl->backend_decl;
		  if (POINTER_TYPE_P (TREE_TYPE (sym->ts.u.cl->passed_length)))
		    sym->ts.u.cl->backend_decl
			= build_fold_indirect_ref (sym->ts.u.cl->backend_decl);
		  else
		    sym->ts.u.cl->backend_decl = NULL_TREE;
		}
	    }

	  if (sym->ts.u.cl->backend_decl == NULL_TREE)
	    length = gfc_create_string_length (sym);
	  else
	    length = sym->ts.u.cl->backend_decl;
	  if (TREE_CODE (length) == VAR_DECL
	      && DECL_FILE_SCOPE_P (length))
	    {
	      /* Add the string length to the same context as the symbol.  */
	      if (DECL_CONTEXT (sym->backend_decl) == current_function_decl)
	        gfc_add_decl_to_function (length);
	      else
		gfc_add_decl_to_parent_function (length);

	      gcc_assert (DECL_CONTEXT (sym->backend_decl) ==
			    DECL_CONTEXT (length));

	      gfc_defer_symbol_init (sym);
	    }
	}

      /* Use a copy of the descriptor for dummy arrays.  */
      if ((sym->attr.dimension || sym->attr.codimension)
         && !TREE_USED (sym->backend_decl))
        {
	  decl = gfc_build_dummy_array_decl (sym, sym->backend_decl);
	  /* Prevent the dummy from being detected as unused if it is copied.  */
	  if (sym->backend_decl != NULL && decl != sym->backend_decl)
	    DECL_ARTIFICIAL (sym->backend_decl) = 1;
	  sym->backend_decl = decl;
	}

      /* Returning the descriptor for dummy class arrays is hazardous, because
	 some caller is expecting an expression to apply the component refs to.
	 Therefore the descriptor is only created and stored in
	 sym->backend_decl's GFC_DECL_SAVED_DESCRIPTOR.  The caller is then
	 responsible to extract it from there, when the descriptor is
	 desired.  */
      if (IS_CLASS_ARRAY (sym)
	  && (!DECL_LANG_SPECIFIC (sym->backend_decl)
	      || !GFC_DECL_SAVED_DESCRIPTOR (sym->backend_decl)))
	{
	  decl = gfc_build_dummy_array_decl (sym, sym->backend_decl);
	  /* Prevent the dummy from being detected as unused if it is copied.  */
	  if (sym->backend_decl != NULL && decl != sym->backend_decl)
	    DECL_ARTIFICIAL (sym->backend_decl) = 1;
	  sym->backend_decl = decl;
	}

      TREE_USED (sym->backend_decl) = 1;
      if (sym->attr.assign && GFC_DECL_ASSIGN (sym->backend_decl) == 0)
	{
	  gfc_add_assign_aux_vars (sym);
	}

      if ((sym->attr.dimension || IS_CLASS_ARRAY (sym))
	  && DECL_LANG_SPECIFIC (sym->backend_decl)
	  && GFC_DECL_SAVED_DESCRIPTOR (sym->backend_decl)
	  && DECL_CONTEXT (sym->backend_decl) != current_function_decl)
	gfc_nonlocal_dummy_array_decl (sym);

      if (sym->ts.type == BT_CLASS && sym->backend_decl)
	GFC_DECL_CLASS(sym->backend_decl) = 1;

     return sym->backend_decl;
    }

  if (sym->backend_decl)
    return sym->backend_decl;

  /* Special case for array-valued named constants from intrinsic
     procedures; those are inlined.  */
  if (sym->attr.use_assoc && sym->attr.flavor == FL_PARAMETER
      && (sym->from_intmod == INTMOD_ISO_FORTRAN_ENV
	  || sym->from_intmod == INTMOD_ISO_C_BINDING))
    intrinsic_array_parameter = true;

  /* If use associated compilation, use the module
     declaration.  */
  if ((sym->attr.flavor == FL_VARIABLE
       || sym->attr.flavor == FL_PARAMETER)
      && (sym->attr.use_assoc || sym->attr.used_in_submodule)
      && !intrinsic_array_parameter
      && sym->module
      && gfc_get_module_backend_decl (sym))
    {
      if (sym->ts.type == BT_CLASS && sym->backend_decl)
	GFC_DECL_CLASS(sym->backend_decl) = 1;
      return sym->backend_decl;
    }

  if (sym->attr.flavor == FL_PROCEDURE)
    {
      /* Catch functions. Only used for actual parameters,
	 procedure pointers and procptr initialization targets.  */
      if (sym->attr.use_assoc || sym->attr.intrinsic
	  || sym->attr.if_source != IFSRC_DECL)
	{
	  decl = gfc_get_extern_function_decl (sym);
	  gfc_set_decl_location (decl, &sym->declared_at);
	}
      else
	{
	  if (!sym->backend_decl)
	    build_function_decl (sym, false);
	  decl = sym->backend_decl;
	}
      return decl;
    }

  if (sym->attr.intrinsic)
    gfc_internal_error ("intrinsic variable which isn't a procedure");

  /* Create string length decl first so that they can be used in the
     type declaration.  For associate names, the target character
     length is used. Set 'length' to a constant so that if the
     string lenght is a variable, it is not finished a second time.  */
  if (sym->ts.type == BT_CHARACTER)
    {
      if (sym->attr.associate_var
	  && sym->ts.u.cl->backend_decl
	  && TREE_CODE (sym->ts.u.cl->backend_decl) == VAR_DECL)
	length = gfc_index_zero_node;
      else
	length = gfc_create_string_length (sym);
    }

  /* Create the decl for the variable.  */
  decl = build_decl (sym->declared_at.lb->location,
		     VAR_DECL, gfc_sym_identifier (sym), gfc_sym_type (sym));

  /* Add attributes to variables.  Functions are handled elsewhere.  */
  attributes = add_attributes_to_decl (sym->attr, NULL_TREE);
  decl_attributes (&decl, attributes, 0);

  /* Symbols from modules should have their assembler names mangled.
     This is done here rather than in gfc_finish_var_decl because it
     is different for string length variables.  */
  if (sym->module)
    {
      gfc_set_decl_assembler_name (decl, gfc_sym_mangled_identifier (sym));
      if (sym->attr.use_assoc && !intrinsic_array_parameter)
	DECL_IGNORED_P (decl) = 1;
    }

  if (sym->attr.select_type_temporary)
    {
      DECL_ARTIFICIAL (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
    }

  if (sym->attr.dimension || sym->attr.codimension)
    {
      /* Create variables to hold the non-constant bits of array info.  */
      gfc_build_qualified_array (decl, sym);

      if (sym->attr.contiguous
	  || ((sym->attr.allocatable || !sym->attr.dummy) && !sym->attr.pointer))
	GFC_DECL_PACKED_ARRAY (decl) = 1;
    }

  /* Remember this variable for allocation/cleanup.  */
  if (sym->attr.dimension || sym->attr.allocatable || sym->attr.codimension
      || (sym->ts.type == BT_CLASS &&
	  (CLASS_DATA (sym)->attr.dimension
	   || CLASS_DATA (sym)->attr.allocatable))
      || (sym->ts.type == BT_DERIVED
	  && (sym->ts.u.derived->attr.alloc_comp
	      || (!sym->attr.pointer && !sym->attr.artificial && !sym->attr.save
		  && !sym->ns->proc_name->attr.is_main_program
		  && gfc_is_finalizable (sym->ts.u.derived, NULL))))
      /* This applies a derived type default initializer.  */
      || (sym->ts.type == BT_DERIVED
	  && sym->attr.save == SAVE_NONE
	  && !sym->attr.data
	  && !sym->attr.allocatable
	  && (sym->value && !sym->ns->proc_name->attr.is_main_program)
	  && !(sym->attr.use_assoc && !intrinsic_array_parameter)))
    gfc_defer_symbol_init (sym);

  /* Associate names can use the hidden string length variable
     of their associated target.  */
  if (sym->ts.type == BT_CHARACTER
      && TREE_CODE (length) != INTEGER_CST)
    {
      gfc_finish_var_decl (length, sym);
      gcc_assert (!sym->value);
    }

  gfc_finish_var_decl (decl, sym);

  if (sym->ts.type == BT_CHARACTER)
    /* Character variables need special handling.  */
    gfc_allocate_lang_decl (decl);
  else if (sym->attr.subref_array_pointer)
    /* We need the span for these beasts.  */
    gfc_allocate_lang_decl (decl);

  if (sym->attr.subref_array_pointer)
    {
      tree span;
      GFC_DECL_SUBREF_ARRAY_P (decl) = 1;
      span = build_decl (input_location,
			 VAR_DECL, create_tmp_var_name ("span"),
			 gfc_array_index_type);
      gfc_finish_var_decl (span, sym);
      TREE_STATIC (span) = TREE_STATIC (decl);
      DECL_ARTIFICIAL (span) = 1;

      GFC_DECL_SPAN (decl) = span;
      GFC_TYPE_ARRAY_SPAN (TREE_TYPE (decl)) = span;
    }

  if (sym->ts.type == BT_CLASS)
	GFC_DECL_CLASS(decl) = 1;

  sym->backend_decl = decl;

  if (sym->attr.assign)
    gfc_add_assign_aux_vars (sym);

  if (intrinsic_array_parameter)
    {
      TREE_STATIC (decl) = 1;
      DECL_EXTERNAL (decl) = 0;
    }

  if (TREE_STATIC (decl)
      && !(sym->attr.use_assoc && !intrinsic_array_parameter)
      && (sym->attr.save || sym->ns->proc_name->attr.is_main_program
	  || !gfc_can_put_var_on_stack (DECL_SIZE_UNIT (decl))
	  || sym->attr.data || sym->ns->proc_name->attr.flavor == FL_MODULE)
      && (flag_coarray != GFC_FCOARRAY_LIB
	  || !sym->attr.codimension || sym->attr.allocatable))
    {
      /* Add static initializer. For procedures, it is only needed if
	 SAVE is specified otherwise they need to be reinitialized
	 every time the procedure is entered. The TREE_STATIC is
	 in this case due to -fmax-stack-var-size=.  */

      DECL_INITIAL (decl) = gfc_conv_initializer (sym->value, &sym->ts,
				    TREE_TYPE (decl), sym->attr.dimension
				    || (sym->attr.codimension
					&& sym->attr.allocatable),
				    sym->attr.pointer || sym->attr.allocatable
				    || sym->ts.type == BT_CLASS,
				    sym->attr.proc_pointer);
    }

  if (!TREE_STATIC (decl)
      && POINTER_TYPE_P (TREE_TYPE (decl))
      && !sym->attr.pointer
      && !sym->attr.allocatable
      && !sym->attr.proc_pointer
      && !sym->attr.select_type_temporary)
    DECL_BY_REFERENCE (decl) = 1;

  if (sym->attr.associate_var)
    GFC_DECL_ASSOCIATE_VAR_P (decl) = 1;

  if (sym->attr.vtab
      || (sym->name[0] == '_' && strncmp ("__def_init", sym->name, 10) == 0))
    TREE_READONLY (decl) = 1;

  return decl;
}


/* Substitute a temporary variable in place of the real one.  */

void
gfc_shadow_sym (gfc_symbol * sym, tree decl, gfc_saved_var * save)
{
  save->attr = sym->attr;
  save->decl = sym->backend_decl;

  gfc_clear_attr (&sym->attr);
  sym->attr.referenced = 1;
  sym->attr.flavor = FL_VARIABLE;

  sym->backend_decl = decl;
}


/* Restore the original variable.  */

void
gfc_restore_sym (gfc_symbol * sym, gfc_saved_var * save)
{
  sym->attr = save->attr;
  sym->backend_decl = save->decl;
}


/* Declare a procedure pointer.  */

static tree
get_proc_pointer_decl (gfc_symbol *sym)
{
  tree decl;
  tree attributes;

  decl = sym->backend_decl;
  if (decl)
    return decl;

  decl = build_decl (input_location,
		     VAR_DECL, get_identifier (sym->name),
		     build_pointer_type (gfc_get_function_type (sym)));

  if (sym->module)
    {
      /* Apply name mangling.  */
      gfc_set_decl_assembler_name (decl, gfc_sym_mangled_identifier (sym));
      if (sym->attr.use_assoc)
	DECL_IGNORED_P (decl) = 1;
    }

  if ((sym->ns->proc_name
      && sym->ns->proc_name->backend_decl == current_function_decl)
      || sym->attr.contained)
    gfc_add_decl_to_function (decl);
  else if (sym->ns->proc_name->attr.flavor != FL_MODULE)
    gfc_add_decl_to_parent_function (decl);

  sym->backend_decl = decl;

  /* If a variable is USE associated, it's always external.  */
  if (sym->attr.use_assoc)
    {
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
    }
  else if (sym->module && sym->ns->proc_name->attr.flavor == FL_MODULE)
    {
      /* This is the declaration of a module variable.  */
      TREE_PUBLIC (decl) = 1;
      if (sym->attr.access == ACCESS_PRIVATE && !sym->attr.public_used)
	{
	  DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
	  DECL_VISIBILITY_SPECIFIED (decl) = true;
	}
      TREE_STATIC (decl) = 1;
    }

  if (!sym->attr.use_assoc
	&& (sym->attr.save != SAVE_NONE || sym->attr.data
	      || (sym->value && sym->ns->proc_name->attr.is_main_program)))
    TREE_STATIC (decl) = 1;

  if (TREE_STATIC (decl) && sym->value)
    {
      /* Add static initializer.  */
      DECL_INITIAL (decl) = gfc_conv_initializer (sym->value, &sym->ts,
						  TREE_TYPE (decl),
						  sym->attr.dimension,
						  false, true);
    }

  /* Handle threadprivate procedure pointers.  */
  if (sym->attr.threadprivate
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
    set_decl_tls_model (decl, decl_default_tls_model (decl));

  attributes = add_attributes_to_decl (sym->attr, NULL_TREE);
  decl_attributes (&decl, attributes, 0);

  return decl;
}


/* Get a basic decl for an external function.  */

tree
gfc_get_extern_function_decl (gfc_symbol * sym)
{
  tree type;
  tree fndecl;
  tree attributes;
  gfc_expr e;
  gfc_intrinsic_sym *isym;
  gfc_expr argexpr;
  char s[GFC_MAX_SYMBOL_LEN + 23]; /* "_gfortran_f2c_specific" and '\0'.  */
  tree name;
  tree mangled_name;
  gfc_gsymbol *gsym;

  if (sym->backend_decl)
    return sym->backend_decl;

  /* We should never be creating external decls for alternate entry points.
     The procedure may be an alternate entry point, but we don't want/need
     to know that.  */
  gcc_assert (!(sym->attr.entry || sym->attr.entry_master));

  if (sym->attr.proc_pointer)
    return get_proc_pointer_decl (sym);

  /* See if this is an external procedure from the same file.  If so,
     return the backend_decl.  */
  gsym =  gfc_find_gsymbol (gfc_gsym_root, sym->binding_label
					   ? sym->binding_label : sym->name);

  if (gsym && !gsym->defined)
    gsym = NULL;

  /* This can happen because of C binding.  */
  if (gsym && gsym->ns && gsym->ns->proc_name
      && gsym->ns->proc_name->attr.flavor == FL_MODULE)
    goto module_sym;

  if ((!sym->attr.use_assoc || sym->attr.if_source != IFSRC_DECL)
      && !sym->backend_decl
      && gsym && gsym->ns
      && ((gsym->type == GSYM_SUBROUTINE) || (gsym->type == GSYM_FUNCTION))
      && (gsym->ns->proc_name->backend_decl || !sym->attr.intrinsic))
    {
      if (!gsym->ns->proc_name->backend_decl)
	{
	  /* By construction, the external function cannot be
	     a contained procedure.  */
	  locus old_loc;

	  gfc_save_backend_locus (&old_loc);
	  push_cfun (NULL);

	  gfc_create_function_decl (gsym->ns, true);

	  pop_cfun ();
	  gfc_restore_backend_locus (&old_loc);
	}

      /* If the namespace has entries, the proc_name is the
	 entry master.  Find the entry and use its backend_decl.
	 otherwise, use the proc_name backend_decl.  */
      if (gsym->ns->entries)
	{
	  gfc_entry_list *entry = gsym->ns->entries;

	  for (; entry; entry = entry->next)
	    {
	      if (strcmp (gsym->name, entry->sym->name) == 0)
		{
	          sym->backend_decl = entry->sym->backend_decl;
		  break;
		}
	    }
	}
      else
	sym->backend_decl = gsym->ns->proc_name->backend_decl;

      if (sym->backend_decl)
	{
	  /* Avoid problems of double deallocation of the backend declaration
	     later in gfc_trans_use_stmts; cf. PR 45087.  */
	  if (sym->attr.if_source != IFSRC_DECL && sym->attr.use_assoc)
	    sym->attr.use_assoc = 0;

	  return sym->backend_decl;
	}
    }

  /* See if this is a module procedure from the same file.  If so,
     return the backend_decl.  */
  if (sym->module)
    gsym =  gfc_find_gsymbol (gfc_gsym_root, sym->module);

module_sym:
  if (gsym && gsym->ns
      && (gsym->type == GSYM_MODULE
	  || (gsym->ns->proc_name && gsym->ns->proc_name->attr.flavor == FL_MODULE)))
    {
      gfc_symbol *s;

      s = NULL;
      if (gsym->type == GSYM_MODULE)
	gfc_find_symbol (sym->name, gsym->ns, 0, &s);
      else
	gfc_find_symbol (gsym->sym_name, gsym->ns, 0, &s);

      if (s && s->backend_decl)
	{
	  if (sym->ts.type == BT_DERIVED || sym->ts.type == BT_CLASS)
	    gfc_copy_dt_decls_ifequal (s->ts.u.derived, sym->ts.u.derived,
				       true);
	  else if (sym->ts.type == BT_CHARACTER)
	    sym->ts.u.cl->backend_decl = s->ts.u.cl->backend_decl;
	  sym->backend_decl = s->backend_decl;
	  return sym->backend_decl;
	}
    }

  if (sym->attr.intrinsic)
    {
      /* Call the resolution function to get the actual name.  This is
         a nasty hack which relies on the resolution functions only looking
	 at the first argument.  We pass NULL for the second argument
	 otherwise things like AINT get confused.  */
      isym = gfc_find_function (sym->name);
      gcc_assert (isym->resolve.f0 != NULL);

      memset (&e, 0, sizeof (e));
      e.expr_type = EXPR_FUNCTION;

      memset (&argexpr, 0, sizeof (argexpr));
      gcc_assert (isym->formal);
      argexpr.ts = isym->formal->ts;

      if (isym->formal->next == NULL)
	isym->resolve.f1 (&e, &argexpr);
      else
	{
	  if (isym->formal->next->next == NULL)
	    isym->resolve.f2 (&e, &argexpr, NULL);
	  else
	    {
	      if (isym->formal->next->next->next == NULL)
		isym->resolve.f3 (&e, &argexpr, NULL, NULL);
	      else
		{
		  /* All specific intrinsics take less than 5 arguments.  */
		  gcc_assert (isym->formal->next->next->next->next == NULL);
		  isym->resolve.f4 (&e, &argexpr, NULL, NULL, NULL);
		}
	    }
	}

      if (flag_f2c
	  && ((e.ts.type == BT_REAL && e.ts.kind == gfc_default_real_kind)
	      || e.ts.type == BT_COMPLEX))
	{
	  /* Specific which needs a different implementation if f2c
	     calling conventions are used.  */
	  sprintf (s, "_gfortran_f2c_specific%s", e.value.function.name);
	}
      else
	sprintf (s, "_gfortran_specific%s", e.value.function.name);

      name = get_identifier (s);
      mangled_name = name;
    }
  else
    {
      name = gfc_sym_identifier (sym);
      mangled_name = gfc_sym_mangled_function_id (sym);
    }

  type = gfc_get_function_type (sym);
  fndecl = build_decl (input_location,
		       FUNCTION_DECL, name, type);

  /* Initialize DECL_EXTERNAL and TREE_PUBLIC before calling decl_attributes;
     TREE_PUBLIC specifies whether a function is globally addressable (i.e.
     the opposite of declaring a function as static in C).  */
  DECL_EXTERNAL (fndecl) = 1;
  TREE_PUBLIC (fndecl) = 1;

  attributes = add_attributes_to_decl (sym->attr, NULL_TREE);
  decl_attributes (&fndecl, attributes, 0);

  gfc_set_decl_assembler_name (fndecl, mangled_name);

  /* Set the context of this decl.  */
  if (0 && sym->ns && sym->ns->proc_name)
    {
      /* TODO: Add external decls to the appropriate scope.  */
      DECL_CONTEXT (fndecl) = sym->ns->proc_name->backend_decl;
    }
  else
    {
      /* Global declaration, e.g. intrinsic subroutine.  */
      DECL_CONTEXT (fndecl) = NULL_TREE;
    }

  /* Set attributes for PURE functions. A call to PURE function in the
     Fortran 95 sense is both pure and without side effects in the C
     sense.  */
  if (sym->attr.pure || sym->attr.implicit_pure)
    {
      if (sym->attr.function && !gfc_return_by_reference (sym))
	DECL_PURE_P (fndecl) = 1;
      /* TODO: check if pure SUBROUTINEs don't have INTENT(OUT)
	 parameters and don't use alternate returns (is this
	 allowed?). In that case, calls to them are meaningless, and
	 can be optimized away. See also in build_function_decl().  */
      TREE_SIDE_EFFECTS (fndecl) = 0;
    }

  /* Mark non-returning functions.  */
  if (sym->attr.noreturn)
      TREE_THIS_VOLATILE(fndecl) = 1;

  sym->backend_decl = fndecl;

  if (DECL_CONTEXT (fndecl) == NULL_TREE)
    pushdecl_top_level (fndecl);

  if (sym->formal_ns
      && sym->formal_ns->proc_name == sym
      && sym->formal_ns->omp_declare_simd)
    gfc_trans_omp_declare_simd (sym->formal_ns);

  return fndecl;
}


/* Create a declaration for a procedure.  For external functions (in the C
   sense) use gfc_get_extern_function_decl.  HAS_ENTRIES is true if this is
   a master function with alternate entry points.  */

static void
build_function_decl (gfc_symbol * sym, bool global)
{
  tree fndecl, type, attributes;
  symbol_attribute attr;
  tree result_decl;
  gfc_formal_arglist *f;

  bool module_procedure = sym->attr.module_procedure
			  && sym->ns
			  && sym->ns->proc_name
			  && sym->ns->proc_name->attr.flavor == FL_MODULE;

  gcc_assert (!sym->attr.external || module_procedure);

  if (sym->backend_decl)
    return;

  /* Set the line and filename.  sym->declared_at seems to point to the
     last statement for subroutines, but it'll do for now.  */
  gfc_set_backend_locus (&sym->declared_at);

  /* Allow only one nesting level.  Allow public declarations.  */
  gcc_assert (current_function_decl == NULL_TREE
	      || DECL_FILE_SCOPE_P (current_function_decl)
	      || (TREE_CODE (DECL_CONTEXT (current_function_decl))
		  == NAMESPACE_DECL));

  type = gfc_get_function_type (sym);
  fndecl = build_decl (input_location,
		       FUNCTION_DECL, gfc_sym_identifier (sym), type);

  attr = sym->attr;

  /* Initialize DECL_EXTERNAL and TREE_PUBLIC before calling decl_attributes;
     TREE_PUBLIC specifies whether a function is globally addressable (i.e.
     the opposite of declaring a function as static in C).  */
  DECL_EXTERNAL (fndecl) = 0;

  if (sym->attr.access == ACCESS_UNKNOWN && sym->module
      && (sym->ns->default_access == ACCESS_PRIVATE
	  || (sym->ns->default_access == ACCESS_UNKNOWN
	      && flag_module_private)))
    sym->attr.access = ACCESS_PRIVATE;

  if (!current_function_decl
      && !sym->attr.entry_master && !sym->attr.is_main_program
      && (sym->attr.access != ACCESS_PRIVATE || sym->binding_label
	  || sym->attr.public_used))
    TREE_PUBLIC (fndecl) = 1;

  if (sym->attr.referenced || sym->attr.entry_master)
    TREE_USED (fndecl) = 1;

  attributes = add_attributes_to_decl (attr, NULL_TREE);
  decl_attributes (&fndecl, attributes, 0);

  /* Figure out the return type of the declared function, and build a
     RESULT_DECL for it.  If this is a subroutine with alternate
     returns, build a RESULT_DECL for it.  */
  result_decl = NULL_TREE;
  /* TODO: Shouldn't this just be TREE_TYPE (TREE_TYPE (fndecl)).  */
  if (attr.function)
    {
      if (gfc_return_by_reference (sym))
	type = void_type_node;
      else
	{
	  if (sym->result != sym)
	    result_decl = gfc_sym_identifier (sym->result);

	  type = TREE_TYPE (TREE_TYPE (fndecl));
	}
    }
  else
    {
      /* Look for alternate return placeholders.  */
      int has_alternate_returns = 0;
      for (f = gfc_sym_get_dummy_args (sym); f; f = f->next)
	{
	  if (f->sym == NULL)
	    {
	      has_alternate_returns = 1;
	      break;
	    }
	}

      if (has_alternate_returns)
	type = integer_type_node;
      else
	type = void_type_node;
    }

  result_decl = build_decl (input_location,
			    RESULT_DECL, result_decl, type);
  DECL_ARTIFICIAL (result_decl) = 1;
  DECL_IGNORED_P (result_decl) = 1;
  DECL_CONTEXT (result_decl) = fndecl;
  DECL_RESULT (fndecl) = result_decl;

  /* Don't call layout_decl for a RESULT_DECL.
     layout_decl (result_decl, 0);  */

  /* TREE_STATIC means the function body is defined here.  */
  TREE_STATIC (fndecl) = 1;

  /* Set attributes for PURE functions. A call to a PURE function in the
     Fortran 95 sense is both pure and without side effects in the C
     sense.  */
  if (attr.pure || attr.implicit_pure)
    {
      /* TODO: check if a pure SUBROUTINE has no INTENT(OUT) arguments
	 including an alternate return. In that case it can also be
	 marked as PURE. See also in gfc_get_extern_function_decl().  */
      if (attr.function && !gfc_return_by_reference (sym))
	DECL_PURE_P (fndecl) = 1;
      TREE_SIDE_EFFECTS (fndecl) = 0;
    }


  /* Layout the function declaration and put it in the binding level
     of the current function.  */

  if (global)
    pushdecl_top_level (fndecl);
  else
    pushdecl (fndecl);

  /* Perform name mangling if this is a top level or module procedure.  */
  if (current_function_decl == NULL_TREE)
    gfc_set_decl_assembler_name (fndecl, gfc_sym_mangled_function_id (sym));

  sym->backend_decl = fndecl;
}


/* Create the DECL_ARGUMENTS for a procedure.  */

static void
create_function_arglist (gfc_symbol * sym)
{
  tree fndecl;
  gfc_formal_arglist *f;
  tree typelist, hidden_typelist;
  tree arglist, hidden_arglist;
  tree type;
  tree parm;

  fndecl = sym->backend_decl;

  /* Build formal argument list. Make sure that their TREE_CONTEXT is
     the new FUNCTION_DECL node.  */
  arglist = NULL_TREE;
  hidden_arglist = NULL_TREE;
  typelist = TYPE_ARG_TYPES (TREE_TYPE (fndecl));

  if (sym->attr.entry_master)
    {
      type = TREE_VALUE (typelist);
      parm = build_decl (input_location,
			 PARM_DECL, get_identifier ("__entry"), type);

      DECL_CONTEXT (parm) = fndecl;
      DECL_ARG_TYPE (parm) = type;
      TREE_READONLY (parm) = 1;
      gfc_finish_decl (parm);
      DECL_ARTIFICIAL (parm) = 1;

      arglist = chainon (arglist, parm);
      typelist = TREE_CHAIN (typelist);
    }

  if (gfc_return_by_reference (sym))
    {
      tree type = TREE_VALUE (typelist), length = NULL;

      if (sym->ts.type == BT_CHARACTER)
	{
	  /* Length of character result.  */
	  tree len_type = TREE_VALUE (TREE_CHAIN (typelist));

	  length = build_decl (input_location,
			       PARM_DECL,
			       get_identifier (".__result"),
			       len_type);
	  if (POINTER_TYPE_P (len_type))
	    {
	      sym->ts.u.cl->passed_length = length;
	      TREE_USED (length) = 1;
	    }
	  else if (!sym->ts.u.cl->length)
	    {
	      sym->ts.u.cl->backend_decl = length;
	      TREE_USED (length) = 1;
	    }
	  gcc_assert (TREE_CODE (length) == PARM_DECL);
	  DECL_CONTEXT (length) = fndecl;
	  DECL_ARG_TYPE (length) = len_type;
	  TREE_READONLY (length) = 1;
	  DECL_ARTIFICIAL (length) = 1;
	  gfc_finish_decl (length);
	  if (sym->ts.u.cl->backend_decl == NULL
	      || sym->ts.u.cl->backend_decl == length)
	    {
	      gfc_symbol *arg;
	      tree backend_decl;

	      if (sym->ts.u.cl->backend_decl == NULL)
		{
		  tree len = build_decl (input_location,
					 VAR_DECL,
					 get_identifier ("..__result"),
					 gfc_charlen_type_node);
		  DECL_ARTIFICIAL (len) = 1;
		  TREE_USED (len) = 1;
		  sym->ts.u.cl->backend_decl = len;
		}

	      /* Make sure PARM_DECL type doesn't point to incomplete type.  */
	      arg = sym->result ? sym->result : sym;
	      backend_decl = arg->backend_decl;
	      /* Temporary clear it, so that gfc_sym_type creates complete
		 type.  */
	      arg->backend_decl = NULL;
	      type = gfc_sym_type (arg);
	      arg->backend_decl = backend_decl;
	      type = build_reference_type (type);
	    }
	}

      parm = build_decl (input_location,
			 PARM_DECL, get_identifier ("__result"), type);

      DECL_CONTEXT (parm) = fndecl;
      DECL_ARG_TYPE (parm) = TREE_VALUE (typelist);
      TREE_READONLY (parm) = 1;
      DECL_ARTIFICIAL (parm) = 1;
      gfc_finish_decl (parm);

      arglist = chainon (arglist, parm);
      typelist = TREE_CHAIN (typelist);

      if (sym->ts.type == BT_CHARACTER)
	{
	  gfc_allocate_lang_decl (parm);
	  arglist = chainon (arglist, length);
	  typelist = TREE_CHAIN (typelist);
	}
    }

  hidden_typelist = typelist;
  for (f = gfc_sym_get_dummy_args (sym); f; f = f->next)
    if (f->sym != NULL)	/* Ignore alternate returns.  */
      hidden_typelist = TREE_CHAIN (hidden_typelist);

  for (f = gfc_sym_get_dummy_args (sym); f; f = f->next)
    {
      char name[GFC_MAX_SYMBOL_LEN + 2];

      /* Ignore alternate returns.  */
      if (f->sym == NULL)
	continue;

      type = TREE_VALUE (typelist);

      if (f->sym->ts.type == BT_CHARACTER
	  && (!sym->attr.is_bind_c || sym->attr.entry_master))
	{
	  tree len_type = TREE_VALUE (hidden_typelist);
	  tree length = NULL_TREE;
	  if (!f->sym->ts.deferred)
	    gcc_assert (len_type == gfc_charlen_type_node);
	  else
	    gcc_assert (POINTER_TYPE_P (len_type));

	  strcpy (&name[1], f->sym->name);
	  name[0] = '_';
	  length = build_decl (input_location,
			       PARM_DECL, get_identifier (name), len_type);

	  hidden_arglist = chainon (hidden_arglist, length);
	  DECL_CONTEXT (length) = fndecl;
	  DECL_ARTIFICIAL (length) = 1;
	  DECL_ARG_TYPE (length) = len_type;
	  TREE_READONLY (length) = 1;
	  gfc_finish_decl (length);

	  /* Remember the passed value.  */
          if (!f->sym->ts.u.cl ||  f->sym->ts.u.cl->passed_length)
            {
	      /* This can happen if the same type is used for multiple
		 arguments. We need to copy cl as otherwise
		 cl->passed_length gets overwritten.  */
	      f->sym->ts.u.cl = gfc_new_charlen (f->sym->ns, f->sym->ts.u.cl);
            }
	  f->sym->ts.u.cl->passed_length = length;

	  /* Use the passed value for assumed length variables.  */
	  if (!f->sym->ts.u.cl->length)
	    {
	      TREE_USED (length) = 1;
	      gcc_assert (!f->sym->ts.u.cl->backend_decl);
	      f->sym->ts.u.cl->backend_decl = length;
	    }

	  hidden_typelist = TREE_CHAIN (hidden_typelist);

	  if (f->sym->ts.u.cl->backend_decl == NULL
	      || f->sym->ts.u.cl->backend_decl == length)
	    {
	      if (POINTER_TYPE_P (len_type))
		f->sym->ts.u.cl->backend_decl =
			build_fold_indirect_ref_loc (input_location, length);
	      else if (f->sym->ts.u.cl->backend_decl == NULL)
		gfc_create_string_length (f->sym);

	      /* Make sure PARM_DECL type doesn't point to incomplete type.  */
	      if (f->sym->attr.flavor == FL_PROCEDURE)
		type = build_pointer_type (gfc_get_function_type (f->sym));
	      else
		type = gfc_sym_type (f->sym);
	    }
	}
      /* For noncharacter scalar intrinsic types, VALUE passes the value,
	 hence, the optional status cannot be transferred via a NULL pointer.
	 Thus, we will use a hidden argument in that case.  */
      else if (f->sym->attr.optional && f->sym->attr.value
	       && !f->sym->attr.dimension && f->sym->ts.type != BT_CLASS
	       && !gfc_bt_struct (f->sym->ts.type))
	{
          tree tmp;
          strcpy (&name[1], f->sym->name);
          name[0] = '_';
          tmp = build_decl (input_location,
			    PARM_DECL, get_identifier (name),
			    boolean_type_node);

          hidden_arglist = chainon (hidden_arglist, tmp);
          DECL_CONTEXT (tmp) = fndecl;
          DECL_ARTIFICIAL (tmp) = 1;
          DECL_ARG_TYPE (tmp) = boolean_type_node;
          TREE_READONLY (tmp) = 1;
          gfc_finish_decl (tmp);
	}

      /* For non-constant length array arguments, make sure they use
	 a different type node from TYPE_ARG_TYPES type.  */
      if (f->sym->attr.dimension
	  && type == TREE_VALUE (typelist)
	  && TREE_CODE (type) == POINTER_TYPE
	  && GFC_ARRAY_TYPE_P (type)
	  && f->sym->as->type != AS_ASSUMED_SIZE
	  && ! COMPLETE_TYPE_P (TREE_TYPE (type)))
	{
	  if (f->sym->attr.flavor == FL_PROCEDURE)
	    type = build_pointer_type (gfc_get_function_type (f->sym));
	  else
	    type = gfc_sym_type (f->sym);
	}

      if (f->sym->attr.proc_pointer)
        type = build_pointer_type (type);

      if (f->sym->attr.volatile_)
	type = build_qualified_type (type, TYPE_QUAL_VOLATILE);

      /* Build the argument declaration.  */
      parm = build_decl (input_location,
			 PARM_DECL, gfc_sym_identifier (f->sym), type);

      if (f->sym->attr.volatile_)
	{
	  TREE_THIS_VOLATILE (parm) = 1;
	  TREE_SIDE_EFFECTS (parm) = 1;
	}

      /* Fill in arg stuff.  */
      DECL_CONTEXT (parm) = fndecl;
      DECL_ARG_TYPE (parm) = TREE_VALUE (typelist);
      /* All implementation args except for VALUE are read-only.  */
      if (!f->sym->attr.value)
	TREE_READONLY (parm) = 1;
      if (POINTER_TYPE_P (type)
	  && (!f->sym->attr.proc_pointer
	      && f->sym->attr.flavor != FL_PROCEDURE))
	DECL_BY_REFERENCE (parm) = 1;

      gfc_finish_decl (parm);
      gfc_finish_decl_attrs (parm, &f->sym->attr);

      f->sym->backend_decl = parm;

      /* Coarrays which are descriptorless or assumed-shape pass with
	 -fcoarray=lib the token and the offset as hidden arguments.  */
      if (flag_coarray == GFC_FCOARRAY_LIB
	  && ((f->sym->ts.type != BT_CLASS && f->sym->attr.codimension
	       && !f->sym->attr.allocatable)
	      || (f->sym->ts.type == BT_CLASS
		  && CLASS_DATA (f->sym)->attr.codimension
		  && !CLASS_DATA (f->sym)->attr.allocatable)))
	{
	  tree caf_type;
	  tree token;
	  tree offset;

	  gcc_assert (f->sym->backend_decl != NULL_TREE
		      && !sym->attr.is_bind_c);
	  caf_type = f->sym->ts.type == BT_CLASS
		     ? TREE_TYPE (CLASS_DATA (f->sym)->backend_decl)
		     : TREE_TYPE (f->sym->backend_decl);

	  token = build_decl (input_location, PARM_DECL,
			      create_tmp_var_name ("caf_token"),
			      build_qualified_type (pvoid_type_node,
						    TYPE_QUAL_RESTRICT));
	  if ((f->sym->ts.type != BT_CLASS
	       && f->sym->as->type != AS_DEFERRED)
	      || (f->sym->ts.type == BT_CLASS
		  && CLASS_DATA (f->sym)->as->type != AS_DEFERRED))
	    {
	      gcc_assert (DECL_LANG_SPECIFIC (f->sym->backend_decl) == NULL
			  || GFC_DECL_TOKEN (f->sym->backend_decl) == NULL_TREE);
	      if (DECL_LANG_SPECIFIC (f->sym->backend_decl) == NULL)
		gfc_allocate_lang_decl (f->sym->backend_decl);
	      GFC_DECL_TOKEN (f->sym->backend_decl) = token;
	    }
          else
	    {
	      gcc_assert (GFC_TYPE_ARRAY_CAF_TOKEN (caf_type) == NULL_TREE);
	      GFC_TYPE_ARRAY_CAF_TOKEN (caf_type) = token;
	    }

	  DECL_CONTEXT (token) = fndecl;
	  DECL_ARTIFICIAL (token) = 1;
	  DECL_ARG_TYPE (token) = TREE_VALUE (typelist);
	  TREE_READONLY (token) = 1;
	  hidden_arglist = chainon (hidden_arglist, token);
	  gfc_finish_decl (token);

	  offset = build_decl (input_location, PARM_DECL,
			       create_tmp_var_name ("caf_offset"),
			       gfc_array_index_type);

	  if ((f->sym->ts.type != BT_CLASS
	       && f->sym->as->type != AS_DEFERRED)
	      || (f->sym->ts.type == BT_CLASS
		  && CLASS_DATA (f->sym)->as->type != AS_DEFERRED))
	    {
	      gcc_assert (GFC_DECL_CAF_OFFSET (f->sym->backend_decl)
					       == NULL_TREE);
	      GFC_DECL_CAF_OFFSET (f->sym->backend_decl) = offset;
	    }
	  else
	    {
	      gcc_assert (GFC_TYPE_ARRAY_CAF_OFFSET (caf_type) == NULL_TREE);
	      GFC_TYPE_ARRAY_CAF_OFFSET (caf_type) = offset;
	    }
	  DECL_CONTEXT (offset) = fndecl;
	  DECL_ARTIFICIAL (offset) = 1;
	  DECL_ARG_TYPE (offset) = TREE_VALUE (typelist);
	  TREE_READONLY (offset) = 1;
	  hidden_arglist = chainon (hidden_arglist, offset);
	  gfc_finish_decl (offset);
	}

      arglist = chainon (arglist, parm);
      typelist = TREE_CHAIN (typelist);
    }

  /* Add the hidden string length parameters, unless the procedure
     is bind(C).  */
  if (!sym->attr.is_bind_c)
    arglist = chainon (arglist, hidden_arglist);

  gcc_assert (hidden_typelist == NULL_TREE
              || TREE_VALUE (hidden_typelist) == void_type_node);
  DECL_ARGUMENTS (fndecl) = arglist;
}

/* Do the setup necessary before generating the body of a function.  */

static void
trans_function_start (gfc_symbol * sym)
{
  tree fndecl;

  fndecl = sym->backend_decl;

  /* Let GCC know the current scope is this function.  */
  current_function_decl = fndecl;

  /* Let the world know what we're about to do.  */
  announce_function (fndecl);

  if (DECL_FILE_SCOPE_P (fndecl))
    {
      /* Create RTL for function declaration.  */
      rest_of_decl_compilation (fndecl, 1, 0);
    }

  /* Create RTL for function definition.  */
  make_decl_rtl (fndecl);

  allocate_struct_function (fndecl, false);

  /* function.c requires a push at the start of the function.  */
  pushlevel ();
}

/* Create thunks for alternate entry points.  */

static void
build_entry_thunks (gfc_namespace * ns, bool global)
{
  gfc_formal_arglist *formal;
  gfc_formal_arglist *thunk_formal;
  gfc_entry_list *el;
  gfc_symbol *thunk_sym;
  stmtblock_t body;
  tree thunk_fndecl;
  tree tmp;
  locus old_loc;

  /* This should always be a toplevel function.  */
  gcc_assert (current_function_decl == NULL_TREE);

  gfc_save_backend_locus (&old_loc);
  for (el = ns->entries; el; el = el->next)
    {
      vec<tree, va_gc> *args = NULL;
      vec<tree, va_gc> *string_args = NULL;

      thunk_sym = el->sym;

      build_function_decl (thunk_sym, global);
      create_function_arglist (thunk_sym);

      trans_function_start (thunk_sym);

      thunk_fndecl = thunk_sym->backend_decl;

      gfc_init_block (&body);

      /* Pass extra parameter identifying this entry point.  */
      tmp = build_int_cst (gfc_array_index_type, el->id);
      vec_safe_push (args, tmp);

      if (thunk_sym->attr.function)
	{
	  if (gfc_return_by_reference (ns->proc_name))
	    {
	      tree ref = DECL_ARGUMENTS (current_function_decl);
	      vec_safe_push (args, ref);
	      if (ns->proc_name->ts.type == BT_CHARACTER)
		vec_safe_push (args, DECL_CHAIN (ref));
	    }
	}

      for (formal = gfc_sym_get_dummy_args (ns->proc_name); formal;
	   formal = formal->next)
	{
	  /* Ignore alternate returns.  */
	  if (formal->sym == NULL)
	    continue;

	  /* We don't have a clever way of identifying arguments, so resort to
	     a brute-force search.  */
	  for (thunk_formal = gfc_sym_get_dummy_args (thunk_sym);
	       thunk_formal;
	       thunk_formal = thunk_formal->next)
	    {
	      if (thunk_formal->sym == formal->sym)
		break;
	    }

	  if (thunk_formal)
	    {
	      /* Pass the argument.  */
	      DECL_ARTIFICIAL (thunk_formal->sym->backend_decl) = 1;
	      vec_safe_push (args, thunk_formal->sym->backend_decl);
	      if (formal->sym->ts.type == BT_CHARACTER)
		{
		  tmp = thunk_formal->sym->ts.u.cl->backend_decl;
		  vec_safe_push (string_args, tmp);
		}
	    }
	  else
	    {
	      /* Pass NULL for a missing argument.  */
	      vec_safe_push (args, null_pointer_node);
	      if (formal->sym->ts.type == BT_CHARACTER)
		{
		  tmp = build_int_cst (gfc_charlen_type_node, 0);
		  vec_safe_push (string_args, tmp);
		}
	    }
	}

      /* Call the master function.  */
      vec_safe_splice (args, string_args);
      tmp = ns->proc_name->backend_decl;
      tmp = build_call_expr_loc_vec (input_location, tmp, args);
      if (ns->proc_name->attr.mixed_entry_master)
	{
	  tree union_decl, field;
	  tree master_type = TREE_TYPE (ns->proc_name->backend_decl);

	  union_decl = build_decl (input_location,
				   VAR_DECL, get_identifier ("__result"),
				   TREE_TYPE (master_type));
	  DECL_ARTIFICIAL (union_decl) = 1;
	  DECL_EXTERNAL (union_decl) = 0;
	  TREE_PUBLIC (union_decl) = 0;
	  TREE_USED (union_decl) = 1;
	  layout_decl (union_decl, 0);
	  pushdecl (union_decl);

	  DECL_CONTEXT (union_decl) = current_function_decl;
	  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
				 TREE_TYPE (union_decl), union_decl, tmp);
	  gfc_add_expr_to_block (&body, tmp);

	  for (field = TYPE_FIELDS (TREE_TYPE (union_decl));
	       field; field = DECL_CHAIN (field))
	    if (strcmp (IDENTIFIER_POINTER (DECL_NAME (field)),
		thunk_sym->result->name) == 0)
	      break;
	  gcc_assert (field != NULL_TREE);
	  tmp = fold_build3_loc (input_location, COMPONENT_REF,
				 TREE_TYPE (field), union_decl, field,
				 NULL_TREE);
	  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
			     TREE_TYPE (DECL_RESULT (current_function_decl)),
			     DECL_RESULT (current_function_decl), tmp);
	  tmp = build1_v (RETURN_EXPR, tmp);
	}
      else if (TREE_TYPE (DECL_RESULT (current_function_decl))
	       != void_type_node)
	{
	  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
			     TREE_TYPE (DECL_RESULT (current_function_decl)),
			     DECL_RESULT (current_function_decl), tmp);
	  tmp = build1_v (RETURN_EXPR, tmp);
	}
      gfc_add_expr_to_block (&body, tmp);

      /* Finish off this function and send it for code generation.  */
      DECL_SAVED_TREE (thunk_fndecl) = gfc_finish_block (&body);
      tmp = getdecls ();
      poplevel (1, 1);
      BLOCK_SUPERCONTEXT (DECL_INITIAL (thunk_fndecl)) = thunk_fndecl;
      DECL_SAVED_TREE (thunk_fndecl)
	= build3_v (BIND_EXPR, tmp, DECL_SAVED_TREE (thunk_fndecl),
		    DECL_INITIAL (thunk_fndecl));

      /* Output the GENERIC tree.  */
      dump_function (TDI_original, thunk_fndecl);

      /* Store the end of the function, so that we get good line number
	 info for the epilogue.  */
      cfun->function_end_locus = input_location;

      /* We're leaving the context of this function, so zap cfun.
	 It's still in DECL_STRUCT_FUNCTION, and we'll restore it in
	 tree_rest_of_compilation.  */
      set_cfun (NULL);

      current_function_decl = NULL_TREE;

      cgraph_node::finalize_function (thunk_fndecl, true);

      /* We share the symbols in the formal argument list with other entry
	 points and the master function.  Clear them so that they are
	 recreated for each function.  */
      for (formal = gfc_sym_get_dummy_args (thunk_sym); formal;
	   formal = formal->next)
	if (formal->sym != NULL)  /* Ignore alternate returns.  */
	  {
	    formal->sym->backend_decl = NULL_TREE;
	    if (formal->sym->ts.type == BT_CHARACTER)
	      formal->sym->ts.u.cl->backend_decl = NULL_TREE;
	  }

      if (thunk_sym->attr.function)
	{
	  if (thunk_sym->ts.type == BT_CHARACTER)
	    thunk_sym->ts.u.cl->backend_decl = NULL_TREE;
	  if (thunk_sym->result->ts.type == BT_CHARACTER)
	    thunk_sym->result->ts.u.cl->backend_decl = NULL_TREE;
	}
    }

  gfc_restore_backend_locus (&old_loc);
}


/* Create a decl for a function, and create any thunks for alternate entry
   points. If global is true, generate the function in the global binding
   level, otherwise in the current binding level (which can be global).  */

void
gfc_create_function_decl (gfc_namespace * ns, bool global)
{
  /* Create a declaration for the master function.  */
  build_function_decl (ns->proc_name, global);

  /* Compile the entry thunks.  */
  if (ns->entries)
    build_entry_thunks (ns, global);

  /* Now create the read argument list.  */
  create_function_arglist (ns->proc_name);

  if (ns->omp_declare_simd)
    gfc_trans_omp_declare_simd (ns);
}

/* Return the decl used to hold the function return value.  If
   parent_flag is set, the context is the parent_scope.  */

tree
gfc_get_fake_result_decl (gfc_symbol * sym, int parent_flag)
{
  tree decl;
  tree length;
  tree this_fake_result_decl;
  tree this_function_decl;

  char name[GFC_MAX_SYMBOL_LEN + 10];

  if (parent_flag)
    {
      this_fake_result_decl = parent_fake_result_decl;
      this_function_decl = DECL_CONTEXT (current_function_decl);
    }
  else
    {
      this_fake_result_decl = current_fake_result_decl;
      this_function_decl = current_function_decl;
    }

  if (sym
      && sym->ns->proc_name->backend_decl == this_function_decl
      && sym->ns->proc_name->attr.entry_master
      && sym != sym->ns->proc_name)
    {
      tree t = NULL, var;
      if (this_fake_result_decl != NULL)
	for (t = TREE_CHAIN (this_fake_result_decl); t; t = TREE_CHAIN (t))
	  if (strcmp (IDENTIFIER_POINTER (TREE_PURPOSE (t)), sym->name) == 0)
	    break;
      if (t)
	return TREE_VALUE (t);
      decl = gfc_get_fake_result_decl (sym->ns->proc_name, parent_flag);

      if (parent_flag)
	this_fake_result_decl = parent_fake_result_decl;
      else
	this_fake_result_decl = current_fake_result_decl;

      if (decl && sym->ns->proc_name->attr.mixed_entry_master)
	{
	  tree field;

	  for (field = TYPE_FIELDS (TREE_TYPE (decl));
	       field; field = DECL_CHAIN (field))
	    if (strcmp (IDENTIFIER_POINTER (DECL_NAME (field)),
		sym->name) == 0)
	      break;

	  gcc_assert (field != NULL_TREE);
	  decl = fold_build3_loc (input_location, COMPONENT_REF,
				  TREE_TYPE (field), decl, field, NULL_TREE);
	}

      var = create_tmp_var_raw (TREE_TYPE (decl), sym->name);
      if (parent_flag)
	gfc_add_decl_to_parent_function (var);
      else
	gfc_add_decl_to_function (var);

      SET_DECL_VALUE_EXPR (var, decl);
      DECL_HAS_VALUE_EXPR_P (var) = 1;
      GFC_DECL_RESULT (var) = 1;

      TREE_CHAIN (this_fake_result_decl)
	  = tree_cons (get_identifier (sym->name), var,
		       TREE_CHAIN (this_fake_result_decl));
      return var;
    }

  if (this_fake_result_decl != NULL_TREE)
    return TREE_VALUE (this_fake_result_decl);

  /* Only when gfc_get_fake_result_decl is called by gfc_trans_return,
     sym is NULL.  */
  if (!sym)
    return NULL_TREE;

  if (sym->ts.type == BT_CHARACTER)
    {
      if (sym->ts.u.cl->backend_decl == NULL_TREE)
	length = gfc_create_string_length (sym);
      else
	length = sym->ts.u.cl->backend_decl;
      if (TREE_CODE (length) == VAR_DECL
	  && DECL_CONTEXT (length) == NULL_TREE)
	gfc_add_decl_to_function (length);
    }

  if (gfc_return_by_reference (sym))
    {
      decl = DECL_ARGUMENTS (this_function_decl);

      if (sym->ns->proc_name->backend_decl == this_function_decl
	  && sym->ns->proc_name->attr.entry_master)
	decl = DECL_CHAIN (decl);

      TREE_USED (decl) = 1;
      if (sym->as)
	decl = gfc_build_dummy_array_decl (sym, decl);
    }
  else
    {
      sprintf (name, "__result_%.20s",
	       IDENTIFIER_POINTER (DECL_NAME (this_function_decl)));

      if (!sym->attr.mixed_entry_master && sym->attr.function)
	decl = build_decl (DECL_SOURCE_LOCATION (this_function_decl),
			   VAR_DECL, get_identifier (name),
			   gfc_sym_type (sym));
      else
	decl = build_decl (DECL_SOURCE_LOCATION (this_function_decl),
			   VAR_DECL, get_identifier (name),
			   TREE_TYPE (TREE_TYPE (this_function_decl)));
      DECL_ARTIFICIAL (decl) = 1;
      DECL_EXTERNAL (decl) = 0;
      TREE_PUBLIC (decl) = 0;
      TREE_USED (decl) = 1;
      GFC_DECL_RESULT (decl) = 1;
      TREE_ADDRESSABLE (decl) = 1;

      layout_decl (decl, 0);
      gfc_finish_decl_attrs (decl, &sym->attr);

      if (parent_flag)
	gfc_add_decl_to_parent_function (decl);
      else
	gfc_add_decl_to_function (decl);
    }

  if (parent_flag)
    parent_fake_result_decl = build_tree_list (NULL, decl);
  else
    current_fake_result_decl = build_tree_list (NULL, decl);

  return decl;
}


/* Builds a function decl.  The remaining parameters are the types of the
   function arguments.  Negative nargs indicates a varargs function.  */

static tree
build_library_function_decl_1 (tree name, const char *spec,
			       tree rettype, int nargs, va_list p)
{
  vec<tree, va_gc> *arglist;
  tree fntype;
  tree fndecl;
  int n;

  /* Library functions must be declared with global scope.  */
  gcc_assert (current_function_decl == NULL_TREE);

  /* Create a list of the argument types.  */
  vec_alloc (arglist, abs (nargs));
  for (n = abs (nargs); n > 0; n--)
    {
      tree argtype = va_arg (p, tree);
      arglist->quick_push (argtype);
    }

  /* Build the function type and decl.  */
  if (nargs >= 0)
    fntype = build_function_type_vec (rettype, arglist);
  else
    fntype = build_varargs_function_type_vec (rettype, arglist);
  if (spec)
    {
      tree attr_args = build_tree_list (NULL_TREE,
					build_string (strlen (spec), spec));
      tree attrs = tree_cons (get_identifier ("fn spec"),
			      attr_args, TYPE_ATTRIBUTES (fntype));
      fntype = build_type_attribute_variant (fntype, attrs);
    }
  fndecl = build_decl (input_location,
		       FUNCTION_DECL, name, fntype);

  /* Mark this decl as external.  */
  DECL_EXTERNAL (fndecl) = 1;
  TREE_PUBLIC (fndecl) = 1;

  pushdecl (fndecl);

  rest_of_decl_compilation (fndecl, 1, 0);

  return fndecl;
}

/* Builds a function decl.  The remaining parameters are the types of the
   function arguments.  Negative nargs indicates a varargs function.  */

tree
gfc_build_library_function_decl (tree name, tree rettype, int nargs, ...)
{
  tree ret;
  va_list args;
  va_start (args, nargs);
  ret = build_library_function_decl_1 (name, NULL, rettype, nargs, args);
  va_end (args);
  return ret;
}

/* Builds a function decl.  The remaining parameters are the types of the
   function arguments.  Negative nargs indicates a varargs function.
   The SPEC parameter specifies the function argument and return type
   specification according to the fnspec function type attribute.  */

tree
gfc_build_library_function_decl_with_spec (tree name, const char *spec,
					   tree rettype, int nargs, ...)
{
  tree ret;
  va_list args;
  va_start (args, nargs);
  ret = build_library_function_decl_1 (name, spec, rettype, nargs, args);
  va_end (args);
  return ret;
}

static void
gfc_build_intrinsic_function_decls (void)
{
  tree gfc_int4_type_node = gfc_get_int_type (4);
  tree gfc_pint4_type_node = build_pointer_type (gfc_int4_type_node);
  tree gfc_int8_type_node = gfc_get_int_type (8);
  tree gfc_pint8_type_node = build_pointer_type (gfc_int8_type_node);
  tree gfc_int16_type_node = gfc_get_int_type (16);
  tree gfc_logical4_type_node = gfc_get_logical_type (4);
  tree pchar1_type_node = gfc_get_pchar_type (1);
  tree pchar4_type_node = gfc_get_pchar_type (4);

  /* String functions.  */
  gfor_fndecl_compare_string = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("compare_string")), "..R.R",
	integer_type_node, 4, gfc_charlen_type_node, pchar1_type_node,
	gfc_charlen_type_node, pchar1_type_node);
  DECL_PURE_P (gfor_fndecl_compare_string) = 1;
  TREE_NOTHROW (gfor_fndecl_compare_string) = 1;

  gfor_fndecl_concat_string = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("concat_string")), "..W.R.R",
	void_type_node, 6, gfc_charlen_type_node, pchar1_type_node,
	gfc_charlen_type_node, pchar1_type_node,
	gfc_charlen_type_node, pchar1_type_node);
  TREE_NOTHROW (gfor_fndecl_concat_string) = 1;

  gfor_fndecl_string_len_trim = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_len_trim")), "..R",
	gfc_charlen_type_node, 2, gfc_charlen_type_node, pchar1_type_node);
  DECL_PURE_P (gfor_fndecl_string_len_trim) = 1;
  TREE_NOTHROW (gfor_fndecl_string_len_trim) = 1;

  gfor_fndecl_string_index = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_index")), "..R.R.",
	gfc_charlen_type_node, 5, gfc_charlen_type_node, pchar1_type_node,
	gfc_charlen_type_node, pchar1_type_node, gfc_logical4_type_node);
  DECL_PURE_P (gfor_fndecl_string_index) = 1;
  TREE_NOTHROW (gfor_fndecl_string_index) = 1;

  gfor_fndecl_string_scan = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_scan")), "..R.R.",
	gfc_charlen_type_node, 5, gfc_charlen_type_node, pchar1_type_node,
	gfc_charlen_type_node, pchar1_type_node, gfc_logical4_type_node);
  DECL_PURE_P (gfor_fndecl_string_scan) = 1;
  TREE_NOTHROW (gfor_fndecl_string_scan) = 1;

  gfor_fndecl_string_verify = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_verify")), "..R.R.",
	gfc_charlen_type_node, 5, gfc_charlen_type_node, pchar1_type_node,
	gfc_charlen_type_node, pchar1_type_node, gfc_logical4_type_node);
  DECL_PURE_P (gfor_fndecl_string_verify) = 1;
  TREE_NOTHROW (gfor_fndecl_string_verify) = 1;

  gfor_fndecl_string_trim = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_trim")), ".Ww.R",
	void_type_node, 4, build_pointer_type (gfc_charlen_type_node),
	build_pointer_type (pchar1_type_node), gfc_charlen_type_node,
	pchar1_type_node);

  gfor_fndecl_string_minmax = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_minmax")), ".Ww.R",
	void_type_node, -4, build_pointer_type (gfc_charlen_type_node),
	build_pointer_type (pchar1_type_node), integer_type_node,
	integer_type_node);

  gfor_fndecl_adjustl = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("adjustl")), ".W.R",
	void_type_node, 3, pchar1_type_node, gfc_charlen_type_node,
	pchar1_type_node);
  TREE_NOTHROW (gfor_fndecl_adjustl) = 1;

  gfor_fndecl_adjustr = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("adjustr")), ".W.R",
	void_type_node, 3, pchar1_type_node, gfc_charlen_type_node,
	pchar1_type_node);
  TREE_NOTHROW (gfor_fndecl_adjustr) = 1;

  gfor_fndecl_select_string =  gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("select_string")), ".R.R.",
	integer_type_node, 4, pvoid_type_node, integer_type_node,
	pchar1_type_node, gfc_charlen_type_node);
  DECL_PURE_P (gfor_fndecl_select_string) = 1;
  TREE_NOTHROW (gfor_fndecl_select_string) = 1;

  gfor_fndecl_compare_string_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("compare_string_char4")), "..R.R",
	integer_type_node, 4, gfc_charlen_type_node, pchar4_type_node,
	gfc_charlen_type_node, pchar4_type_node);
  DECL_PURE_P (gfor_fndecl_compare_string_char4) = 1;
  TREE_NOTHROW (gfor_fndecl_compare_string_char4) = 1;

  gfor_fndecl_concat_string_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("concat_string_char4")), "..W.R.R",
	void_type_node, 6, gfc_charlen_type_node, pchar4_type_node,
	gfc_charlen_type_node, pchar4_type_node, gfc_charlen_type_node,
	pchar4_type_node);
  TREE_NOTHROW (gfor_fndecl_concat_string_char4) = 1;

  gfor_fndecl_string_len_trim_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_len_trim_char4")), "..R",
	gfc_charlen_type_node, 2, gfc_charlen_type_node, pchar4_type_node);
  DECL_PURE_P (gfor_fndecl_string_len_trim_char4) = 1;
  TREE_NOTHROW (gfor_fndecl_string_len_trim_char4) = 1;

  gfor_fndecl_string_index_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_index_char4")), "..R.R.",
	gfc_charlen_type_node, 5, gfc_charlen_type_node, pchar4_type_node,
	gfc_charlen_type_node, pchar4_type_node, gfc_logical4_type_node);
  DECL_PURE_P (gfor_fndecl_string_index_char4) = 1;
  TREE_NOTHROW (gfor_fndecl_string_index_char4) = 1;

  gfor_fndecl_string_scan_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_scan_char4")), "..R.R.",
	gfc_charlen_type_node, 5, gfc_charlen_type_node, pchar4_type_node,
	gfc_charlen_type_node, pchar4_type_node, gfc_logical4_type_node);
  DECL_PURE_P (gfor_fndecl_string_scan_char4) = 1;
  TREE_NOTHROW (gfor_fndecl_string_scan_char4) = 1;

  gfor_fndecl_string_verify_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_verify_char4")), "..R.R.",
	gfc_charlen_type_node, 5, gfc_charlen_type_node, pchar4_type_node,
	gfc_charlen_type_node, pchar4_type_node, gfc_logical4_type_node);
  DECL_PURE_P (gfor_fndecl_string_verify_char4) = 1;
  TREE_NOTHROW (gfor_fndecl_string_verify_char4) = 1;

  gfor_fndecl_string_trim_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_trim_char4")), ".Ww.R",
	void_type_node, 4, build_pointer_type (gfc_charlen_type_node),
	build_pointer_type (pchar4_type_node), gfc_charlen_type_node,
	pchar4_type_node);

  gfor_fndecl_string_minmax_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("string_minmax_char4")), ".Ww.R",
	void_type_node, -4, build_pointer_type (gfc_charlen_type_node),
	build_pointer_type (pchar4_type_node), integer_type_node,
	integer_type_node);

  gfor_fndecl_adjustl_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("adjustl_char4")), ".W.R",
	void_type_node, 3, pchar4_type_node, gfc_charlen_type_node,
	pchar4_type_node);
  TREE_NOTHROW (gfor_fndecl_adjustl_char4) = 1;

  gfor_fndecl_adjustr_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("adjustr_char4")), ".W.R",
	void_type_node, 3, pchar4_type_node, gfc_charlen_type_node,
	pchar4_type_node);
  TREE_NOTHROW (gfor_fndecl_adjustr_char4) = 1;

  gfor_fndecl_select_string_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("select_string_char4")), ".R.R.",
	integer_type_node, 4, pvoid_type_node, integer_type_node,
	pvoid_type_node, gfc_charlen_type_node);
  DECL_PURE_P (gfor_fndecl_select_string_char4) = 1;
  TREE_NOTHROW (gfor_fndecl_select_string_char4) = 1;


  /* Conversion between character kinds.  */

  gfor_fndecl_convert_char1_to_char4 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("convert_char1_to_char4")), ".w.R",
	void_type_node, 3, build_pointer_type (pchar4_type_node),
	gfc_charlen_type_node, pchar1_type_node);

  gfor_fndecl_convert_char4_to_char1 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("convert_char4_to_char1")), ".w.R",
	void_type_node, 3, build_pointer_type (pchar1_type_node),
	gfc_charlen_type_node, pchar4_type_node);

  /* Misc. functions.  */

  gfor_fndecl_ttynam = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("ttynam")), ".W",
	void_type_node, 3, pchar_type_node, gfc_charlen_type_node,
	integer_type_node);

  gfor_fndecl_fdate = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("fdate")), ".W",
	void_type_node, 2, pchar_type_node, gfc_charlen_type_node);

  gfor_fndecl_ctime = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("ctime")), ".W",
	void_type_node, 3, pchar_type_node, gfc_charlen_type_node,
	gfc_int8_type_node);

  gfor_fndecl_sc_kind = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("selected_char_kind")), "..R",
	gfc_int4_type_node, 2, gfc_charlen_type_node, pchar_type_node);
  DECL_PURE_P (gfor_fndecl_sc_kind) = 1;
  TREE_NOTHROW (gfor_fndecl_sc_kind) = 1;

  gfor_fndecl_si_kind = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("selected_int_kind")), ".R",
	gfc_int4_type_node, 1, pvoid_type_node);
  DECL_PURE_P (gfor_fndecl_si_kind) = 1;
  TREE_NOTHROW (gfor_fndecl_si_kind) = 1;

  gfor_fndecl_sr_kind = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("selected_real_kind2008")), ".RR",
	gfc_int4_type_node, 3, pvoid_type_node, pvoid_type_node,
	pvoid_type_node);
  DECL_PURE_P (gfor_fndecl_sr_kind) = 1;
  TREE_NOTHROW (gfor_fndecl_sr_kind) = 1;

  gfor_fndecl_system_clock4 = gfc_build_library_function_decl (
	get_identifier (PREFIX("system_clock_4")),
	void_type_node, 3, gfc_pint4_type_node, gfc_pint4_type_node,
	gfc_pint4_type_node);

  gfor_fndecl_system_clock8 = gfc_build_library_function_decl (
	get_identifier (PREFIX("system_clock_8")),
	void_type_node, 3, gfc_pint8_type_node, gfc_pint8_type_node,
	gfc_pint8_type_node);

  /* Power functions.  */
  {
    tree ctype, rtype, itype, jtype;
    int rkind, ikind, jkind;
#define NIKINDS 3
#define NRKINDS 4
    static int ikinds[NIKINDS] = {4, 8, 16};
    static int rkinds[NRKINDS] = {4, 8, 10, 16};
    char name[PREFIX_LEN + 12]; /* _gfortran_pow_?n_?n */

    for (ikind=0; ikind < NIKINDS; ikind++)
      {
	itype = gfc_get_int_type (ikinds[ikind]);

	for (jkind=0; jkind < NIKINDS; jkind++)
	  {
	    jtype = gfc_get_int_type (ikinds[jkind]);
	    if (itype && jtype)
	      {
		sprintf(name, PREFIX("pow_i%d_i%d"), ikinds[ikind],
			ikinds[jkind]);
		gfor_fndecl_math_powi[jkind][ikind].integer =
		  gfc_build_library_function_decl (get_identifier (name),
		    jtype, 2, jtype, itype);
		TREE_READONLY (gfor_fndecl_math_powi[jkind][ikind].integer) = 1;
		TREE_NOTHROW (gfor_fndecl_math_powi[jkind][ikind].integer) = 1;
	      }
	  }

	for (rkind = 0; rkind < NRKINDS; rkind ++)
	  {
	    rtype = gfc_get_real_type (rkinds[rkind]);
	    if (rtype && itype)
	      {
		sprintf(name, PREFIX("pow_r%d_i%d"), rkinds[rkind],
			ikinds[ikind]);
		gfor_fndecl_math_powi[rkind][ikind].real =
		  gfc_build_library_function_decl (get_identifier (name),
		    rtype, 2, rtype, itype);
		TREE_READONLY (gfor_fndecl_math_powi[rkind][ikind].real) = 1;
		TREE_NOTHROW (gfor_fndecl_math_powi[rkind][ikind].real) = 1;
	      }

	    ctype = gfc_get_complex_type (rkinds[rkind]);
	    if (ctype && itype)
	      {
		sprintf(name, PREFIX("pow_c%d_i%d"), rkinds[rkind],
			ikinds[ikind]);
		gfor_fndecl_math_powi[rkind][ikind].cmplx =
		  gfc_build_library_function_decl (get_identifier (name),
		    ctype, 2,ctype, itype);
		TREE_READONLY (gfor_fndecl_math_powi[rkind][ikind].cmplx) = 1;
		TREE_NOTHROW (gfor_fndecl_math_powi[rkind][ikind].cmplx) = 1;
	      }
	  }
      }
#undef NIKINDS
#undef NRKINDS
  }

  gfor_fndecl_math_ishftc4 = gfc_build_library_function_decl (
	get_identifier (PREFIX("ishftc4")),
	gfc_int4_type_node, 3, gfc_int4_type_node, gfc_int4_type_node,
	gfc_int4_type_node);
  TREE_READONLY (gfor_fndecl_math_ishftc4) = 1;
  TREE_NOTHROW (gfor_fndecl_math_ishftc4) = 1;

  gfor_fndecl_math_ishftc8 = gfc_build_library_function_decl (
	get_identifier (PREFIX("ishftc8")),
	gfc_int8_type_node, 3, gfc_int8_type_node, gfc_int4_type_node,
	gfc_int4_type_node);
  TREE_READONLY (gfor_fndecl_math_ishftc8) = 1;
  TREE_NOTHROW (gfor_fndecl_math_ishftc8) = 1;

  if (gfc_int16_type_node)
    {
      gfor_fndecl_math_ishftc16 = gfc_build_library_function_decl (
	get_identifier (PREFIX("ishftc16")),
	gfc_int16_type_node, 3, gfc_int16_type_node, gfc_int4_type_node,
	gfc_int4_type_node);
      TREE_READONLY (gfor_fndecl_math_ishftc16) = 1;
      TREE_NOTHROW (gfor_fndecl_math_ishftc16) = 1;
    }

  /* BLAS functions.  */
  {
    tree pint = build_pointer_type (integer_type_node);
    tree ps = build_pointer_type (gfc_get_real_type (gfc_default_real_kind));
    tree pd = build_pointer_type (gfc_get_real_type (gfc_default_double_kind));
    tree pc = build_pointer_type (gfc_get_complex_type (gfc_default_real_kind));
    tree pz = build_pointer_type
		(gfc_get_complex_type (gfc_default_double_kind));

    gfor_fndecl_sgemm = gfc_build_library_function_decl
			  (get_identifier
			     (flag_underscoring ? "sgemm_" : "sgemm"),
			   void_type_node, 15, pchar_type_node,
			   pchar_type_node, pint, pint, pint, ps, ps, pint,
			   ps, pint, ps, ps, pint, integer_type_node,
			   integer_type_node);
    gfor_fndecl_dgemm = gfc_build_library_function_decl
			  (get_identifier
			     (flag_underscoring ? "dgemm_" : "dgemm"),
			   void_type_node, 15, pchar_type_node,
			   pchar_type_node, pint, pint, pint, pd, pd, pint,
			   pd, pint, pd, pd, pint, integer_type_node,
			   integer_type_node);
    gfor_fndecl_cgemm = gfc_build_library_function_decl
			  (get_identifier
			     (flag_underscoring ? "cgemm_" : "cgemm"),
			   void_type_node, 15, pchar_type_node,
			   pchar_type_node, pint, pint, pint, pc, pc, pint,
			   pc, pint, pc, pc, pint, integer_type_node,
			   integer_type_node);
    gfor_fndecl_zgemm = gfc_build_library_function_decl
			  (get_identifier
			     (flag_underscoring ? "zgemm_" : "zgemm"),
			   void_type_node, 15, pchar_type_node,
			   pchar_type_node, pint, pint, pint, pz, pz, pint,
			   pz, pint, pz, pz, pint, integer_type_node,
			   integer_type_node);
  }

  /* Other functions.  */
  gfor_fndecl_size0 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("size0")), ".R",
	gfc_array_index_type, 1, pvoid_type_node);
  DECL_PURE_P (gfor_fndecl_size0) = 1;
  TREE_NOTHROW (gfor_fndecl_size0) = 1;

  gfor_fndecl_size1 = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("size1")), ".R",
	gfc_array_index_type, 2, pvoid_type_node, gfc_array_index_type);
  DECL_PURE_P (gfor_fndecl_size1) = 1;
  TREE_NOTHROW (gfor_fndecl_size1) = 1;

  gfor_fndecl_iargc = gfc_build_library_function_decl (
	get_identifier (PREFIX ("iargc")), gfc_int4_type_node, 0);
  TREE_NOTHROW (gfor_fndecl_iargc) = 1;
}


/* Make prototypes for runtime library functions.  */

void
gfc_build_builtin_function_decls (void)
{
  tree gfc_int4_type_node = gfc_get_int_type (4);

  gfor_fndecl_stop_numeric = gfc_build_library_function_decl (
	get_identifier (PREFIX("stop_numeric")),
	void_type_node, 1, gfc_int4_type_node);
  /* STOP doesn't return.  */
  TREE_THIS_VOLATILE (gfor_fndecl_stop_numeric) = 1;

  gfor_fndecl_stop_numeric_f08 = gfc_build_library_function_decl (
	get_identifier (PREFIX("stop_numeric_f08")),
	void_type_node, 1, gfc_int4_type_node);
  /* STOP doesn't return.  */
  TREE_THIS_VOLATILE (gfor_fndecl_stop_numeric_f08) = 1;

  gfor_fndecl_stop_string = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("stop_string")), ".R.",
	void_type_node, 2, pchar_type_node, gfc_int4_type_node);
  /* STOP doesn't return.  */
  TREE_THIS_VOLATILE (gfor_fndecl_stop_string) = 1;

  gfor_fndecl_error_stop_numeric = gfc_build_library_function_decl (
        get_identifier (PREFIX("error_stop_numeric")),
        void_type_node, 1, gfc_int4_type_node);
  /* ERROR STOP doesn't return.  */
  TREE_THIS_VOLATILE (gfor_fndecl_error_stop_numeric) = 1;

  gfor_fndecl_error_stop_string = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("error_stop_string")), ".R.",
	void_type_node, 2, pchar_type_node, gfc_int4_type_node);
  /* ERROR STOP doesn't return.  */
  TREE_THIS_VOLATILE (gfor_fndecl_error_stop_string) = 1;

  gfor_fndecl_pause_numeric = gfc_build_library_function_decl (
	get_identifier (PREFIX("pause_numeric")),
	void_type_node, 1, gfc_int4_type_node);

  gfor_fndecl_pause_string = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("pause_string")), ".R.",
	void_type_node, 2, pchar_type_node, gfc_int4_type_node);

  gfor_fndecl_runtime_error = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("runtime_error")), ".R",
	void_type_node, -1, pchar_type_node);
  /* The runtime_error function does not return.  */
  TREE_THIS_VOLATILE (gfor_fndecl_runtime_error) = 1;

  gfor_fndecl_runtime_error_at = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("runtime_error_at")), ".RR",
	void_type_node, -2, pchar_type_node, pchar_type_node);
  /* The runtime_error_at function does not return.  */
  TREE_THIS_VOLATILE (gfor_fndecl_runtime_error_at) = 1;

  gfor_fndecl_runtime_warning_at = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("runtime_warning_at")), ".RR",
	void_type_node, -2, pchar_type_node, pchar_type_node);

  gfor_fndecl_generate_error = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("generate_error")), ".R.R",
	void_type_node, 3, pvoid_type_node, integer_type_node,
	pchar_type_node);

  gfor_fndecl_os_error = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("os_error")), ".R",
	void_type_node, 1, pchar_type_node);
  /* The runtime_error function does not return.  */
  TREE_THIS_VOLATILE (gfor_fndecl_os_error) = 1;

  gfor_fndecl_set_args = gfc_build_library_function_decl (
	get_identifier (PREFIX("set_args")),
	void_type_node, 2, integer_type_node,
	build_pointer_type (pchar_type_node));

  gfor_fndecl_set_fpe = gfc_build_library_function_decl (
	get_identifier (PREFIX("set_fpe")),
	void_type_node, 1, integer_type_node);

  gfor_fndecl_ieee_procedure_entry = gfc_build_library_function_decl (
	get_identifier (PREFIX("ieee_procedure_entry")),
	void_type_node, 1, pvoid_type_node);

  gfor_fndecl_ieee_procedure_exit = gfc_build_library_function_decl (
	get_identifier (PREFIX("ieee_procedure_exit")),
	void_type_node, 1, pvoid_type_node);

  /* Keep the array dimension in sync with the call, later in this file.  */
  gfor_fndecl_set_options = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("set_options")), "..R",
	void_type_node, 2, integer_type_node,
	build_pointer_type (integer_type_node));

  gfor_fndecl_set_convert = gfc_build_library_function_decl (
	get_identifier (PREFIX("set_convert")),
	void_type_node, 1, integer_type_node);

  gfor_fndecl_set_record_marker = gfc_build_library_function_decl (
	get_identifier (PREFIX("set_record_marker")),
	void_type_node, 1, integer_type_node);

  gfor_fndecl_set_max_subrecord_length = gfc_build_library_function_decl (
	get_identifier (PREFIX("set_max_subrecord_length")),
	void_type_node, 1, integer_type_node);

  gfor_fndecl_in_pack = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("internal_pack")), ".r",
	pvoid_type_node, 1, pvoid_type_node);

  gfor_fndecl_in_unpack = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("internal_unpack")), ".wR",
	void_type_node, 2, pvoid_type_node, pvoid_type_node);

  gfor_fndecl_associated = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("associated")), ".RR",
	integer_type_node, 2, ppvoid_type_node, ppvoid_type_node);
  DECL_PURE_P (gfor_fndecl_associated) = 1;
  TREE_NOTHROW (gfor_fndecl_associated) = 1;

  /* Coarray library calls.  */
  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tree pint_type, pppchar_type;

      pint_type = build_pointer_type (integer_type_node);
      pppchar_type
	= build_pointer_type (build_pointer_type (pchar_type_node));

      gfor_fndecl_caf_init = gfc_build_library_function_decl (
	get_identifier (PREFIX("caf_init")), void_type_node,
	2, pint_type, pppchar_type);

      gfor_fndecl_caf_finalize = gfc_build_library_function_decl (
	get_identifier (PREFIX("caf_finalize")), void_type_node, 0);

      gfor_fndecl_caf_this_image = gfc_build_library_function_decl (
	get_identifier (PREFIX("caf_this_image")), integer_type_node,
	1, integer_type_node);

      gfor_fndecl_caf_num_images = gfc_build_library_function_decl (
	get_identifier (PREFIX("caf_num_images")), integer_type_node,
	2, integer_type_node, integer_type_node);

      gfor_fndecl_caf_register = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_register")), "RRWWWWR", void_type_node, 7,
	size_type_node, integer_type_node, ppvoid_type_node, pvoid_type_node,
	pint_type, pchar_type_node, integer_type_node);

      gfor_fndecl_caf_deregister = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_deregister")), "WWWR", void_type_node, 4,
	ppvoid_type_node, pint_type, pchar_type_node, integer_type_node);

      gfor_fndecl_caf_get = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_get")), ".R.RRWRRRW", void_type_node, 10,
	pvoid_type_node, size_type_node, integer_type_node, pvoid_type_node,
	pvoid_type_node, pvoid_type_node, integer_type_node, integer_type_node,
	boolean_type_node, pint_type);

      gfor_fndecl_caf_send = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_send")), ".R.RRRRRRW", void_type_node, 10,
	pvoid_type_node, size_type_node, integer_type_node, pvoid_type_node,
	pvoid_type_node, pvoid_type_node, integer_type_node, integer_type_node,
	boolean_type_node, pint_type);

      gfor_fndecl_caf_sendget = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_sendget")), ".R.RRRR.RRRRRR",
	void_type_node,	14, pvoid_type_node, size_type_node, integer_type_node,
	pvoid_type_node, pvoid_type_node, pvoid_type_node, size_type_node,
	integer_type_node, pvoid_type_node, pvoid_type_node, integer_type_node,
	integer_type_node, boolean_type_node, integer_type_node);

      gfor_fndecl_caf_get_by_ref = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_get_by_ref")), ".RWRRRRRW", void_type_node,
	9, pvoid_type_node, integer_type_node, pvoid_type_node, pvoid_type_node,
	integer_type_node, integer_type_node, boolean_type_node,
	boolean_type_node, pint_type);

      gfor_fndecl_caf_send_by_ref = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_send_by_ref")), ".RRRRRRRW", void_type_node,
	9, pvoid_type_node, integer_type_node, pvoid_type_node, pvoid_type_node,
	integer_type_node, integer_type_node, boolean_type_node,
	boolean_type_node, pint_type);

      gfor_fndecl_caf_sendget_by_ref
	  = gfc_build_library_function_decl_with_spec (
	    get_identifier (PREFIX("caf_sendget_by_ref")), ".RR.RRRRRWW",
	    void_type_node, 11, pvoid_type_node, integer_type_node,
	    pvoid_type_node, pvoid_type_node, integer_type_node,
	    pvoid_type_node, integer_type_node, integer_type_node,
	    boolean_type_node, pint_type, pint_type);

      gfor_fndecl_caf_sync_all = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_sync_all")), ".WW", void_type_node,
	3, pint_type, pchar_type_node, integer_type_node);

      gfor_fndecl_caf_sync_memory = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_sync_memory")), ".WW", void_type_node,
	3, pint_type, pchar_type_node, integer_type_node);

      gfor_fndecl_caf_sync_images = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_sync_images")), ".RRWW", void_type_node,
	5, integer_type_node, pint_type, pint_type,
	pchar_type_node, integer_type_node);

      gfor_fndecl_caf_error_stop = gfc_build_library_function_decl (
	get_identifier (PREFIX("caf_error_stop")),
	void_type_node, 1, gfc_int4_type_node);
      /* CAF's ERROR STOP doesn't return.  */
      TREE_THIS_VOLATILE (gfor_fndecl_caf_error_stop) = 1;

      gfor_fndecl_caf_error_stop_str = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_error_stop_str")), ".R.",
	void_type_node, 2, pchar_type_node, gfc_int4_type_node);
      /* CAF's ERROR STOP doesn't return.  */
      TREE_THIS_VOLATILE (gfor_fndecl_caf_error_stop_str) = 1;

      gfor_fndecl_caf_stop_numeric = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_stop_numeric")), ".R.",
	void_type_node, 1, gfc_int4_type_node);
      /* CAF's STOP doesn't return.  */
      TREE_THIS_VOLATILE (gfor_fndecl_caf_stop_numeric) = 1;

      gfor_fndecl_caf_stop_str = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_stop_str")), ".R.",
	void_type_node, 2, pchar_type_node, gfc_int4_type_node);
      /* CAF's STOP doesn't return.  */
      TREE_THIS_VOLATILE (gfor_fndecl_caf_stop_str) = 1;

      gfor_fndecl_caf_atomic_def = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_atomic_define")), "R..RW",
	void_type_node, 7, pvoid_type_node, size_type_node, integer_type_node,
	pvoid_type_node, pint_type, integer_type_node, integer_type_node);

      gfor_fndecl_caf_atomic_ref = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_atomic_ref")), "R..WW",
	void_type_node, 7, pvoid_type_node, size_type_node, integer_type_node,
	pvoid_type_node, pint_type, integer_type_node, integer_type_node);

      gfor_fndecl_caf_atomic_cas = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_atomic_cas")), "R..WRRW",
	void_type_node, 9, pvoid_type_node, size_type_node, integer_type_node,
	pvoid_type_node, pvoid_type_node, pvoid_type_node, pint_type,
	integer_type_node, integer_type_node);

      gfor_fndecl_caf_atomic_op = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_atomic_op")), ".R..RWW",
	void_type_node, 9, integer_type_node, pvoid_type_node, size_type_node,
	integer_type_node, pvoid_type_node, pvoid_type_node, pint_type,
	integer_type_node, integer_type_node);

      gfor_fndecl_caf_lock = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_lock")), "R..WWW",
	void_type_node, 7, pvoid_type_node, size_type_node, integer_type_node,
	pint_type, pint_type, pchar_type_node, integer_type_node);

      gfor_fndecl_caf_unlock = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_unlock")), "R..WW",
	void_type_node, 6, pvoid_type_node, size_type_node, integer_type_node,
	pint_type, pchar_type_node, integer_type_node);

      gfor_fndecl_caf_event_post = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_event_post")), "R..WW",
	void_type_node, 6, pvoid_type_node, size_type_node, integer_type_node,
	pint_type, pchar_type_node, integer_type_node);

      gfor_fndecl_caf_event_wait = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_event_wait")), "R..WW",
	void_type_node, 6, pvoid_type_node, size_type_node, integer_type_node,
	pint_type, pchar_type_node, integer_type_node);

      gfor_fndecl_caf_event_query = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_event_query")), "R..WW",
	void_type_node, 5, pvoid_type_node, size_type_node, integer_type_node,
	pint_type, pint_type);

      gfor_fndecl_co_broadcast = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_co_broadcast")), "W.WW",
	void_type_node, 5, pvoid_type_node, integer_type_node,
	pint_type, pchar_type_node, integer_type_node);

      gfor_fndecl_co_max = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_co_max")), "W.WW",
	void_type_node, 6, pvoid_type_node, integer_type_node,
	pint_type, pchar_type_node, integer_type_node, integer_type_node);

      gfor_fndecl_co_min = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_co_min")), "W.WW",
	void_type_node, 6, pvoid_type_node, integer_type_node,
	pint_type, pchar_type_node, integer_type_node, integer_type_node);

      gfor_fndecl_co_reduce = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_co_reduce")), "W.R.WW",
	void_type_node, 8, pvoid_type_node,
	build_pointer_type (build_varargs_function_type_list (void_type_node,
							      NULL_TREE)),
	integer_type_node, integer_type_node, pint_type, pchar_type_node,
	integer_type_node, integer_type_node);

      gfor_fndecl_co_sum = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("caf_co_sum")), "W.WW",
	void_type_node, 5, pvoid_type_node, integer_type_node,
	pint_type, pchar_type_node, integer_type_node);
    }

  gfc_build_intrinsic_function_decls ();
  gfc_build_intrinsic_lib_fndecls ();
  gfc_build_io_library_fndecls ();
}


/* Evaluate the length of dummy character variables.  */

static void
gfc_trans_dummy_character (gfc_symbol *sym, gfc_charlen *cl,
			   gfc_wrapped_block *block)
{
  stmtblock_t init;

  gfc_finish_decl (cl->backend_decl);

  gfc_start_block (&init);

  /* Evaluate the string length expression.  */
  gfc_conv_string_length (cl, NULL, &init);

  gfc_trans_vla_type_sizes (sym, &init);

  gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
}


/* Allocate and cleanup an automatic character variable.  */

static void
gfc_trans_auto_character_variable (gfc_symbol * sym, gfc_wrapped_block * block)
{
  stmtblock_t init;
  tree decl;
  tree tmp;

  gcc_assert (sym->backend_decl);
  gcc_assert (sym->ts.u.cl && sym->ts.u.cl->length);

  gfc_init_block (&init);

  /* Evaluate the string length expression.  */
  gfc_conv_string_length (sym->ts.u.cl, NULL, &init);

  gfc_trans_vla_type_sizes (sym, &init);

  decl = sym->backend_decl;

  /* Emit a DECL_EXPR for this variable, which will cause the
     gimplifier to allocate storage, and all that good stuff.  */
  tmp = fold_build1_loc (input_location, DECL_EXPR, TREE_TYPE (decl), decl);
  gfc_add_expr_to_block (&init, tmp);

  gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
}

/* Set the initial value of ASSIGN statement auxiliary variable explicitly.  */

static void
gfc_trans_assign_aux_var (gfc_symbol * sym, gfc_wrapped_block * block)
{
  stmtblock_t init;

  gcc_assert (sym->backend_decl);
  gfc_start_block (&init);

  /* Set the initial value to length. See the comments in
     function gfc_add_assign_aux_vars in this file.  */
  gfc_add_modify (&init, GFC_DECL_STRING_LEN (sym->backend_decl),
		  build_int_cst (gfc_charlen_type_node, -2));

  gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
}

static void
gfc_trans_vla_one_sizepos (tree *tp, stmtblock_t *body)
{
  tree t = *tp, var, val;

  if (t == NULL || t == error_mark_node)
    return;
  if (TREE_CONSTANT (t) || DECL_P (t))
    return;

  if (TREE_CODE (t) == SAVE_EXPR)
    {
      if (SAVE_EXPR_RESOLVED_P (t))
	{
	  *tp = TREE_OPERAND (t, 0);
	  return;
	}
      val = TREE_OPERAND (t, 0);
    }
  else
    val = t;

  var = gfc_create_var_np (TREE_TYPE (t), NULL);
  gfc_add_decl_to_function (var);
  gfc_add_modify (body, var, unshare_expr (val));
  if (TREE_CODE (t) == SAVE_EXPR)
    TREE_OPERAND (t, 0) = var;
  *tp = var;
}

static void
gfc_trans_vla_type_sizes_1 (tree type, stmtblock_t *body)
{
  tree t;

  if (type == NULL || type == error_mark_node)
    return;

  type = TYPE_MAIN_VARIANT (type);

  if (TREE_CODE (type) == INTEGER_TYPE)
    {
      gfc_trans_vla_one_sizepos (&TYPE_MIN_VALUE (type), body);
      gfc_trans_vla_one_sizepos (&TYPE_MAX_VALUE (type), body);

      for (t = TYPE_NEXT_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	{
	  TYPE_MIN_VALUE (t) = TYPE_MIN_VALUE (type);
	  TYPE_MAX_VALUE (t) = TYPE_MAX_VALUE (type);
	}
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      gfc_trans_vla_type_sizes_1 (TREE_TYPE (type), body);
      gfc_trans_vla_type_sizes_1 (TYPE_DOMAIN (type), body);
      gfc_trans_vla_one_sizepos (&TYPE_SIZE (type), body);
      gfc_trans_vla_one_sizepos (&TYPE_SIZE_UNIT (type), body);

      for (t = TYPE_NEXT_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	{
	  TYPE_SIZE (t) = TYPE_SIZE (type);
	  TYPE_SIZE_UNIT (t) = TYPE_SIZE_UNIT (type);
	}
    }
}

/* Make sure all type sizes and array domains are either constant,
   or variable or parameter decls.  This is a simplified variant
   of gimplify_type_sizes, but we can't use it here, as none of the
   variables in the expressions have been gimplified yet.
   As type sizes and domains for various variable length arrays
   contain VAR_DECLs that are only initialized at gfc_trans_deferred_vars
   time, without this routine gimplify_type_sizes in the middle-end
   could result in the type sizes being gimplified earlier than where
   those variables are initialized.  */

void
gfc_trans_vla_type_sizes (gfc_symbol *sym, stmtblock_t *body)
{
  tree type = TREE_TYPE (sym->backend_decl);

  if (TREE_CODE (type) == FUNCTION_TYPE
      && (sym->attr.function || sym->attr.result || sym->attr.entry))
    {
      if (! current_fake_result_decl)
	return;

      type = TREE_TYPE (TREE_VALUE (current_fake_result_decl));
    }

  while (POINTER_TYPE_P (type))
    type = TREE_TYPE (type);

  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      tree etype = GFC_TYPE_ARRAY_DATAPTR_TYPE (type);

      while (POINTER_TYPE_P (etype))
	etype = TREE_TYPE (etype);

      gfc_trans_vla_type_sizes_1 (etype, body);
    }

  gfc_trans_vla_type_sizes_1 (type, body);
}


/* Initialize a derived type by building an lvalue from the symbol
   and using trans_assignment to do the work. Set dealloc to false
   if no deallocation prior the assignment is needed.  */
void
gfc_init_default_dt (gfc_symbol * sym, stmtblock_t * block, bool dealloc)
{
  gfc_expr *e;
  tree tmp;
  tree present;

  gcc_assert (block);

  gcc_assert (!sym->attr.allocatable);
  gfc_set_sym_referenced (sym);
  e = gfc_lval_expr_from_sym (sym);
  tmp = gfc_trans_assignment (e, sym->value, false, dealloc);
  if (sym->attr.dummy && (sym->attr.optional
			  || sym->ns->proc_name->attr.entry_master))
    {
      present = gfc_conv_expr_present (sym);
      tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp), present,
			tmp, build_empty_stmt (input_location));
    }
  gfc_add_expr_to_block (block, tmp);
  gfc_free_expr (e);
}


/* Initialize INTENT(OUT) derived type dummies.  As well as giving
   them their default initializer, if they do not have allocatable
   components, they have their allocatable components deallocated.  */

static void
init_intent_out_dt (gfc_symbol * proc_sym, gfc_wrapped_block * block)
{
  stmtblock_t init;
  gfc_formal_arglist *f;
  tree tmp;
  tree present;

  gfc_init_block (&init);
  for (f = gfc_sym_get_dummy_args (proc_sym); f; f = f->next)
    if (f->sym && f->sym->attr.intent == INTENT_OUT
	&& !f->sym->attr.pointer
	&& f->sym->ts.type == BT_DERIVED)
      {
	tmp = NULL_TREE;

	/* Note: Allocatables are excluded as they are already handled
	   by the caller.  */
	if (!f->sym->attr.allocatable
	    && gfc_is_finalizable (f->sym->ts.u.derived, NULL))
	  {
	    stmtblock_t block;
	    gfc_expr *e;

	    gfc_init_block (&block);
	    f->sym->attr.referenced = 1;
	    e = gfc_lval_expr_from_sym (f->sym);
	    gfc_add_finalizer_call (&block, e);
	    gfc_free_expr (e);
	    tmp = gfc_finish_block (&block);
	  }

	if (tmp == NULL_TREE && !f->sym->attr.allocatable
	    && f->sym->ts.u.derived->attr.alloc_comp && !f->sym->value)
	  tmp = gfc_deallocate_alloc_comp (f->sym->ts.u.derived,
					   f->sym->backend_decl,
					   f->sym->as ? f->sym->as->rank : 0);

	if (tmp != NULL_TREE && (f->sym->attr.optional
				 || f->sym->ns->proc_name->attr.entry_master))
	  {
	    present = gfc_conv_expr_present (f->sym);
	    tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp),
			      present, tmp, build_empty_stmt (input_location));
	  }

	if (tmp != NULL_TREE)
	  gfc_add_expr_to_block (&init, tmp);
	else if (f->sym->value && !f->sym->attr.allocatable)
	  gfc_init_default_dt (f->sym, &init, true);
      }
    else if (f->sym && f->sym->attr.intent == INTENT_OUT
	     && f->sym->ts.type == BT_CLASS
	     && !CLASS_DATA (f->sym)->attr.class_pointer
	     && !CLASS_DATA (f->sym)->attr.allocatable)
      {
	stmtblock_t block;
	gfc_expr *e;

	gfc_init_block (&block);
	f->sym->attr.referenced = 1;
	e = gfc_lval_expr_from_sym (f->sym);
	gfc_add_finalizer_call (&block, e);
	gfc_free_expr (e);
	tmp = gfc_finish_block (&block);

	if (f->sym->attr.optional || f->sym->ns->proc_name->attr.entry_master)
	  {
	    present = gfc_conv_expr_present (f->sym);
	    tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp),
			      present, tmp,
			      build_empty_stmt (input_location));
	  }

	gfc_add_expr_to_block (&init, tmp);
      }

  gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
}


/* Helper function to manage deferred string lengths.  */

static tree
gfc_null_and_pass_deferred_len (gfc_symbol *sym, stmtblock_t *init,
			        locus *loc)
{
  tree tmp;

  /* Character length passed by reference.  */
  tmp = sym->ts.u.cl->passed_length;
  tmp = build_fold_indirect_ref_loc (input_location, tmp);
  tmp = fold_convert (gfc_charlen_type_node, tmp);

  if (!sym->attr.dummy || sym->attr.intent == INTENT_OUT)
    /* Zero the string length when entering the scope.  */
    gfc_add_modify (init, sym->ts.u.cl->backend_decl,
		    build_int_cst (gfc_charlen_type_node, 0));
  else
    {
      tree tmp2;

      tmp2 = fold_build2_loc (input_location, MODIFY_EXPR,
			      gfc_charlen_type_node,
			      sym->ts.u.cl->backend_decl, tmp);
      if (sym->attr.optional)
	{
	  tree present = gfc_conv_expr_present (sym);
	  tmp2 = build3_loc (input_location, COND_EXPR,
			     void_type_node, present, tmp2,
			     build_empty_stmt (input_location));
	}
      gfc_add_expr_to_block (init, tmp2);
    }

  gfc_restore_backend_locus (loc);

  /* Pass the final character length back.  */
  if (sym->attr.intent != INTENT_IN)
    {
      tmp = fold_build2_loc (input_location, MODIFY_EXPR,
			     gfc_charlen_type_node, tmp,
			     sym->ts.u.cl->backend_decl);
      if (sym->attr.optional)
	{
	  tree present = gfc_conv_expr_present (sym);
	  tmp = build3_loc (input_location, COND_EXPR,
			    void_type_node, present, tmp,
			    build_empty_stmt (input_location));
	}
    }
  else
    tmp = NULL_TREE;

  return tmp;
}

/* Generate function entry and exit code, and add it to the function body.
   This includes:
    Allocation and initialization of array variables.
    Allocation of character string variables.
    Initialization and possibly repacking of dummy arrays.
    Initialization of ASSIGN statement auxiliary variable.
    Initialization of ASSOCIATE names.
    Automatic deallocation.  */

void
gfc_trans_deferred_vars (gfc_symbol * proc_sym, gfc_wrapped_block * block)
{
  locus loc;
  gfc_symbol *sym;
  gfc_formal_arglist *f;
  stmtblock_t tmpblock;
  bool seen_trans_deferred_array = false;
  tree tmp = NULL;
  gfc_expr *e;
  gfc_se se;
  stmtblock_t init;

  /* Deal with implicit return variables.  Explicit return variables will
     already have been added.  */
  if (gfc_return_by_reference (proc_sym) && proc_sym->result == proc_sym)
    {
      if (!current_fake_result_decl)
	{
	  gfc_entry_list *el = NULL;
	  if (proc_sym->attr.entry_master)
	    {
	      for (el = proc_sym->ns->entries; el; el = el->next)
		if (el->sym != el->sym->result)
		  break;
	    }
	  /* TODO: move to the appropriate place in resolve.c.  */
	  if (warn_return_type && el == NULL)
	    gfc_warning (OPT_Wreturn_type,
			 "Return value of function %qs at %L not set",
			 proc_sym->name, &proc_sym->declared_at);
	}
      else if (proc_sym->as)
	{
	  tree result = TREE_VALUE (current_fake_result_decl);
	  gfc_save_backend_locus (&loc);
	  gfc_set_backend_locus (&proc_sym->declared_at);
	  gfc_trans_dummy_array_bias (proc_sym, result, block);

	  /* An automatic character length, pointer array result.  */
	  if (proc_sym->ts.type == BT_CHARACTER
		&& TREE_CODE (proc_sym->ts.u.cl->backend_decl) == VAR_DECL)
	    {
	      tmp = NULL;
	      if (proc_sym->ts.deferred)
		{
		  gfc_start_block (&init);
		  tmp = gfc_null_and_pass_deferred_len (proc_sym, &init, &loc);
		  gfc_add_init_cleanup (block, gfc_finish_block (&init), tmp);
		}
	      else
		gfc_trans_dummy_character (proc_sym, proc_sym->ts.u.cl, block);
	    }
	}
      else if (proc_sym->ts.type == BT_CHARACTER)
	{
	  if (proc_sym->ts.deferred)
	    {
	      tmp = NULL;
	      gfc_save_backend_locus (&loc);
	      gfc_set_backend_locus (&proc_sym->declared_at);
	      gfc_start_block (&init);
	      /* Zero the string length on entry.  */
	      gfc_add_modify (&init, proc_sym->ts.u.cl->backend_decl,
			      build_int_cst (gfc_charlen_type_node, 0));
	      /* Null the pointer.  */
	      e = gfc_lval_expr_from_sym (proc_sym);
	      gfc_init_se (&se, NULL);
	      se.want_pointer = 1;
	      gfc_conv_expr (&se, e);
	      gfc_free_expr (e);
	      tmp = se.expr;
	      gfc_add_modify (&init, tmp,
			      fold_convert (TREE_TYPE (se.expr),
					    null_pointer_node));
	      gfc_restore_backend_locus (&loc);

	      /* Pass back the string length on exit.  */
	      tmp = proc_sym->ts.u.cl->backend_decl;
	      if (TREE_CODE (tmp) != INDIRECT_REF
		  && proc_sym->ts.u.cl->passed_length)
		{
		  tmp = proc_sym->ts.u.cl->passed_length;
		  tmp = build_fold_indirect_ref_loc (input_location, tmp);
		  tmp = fold_convert (gfc_charlen_type_node, tmp);
		  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
					 gfc_charlen_type_node, tmp,
					 proc_sym->ts.u.cl->backend_decl);
		}
	      else
		tmp = NULL_TREE;

	      gfc_add_init_cleanup (block, gfc_finish_block (&init), tmp);
	    }
	  else if (TREE_CODE (proc_sym->ts.u.cl->backend_decl) == VAR_DECL)
	    gfc_trans_dummy_character (proc_sym, proc_sym->ts.u.cl, block);
	}
      else
	gcc_assert (flag_f2c && proc_sym->ts.type == BT_COMPLEX);
    }

  /* Initialize the INTENT(OUT) derived type dummy arguments.  This
     should be done here so that the offsets and lbounds of arrays
     are available.  */
  gfc_save_backend_locus (&loc);
  gfc_set_backend_locus (&proc_sym->declared_at);
  init_intent_out_dt (proc_sym, block);
  gfc_restore_backend_locus (&loc);

  for (sym = proc_sym->tlink; sym != proc_sym; sym = sym->tlink)
    {
      bool alloc_comp_or_fini = (sym->ts.type == BT_DERIVED)
				&& (sym->ts.u.derived->attr.alloc_comp
				    || gfc_is_finalizable (sym->ts.u.derived,
							   NULL));
      if (sym->assoc)
	continue;

      if (sym->attr.subref_array_pointer
	  && GFC_DECL_SPAN (sym->backend_decl)
	  && !TREE_STATIC (GFC_DECL_SPAN (sym->backend_decl)))
	{
	  gfc_init_block (&tmpblock);
	  gfc_add_modify (&tmpblock, GFC_DECL_SPAN (sym->backend_decl),
			  build_int_cst (gfc_array_index_type, 0));
	  gfc_add_init_cleanup (block, gfc_finish_block (&tmpblock),
				NULL_TREE);
	}

      if (sym->ts.type == BT_CLASS
	  && (sym->attr.save || flag_max_stack_var_size == 0)
	  && CLASS_DATA (sym)->attr.allocatable)
	{
	  tree vptr;

          if (UNLIMITED_POLY (sym))
	    vptr = null_pointer_node;
	  else
	    {
	      gfc_symbol *vsym;
	      vsym = gfc_find_derived_vtab (sym->ts.u.derived);
	      vptr = gfc_get_symbol_decl (vsym);
	      vptr = gfc_build_addr_expr (NULL, vptr);
	    }

	  if (CLASS_DATA (sym)->attr.dimension
	      || (CLASS_DATA (sym)->attr.codimension
		  && flag_coarray != GFC_FCOARRAY_LIB))
	    {
	      tmp = gfc_class_data_get (sym->backend_decl);
	      tmp = gfc_build_null_descriptor (TREE_TYPE (tmp));
	    }
	  else
	    tmp = null_pointer_node;

	  DECL_INITIAL (sym->backend_decl)
		= gfc_class_set_static_fields (sym->backend_decl, vptr, tmp);
	  TREE_CONSTANT (DECL_INITIAL (sym->backend_decl)) = 1;
	}
      else if ((sym->attr.dimension || sym->attr.codimension
	       || (IS_CLASS_ARRAY (sym) && !CLASS_DATA (sym)->attr.allocatable)))
	{
	  bool is_classarray = IS_CLASS_ARRAY (sym);
	  symbol_attribute *array_attr;
	  gfc_array_spec *as;
	  array_type type_of_array;

	  array_attr = is_classarray ? &CLASS_DATA (sym)->attr : &sym->attr;
	  as = is_classarray ? CLASS_DATA (sym)->as : sym->as;
	  /* Assumed-size Cray pointees need to be treated as AS_EXPLICIT.  */
	  type_of_array = as->type;
	  if (type_of_array == AS_ASSUMED_SIZE && as->cp_was_assumed)
	    type_of_array = AS_EXPLICIT;
	  switch (type_of_array)
	    {
	    case AS_EXPLICIT:
	      if (sym->attr.dummy || sym->attr.result)
		gfc_trans_dummy_array_bias (sym, sym->backend_decl, block);
	      /* Allocatable and pointer arrays need to processed
		 explicitly.  */
	      else if ((sym->ts.type != BT_CLASS && sym->attr.pointer)
		       || (sym->ts.type == BT_CLASS
			   && CLASS_DATA (sym)->attr.class_pointer)
		       || array_attr->allocatable)
		{
		  if (TREE_STATIC (sym->backend_decl))
		    {
		      gfc_save_backend_locus (&loc);
		      gfc_set_backend_locus (&sym->declared_at);
		      gfc_trans_static_array_pointer (sym);
		      gfc_restore_backend_locus (&loc);
		    }
		  else
		    {
		      seen_trans_deferred_array = true;
		      gfc_trans_deferred_array (sym, block);
		    }
		}
	      else if (sym->attr.codimension
		       && TREE_STATIC (sym->backend_decl))
		{
		  gfc_init_block (&tmpblock);
		  gfc_trans_array_cobounds (TREE_TYPE (sym->backend_decl),
					    &tmpblock, sym);
		  gfc_add_init_cleanup (block, gfc_finish_block (&tmpblock),
					NULL_TREE);
		  continue;
		}
	      else
		{
		  gfc_save_backend_locus (&loc);
		  gfc_set_backend_locus (&sym->declared_at);

		  if (alloc_comp_or_fini)
		    {
		      seen_trans_deferred_array = true;
		      gfc_trans_deferred_array (sym, block);
		    }
		  else if (sym->ts.type == BT_DERIVED
			     && sym->value
			     && !sym->attr.data
			     && sym->attr.save == SAVE_NONE)
		    {
		      gfc_start_block (&tmpblock);
		      gfc_init_default_dt (sym, &tmpblock, false);
		      gfc_add_init_cleanup (block,
					    gfc_finish_block (&tmpblock),
					    NULL_TREE);
		    }

		  gfc_trans_auto_array_allocation (sym->backend_decl,
						   sym, block);
		  gfc_restore_backend_locus (&loc);
		}
	      break;

	    case AS_ASSUMED_SIZE:
	      /* Must be a dummy parameter.  */
	      gcc_assert (sym->attr.dummy || as->cp_was_assumed);

	      /* We should always pass assumed size arrays the g77 way.  */
	      if (sym->attr.dummy)
		gfc_trans_g77_array (sym, block);
	      break;

	    case AS_ASSUMED_SHAPE:
	      /* Must be a dummy parameter.  */
	      gcc_assert (sym->attr.dummy);

	      gfc_trans_dummy_array_bias (sym, sym->backend_decl, block);
	      break;

	    case AS_ASSUMED_RANK:
	    case AS_DEFERRED:
	      seen_trans_deferred_array = true;
	      gfc_trans_deferred_array (sym, block);
	      if (sym->ts.type == BT_CHARACTER && sym->ts.deferred
		  && sym->attr.result)
		{
		  gfc_start_block (&init);
		  gfc_save_backend_locus (&loc);
		  gfc_set_backend_locus (&sym->declared_at);
		  tmp = gfc_null_and_pass_deferred_len (sym, &init, &loc);
		  gfc_add_init_cleanup (block, gfc_finish_block (&init), tmp);
		}
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  if (alloc_comp_or_fini && !seen_trans_deferred_array)
	    gfc_trans_deferred_array (sym, block);
	}
      else if ((!sym->attr.dummy || sym->ts.deferred)
		&& (sym->ts.type == BT_CLASS
		&& CLASS_DATA (sym)->attr.class_pointer))
	continue;
      else if ((!sym->attr.dummy || sym->ts.deferred)
		&& (sym->attr.allocatable
		    || (sym->attr.pointer && sym->attr.result)
		    || (sym->ts.type == BT_CLASS
			&& CLASS_DATA (sym)->attr.allocatable)))
	{
	  if (!sym->attr.save && flag_max_stack_var_size != 0)
	    {
	      tree descriptor = NULL_TREE;

	      gfc_save_backend_locus (&loc);
	      gfc_set_backend_locus (&sym->declared_at);
	      gfc_start_block (&init);

	      if (!sym->attr.pointer)
		{
		  /* Nullify and automatic deallocation of allocatable
		     scalars.  */
		  e = gfc_lval_expr_from_sym (sym);
		  if (sym->ts.type == BT_CLASS)
		    gfc_add_data_component (e);

		  gfc_init_se (&se, NULL);
		  if (sym->ts.type != BT_CLASS
		      || sym->ts.u.derived->attr.dimension
		      || sym->ts.u.derived->attr.codimension)
		    {
		      se.want_pointer = 1;
		      gfc_conv_expr (&se, e);
		    }
		  else if (sym->ts.type == BT_CLASS
			   && !CLASS_DATA (sym)->attr.dimension
			   && !CLASS_DATA (sym)->attr.codimension)
		    {
		      se.want_pointer = 1;
		      gfc_conv_expr (&se, e);
		    }
		  else
		    {
		      se.descriptor_only = 1;
		      gfc_conv_expr (&se, e);
		      descriptor = se.expr;
		      se.expr = gfc_conv_descriptor_data_addr (se.expr);
		      se.expr = build_fold_indirect_ref_loc (input_location, se.expr);
		    }
		  gfc_free_expr (e);

		  if (!sym->attr.dummy || sym->attr.intent == INTENT_OUT)
		    {
		      /* Nullify when entering the scope.  */
		      tmp = fold_build2_loc (input_location, MODIFY_EXPR,
					     TREE_TYPE (se.expr), se.expr,
					     fold_convert (TREE_TYPE (se.expr),
							   null_pointer_node));
		      if (sym->attr.optional)
			{
			  tree present = gfc_conv_expr_present (sym);
			  tmp = build3_loc (input_location, COND_EXPR,
					    void_type_node, present, tmp,
					    build_empty_stmt (input_location));
			}
		      gfc_add_expr_to_block (&init, tmp);
		    }
		}

	      if ((sym->attr.dummy || sym->attr.result)
		    && sym->ts.type == BT_CHARACTER
		    && sym->ts.deferred
		    && sym->ts.u.cl->passed_length)
		tmp = gfc_null_and_pass_deferred_len (sym, &init, &loc);
	      else
		gfc_restore_backend_locus (&loc);

	      /* Deallocate when leaving the scope. Nullifying is not
		 needed.  */
	      if (!sym->attr.result && !sym->attr.dummy && !sym->attr.pointer
		  && !sym->ns->proc_name->attr.is_main_program)
		{
		  if (sym->ts.type == BT_CLASS
		      && CLASS_DATA (sym)->attr.codimension)
		    tmp = gfc_deallocate_with_status (descriptor, NULL_TREE,
						      NULL_TREE, NULL_TREE,
						      NULL_TREE, true, NULL,
						      true);
		  else
		    {
		      gfc_expr *expr = gfc_lval_expr_from_sym (sym);
		      tmp = gfc_deallocate_scalar_with_status (se.expr, NULL_TREE,
						   true, expr, sym->ts);
		      gfc_free_expr (expr);
		    }
		}

	      if (sym->ts.type == BT_CLASS)
		{
		  /* Initialize _vptr to declared type.  */
		  gfc_symbol *vtab;
		  tree rhs;

		  gfc_save_backend_locus (&loc);
		  gfc_set_backend_locus (&sym->declared_at);
		  e = gfc_lval_expr_from_sym (sym);
		  gfc_add_vptr_component (e);
		  gfc_init_se (&se, NULL);
		  se.want_pointer = 1;
		  gfc_conv_expr (&se, e);
		  gfc_free_expr (e);
		  if (UNLIMITED_POLY (sym))
		    rhs = build_int_cst (TREE_TYPE (se.expr), 0);
		  else
		    {
		      vtab = gfc_find_derived_vtab (sym->ts.u.derived);
		      rhs = gfc_build_addr_expr (TREE_TYPE (se.expr),
						gfc_get_symbol_decl (vtab));
		    }
		  gfc_add_modify (&init, se.expr, rhs);
		  gfc_restore_backend_locus (&loc);
		}

	      gfc_add_init_cleanup (block, gfc_finish_block (&init), tmp);
	    }
	}
      else if (sym->ts.type == BT_CHARACTER && sym->ts.deferred)
	{
	  tree tmp = NULL;
	  stmtblock_t init;

	  /* If we get to here, all that should be left are pointers.  */
	  gcc_assert (sym->attr.pointer);

	  if (sym->attr.dummy)
	    {
	      gfc_start_block (&init);
	      gfc_save_backend_locus (&loc);
	      gfc_set_backend_locus (&sym->declared_at);
	      tmp = gfc_null_and_pass_deferred_len (sym, &init, &loc);
	      gfc_add_init_cleanup (block, gfc_finish_block (&init), tmp);
	    }
	}
      else if (sym->ts.deferred)
	gfc_fatal_error ("Deferred type parameter not yet supported");
      else if (alloc_comp_or_fini)
	gfc_trans_deferred_array (sym, block);
      else if (sym->ts.type == BT_CHARACTER)
	{
	  gfc_save_backend_locus (&loc);
	  gfc_set_backend_locus (&sym->declared_at);
	  if (sym->attr.dummy || sym->attr.result)
	    gfc_trans_dummy_character (sym, sym->ts.u.cl, block);
	  else
	    gfc_trans_auto_character_variable (sym, block);
	  gfc_restore_backend_locus (&loc);
	}
      else if (sym->attr.assign)
	{
	  gfc_save_backend_locus (&loc);
	  gfc_set_backend_locus (&sym->declared_at);
	  gfc_trans_assign_aux_var (sym, block);
	  gfc_restore_backend_locus (&loc);
	}
      else if (sym->ts.type == BT_DERIVED
		 && sym->value
		 && !sym->attr.data
		 && sym->attr.save == SAVE_NONE)
	{
	  gfc_start_block (&tmpblock);
	  gfc_init_default_dt (sym, &tmpblock, false);
	  gfc_add_init_cleanup (block, gfc_finish_block (&tmpblock),
				NULL_TREE);
	}
      else if (!(UNLIMITED_POLY(sym)))
	gcc_unreachable ();
    }

  gfc_init_block (&tmpblock);

  for (f = gfc_sym_get_dummy_args (proc_sym); f; f = f->next)
    {
      if (f->sym && f->sym->tlink == NULL && f->sym->ts.type == BT_CHARACTER)
	{
	  gcc_assert (f->sym->ts.u.cl->backend_decl != NULL);
	  if (TREE_CODE (f->sym->ts.u.cl->backend_decl) == PARM_DECL)
	    gfc_trans_vla_type_sizes (f->sym, &tmpblock);
	}
    }

  if (gfc_return_by_reference (proc_sym) && proc_sym->ts.type == BT_CHARACTER
      && current_fake_result_decl != NULL)
    {
      gcc_assert (proc_sym->ts.u.cl->backend_decl != NULL);
      if (TREE_CODE (proc_sym->ts.u.cl->backend_decl) == PARM_DECL)
	gfc_trans_vla_type_sizes (proc_sym, &tmpblock);
    }

  gfc_add_init_cleanup (block, gfc_finish_block (&tmpblock), NULL_TREE);
}


struct module_hasher : ggc_ptr_hash<module_htab_entry>
{
  typedef const char *compare_type;

  static hashval_t hash (module_htab_entry *s) { return htab_hash_string (s); }
  static bool
  equal (module_htab_entry *a, const char *b)
  {
    return !strcmp (a->name, b);
  }
};

static GTY (()) hash_table<module_hasher> *module_htab;

/* Hash and equality functions for module_htab's decls.  */

hashval_t
module_decl_hasher::hash (tree t)
{
  const_tree n = DECL_NAME (t);
  if (n == NULL_TREE)
    n = TYPE_NAME (TREE_TYPE (t));
  return htab_hash_string (IDENTIFIER_POINTER (n));
}

bool
module_decl_hasher::equal (tree t1, const char *x2)
{
  const_tree n1 = DECL_NAME (t1);
  if (n1 == NULL_TREE)
    n1 = TYPE_NAME (TREE_TYPE (t1));
  return strcmp (IDENTIFIER_POINTER (n1), x2) == 0;
}

struct module_htab_entry *
gfc_find_module (const char *name)
{
  if (! module_htab)
    module_htab = hash_table<module_hasher>::create_ggc (10);

  module_htab_entry **slot
    = module_htab->find_slot_with_hash (name, htab_hash_string (name), INSERT);
  if (*slot == NULL)
    {
      module_htab_entry *entry = ggc_cleared_alloc<module_htab_entry> ();

      entry->name = gfc_get_string (name);
      entry->decls = hash_table<module_decl_hasher>::create_ggc (10);
      *slot = entry;
    }
  return *slot;
}

void
gfc_module_add_decl (struct module_htab_entry *entry, tree decl)
{
  const char *name;

  if (DECL_NAME (decl))
    name = IDENTIFIER_POINTER (DECL_NAME (decl));
  else
    {
      gcc_assert (TREE_CODE (decl) == TYPE_DECL);
      name = IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (decl)));
    }
  tree *slot
    = entry->decls->find_slot_with_hash (name, htab_hash_string (name),
					 INSERT);
  if (*slot == NULL)
    *slot = decl;
}


/* Generate debugging symbols for namelists. This function must come after
   generate_local_decl to ensure that the variables in the namelist are
   already declared.  */

static tree
generate_namelist_decl (gfc_symbol * sym)
{
  gfc_namelist *nml;
  tree decl;
  vec<constructor_elt, va_gc> *nml_decls = NULL;

  gcc_assert (sym->attr.flavor == FL_NAMELIST);
  for (nml = sym->namelist; nml; nml = nml->next)
    {
      if (nml->sym->backend_decl == NULL_TREE)
	{
	  nml->sym->attr.referenced = 1;
	  nml->sym->backend_decl = gfc_get_symbol_decl (nml->sym);
	}
      DECL_IGNORED_P (nml->sym->backend_decl) = 0;
      CONSTRUCTOR_APPEND_ELT (nml_decls, NULL_TREE, nml->sym->backend_decl);
    }

  decl = make_node (NAMELIST_DECL);
  TREE_TYPE (decl) = void_type_node;
  NAMELIST_DECL_ASSOCIATED_DECL (decl) = build_constructor (NULL_TREE, nml_decls);
  DECL_NAME (decl) = get_identifier (sym->name);
  return decl;
}


/* Output an initialized decl for a module variable.  */

static void
gfc_create_module_variable (gfc_symbol * sym)
{
  tree decl;

  /* Module functions with alternate entries are dealt with later and
     would get caught by the next condition.  */
  if (sym->attr.entry)
    return;

  /* Make sure we convert the types of the derived types from iso_c_binding
     into (void *).  */
  if (sym->attr.flavor != FL_PROCEDURE && sym->attr.is_iso_c
      && sym->ts.type == BT_DERIVED)
    sym->backend_decl = gfc_typenode_for_spec (&(sym->ts));

  if (gfc_fl_struct (sym->attr.flavor)
      && sym->backend_decl
      && TREE_CODE (sym->backend_decl) == RECORD_TYPE)
    {
      decl = sym->backend_decl;
      gcc_assert (sym->ns->proc_name->attr.flavor == FL_MODULE);

      if (!sym->attr.use_assoc && !sym->attr.used_in_submodule)
	{
	  gcc_assert (TYPE_CONTEXT (decl) == NULL_TREE
		      || TYPE_CONTEXT (decl) == sym->ns->proc_name->backend_decl);
	  gcc_assert (DECL_CONTEXT (TYPE_STUB_DECL (decl)) == NULL_TREE
		      || DECL_CONTEXT (TYPE_STUB_DECL (decl))
			   == sym->ns->proc_name->backend_decl);
	}
      TYPE_CONTEXT (decl) = sym->ns->proc_name->backend_decl;
      DECL_CONTEXT (TYPE_STUB_DECL (decl)) = sym->ns->proc_name->backend_decl;
      gfc_module_add_decl (cur_module, TYPE_STUB_DECL (decl));
    }

  /* Only output variables, procedure pointers and array valued,
     or derived type, parameters.  */
  if (sym->attr.flavor != FL_VARIABLE
	&& !(sym->attr.flavor == FL_PARAMETER
	       && (sym->attr.dimension || sym->ts.type == BT_DERIVED))
	&& !(sym->attr.flavor == FL_PROCEDURE && sym->attr.proc_pointer))
    return;

  if ((sym->attr.in_common || sym->attr.in_equivalence) && sym->backend_decl)
    {
      decl = sym->backend_decl;
      gcc_assert (DECL_FILE_SCOPE_P (decl));
      gcc_assert (sym->ns->proc_name->attr.flavor == FL_MODULE);
      DECL_CONTEXT (decl) = sym->ns->proc_name->backend_decl;
      gfc_module_add_decl (cur_module, decl);
    }

  /* Don't generate variables from other modules. Variables from
     COMMONs and Cray pointees will already have been generated.  */
  if (sym->attr.use_assoc || sym->attr.used_in_submodule
      || sym->attr.in_common || sym->attr.cray_pointee)
    return;

  /* Equivalenced variables arrive here after creation.  */
  if (sym->backend_decl
      && (sym->equiv_built || sym->attr.in_equivalence))
    return;

  if (sym->backend_decl && !sym->attr.vtab && !sym->attr.target)
    gfc_internal_error ("backend decl for module variable %qs already exists",
			sym->name);

  if (sym->module && !sym->attr.result && !sym->attr.dummy
      && (sym->attr.access == ACCESS_UNKNOWN
	  && (sym->ns->default_access == ACCESS_PRIVATE
	      || (sym->ns->default_access == ACCESS_UNKNOWN
		  && flag_module_private))))
    sym->attr.access = ACCESS_PRIVATE;

  if (warn_unused_variable && !sym->attr.referenced
      && sym->attr.access == ACCESS_PRIVATE)
    gfc_warning (OPT_Wunused_value,
		 "Unused PRIVATE module variable %qs declared at %L",
		 sym->name, &sym->declared_at);

  /* We always want module variables to be created.  */
  sym->attr.referenced = 1;
  /* Create the decl.  */
  decl = gfc_get_symbol_decl (sym);

  /* Create the variable.  */
  pushdecl (decl);
  gcc_assert (sym->ns->proc_name->attr.flavor == FL_MODULE);
  DECL_CONTEXT (decl) = sym->ns->proc_name->backend_decl;
  rest_of_decl_compilation (decl, 1, 0);
  gfc_module_add_decl (cur_module, decl);

  /* Also add length of strings.  */
  if (sym->ts.type == BT_CHARACTER)
    {
      tree length;

      length = sym->ts.u.cl->backend_decl;
      gcc_assert (length || sym->attr.proc_pointer);
      if (length && !INTEGER_CST_P (length))
        {
          pushdecl (length);
          rest_of_decl_compilation (length, 1, 0);
        }
    }

  if (sym->attr.codimension && !sym->attr.dummy && !sym->attr.allocatable
      && sym->attr.referenced && !sym->attr.use_assoc)
    has_coarray_vars = true;
}

/* Emit debug information for USE statements.  */

static void
gfc_trans_use_stmts (gfc_namespace * ns)
{
  gfc_use_list *use_stmt;
  for (use_stmt = ns->use_stmts; use_stmt; use_stmt = use_stmt->next)
    {
      struct module_htab_entry *entry
	= gfc_find_module (use_stmt->module_name);
      gfc_use_rename *rent;

      if (entry->namespace_decl == NULL)
	{
	  entry->namespace_decl
	    = build_decl (input_location,
			  NAMESPACE_DECL,
			  get_identifier (use_stmt->module_name),
			  void_type_node);
	  DECL_EXTERNAL (entry->namespace_decl) = 1;
	}
      gfc_set_backend_locus (&use_stmt->where);
      if (!use_stmt->only_flag)
	(*debug_hooks->imported_module_or_decl) (entry->namespace_decl,
						 NULL_TREE,
						 ns->proc_name->backend_decl,
						 false);
      for (rent = use_stmt->rename; rent; rent = rent->next)
	{
	  tree decl, local_name;

	  if (rent->op != INTRINSIC_NONE)
	    continue;

						 hashval_t hash = htab_hash_string (rent->use_name);
	  tree *slot = entry->decls->find_slot_with_hash (rent->use_name, hash,
							  INSERT);
	  if (*slot == NULL)
	    {
	      gfc_symtree *st;

	      st = gfc_find_symtree (ns->sym_root,
				     rent->local_name[0]
				     ? rent->local_name : rent->use_name);

	      /* The following can happen if a derived type is renamed.  */
	      if (!st)
		{
		  char *name;
		  name = xstrdup (rent->local_name[0]
				  ? rent->local_name : rent->use_name);
		  name[0] = (char) TOUPPER ((unsigned char) name[0]);
		  st = gfc_find_symtree (ns->sym_root, name);
		  free (name);
		  gcc_assert (st);
		}

	      /* Sometimes, generic interfaces wind up being over-ruled by a
		 local symbol (see PR41062).  */
	      if (!st->n.sym->attr.use_assoc)
		continue;

	      if (st->n.sym->backend_decl
		  && DECL_P (st->n.sym->backend_decl)
		  && st->n.sym->module
		  && strcmp (st->n.sym->module, use_stmt->module_name) == 0)
		{
		  gcc_assert (DECL_EXTERNAL (entry->namespace_decl)
			      || (TREE_CODE (st->n.sym->backend_decl)
				  != VAR_DECL));
		  decl = copy_node (st->n.sym->backend_decl);
		  DECL_CONTEXT (decl) = entry->namespace_decl;
		  DECL_EXTERNAL (decl) = 1;
		  DECL_IGNORED_P (decl) = 0;
		  DECL_INITIAL (decl) = NULL_TREE;
		}
	      else if (st->n.sym->attr.flavor == FL_NAMELIST
		       && st->n.sym->attr.use_only
		       && st->n.sym->module
		       && strcmp (st->n.sym->module, use_stmt->module_name)
			  == 0)
		{
		  decl = generate_namelist_decl (st->n.sym);
		  DECL_CONTEXT (decl) = entry->namespace_decl;
		  DECL_EXTERNAL (decl) = 1;
		  DECL_IGNORED_P (decl) = 0;
		  DECL_INITIAL (decl) = NULL_TREE;
		}
	      else
		{
		  *slot = error_mark_node;
		  entry->decls->clear_slot (slot);
		  continue;
		}
	      *slot = decl;
	    }
	  decl = (tree) *slot;
	  if (rent->local_name[0])
	    local_name = get_identifier (rent->local_name);
	  else
	    local_name = NULL_TREE;
	  gfc_set_backend_locus (&rent->where);
	  (*debug_hooks->imported_module_or_decl) (decl, local_name,
						   ns->proc_name->backend_decl,
						   !use_stmt->only_flag);
	}
    }
}


/* Return true if expr is a constant initializer that gfc_conv_initializer
   will handle.  */

static bool
check_constant_initializer (gfc_expr *expr, gfc_typespec *ts, bool array,
			    bool pointer)
{
  gfc_constructor *c;
  gfc_component *cm;

  if (pointer)
    return true;
  else if (array)
    {
      if (expr->expr_type == EXPR_CONSTANT || expr->expr_type == EXPR_NULL)
	return true;
      else if (expr->expr_type == EXPR_STRUCTURE)
	return check_constant_initializer (expr, ts, false, false);
      else if (expr->expr_type != EXPR_ARRAY)
	return false;
      for (c = gfc_constructor_first (expr->value.constructor);
	   c; c = gfc_constructor_next (c))
	{
	  if (c->iterator)
	    return false;
	  if (c->expr->expr_type == EXPR_STRUCTURE)
	    {
	      if (!check_constant_initializer (c->expr, ts, false, false))
		return false;
	    }
	  else if (c->expr->expr_type != EXPR_CONSTANT)
	    return false;
	}
      return true;
    }
  else switch (ts->type)
    {
    case_bt_struct:
      if (expr->expr_type != EXPR_STRUCTURE)
	return false;
      cm = expr->ts.u.derived->components;
      for (c = gfc_constructor_first (expr->value.constructor);
	   c; c = gfc_constructor_next (c), cm = cm->next)
	{
	  if (!c->expr || cm->attr.allocatable)
	    continue;
	  if (!check_constant_initializer (c->expr, &cm->ts,
					   cm->attr.dimension,
					   cm->attr.pointer))
	    return false;
	}
      return true;
    default:
      return expr->expr_type == EXPR_CONSTANT;
    }
}

/* Emit debug info for parameters and unreferenced variables with
   initializers.  */

static void
gfc_emit_parameter_debug_info (gfc_symbol *sym)
{
  tree decl;

  if (sym->attr.flavor != FL_PARAMETER
      && (sym->attr.flavor != FL_VARIABLE || sym->attr.referenced))
    return;

  if (sym->backend_decl != NULL
      || sym->value == NULL
      || sym->attr.use_assoc
      || sym->attr.dummy
      || sym->attr.result
      || sym->attr.function
      || sym->attr.intrinsic
      || sym->attr.pointer
      || sym->attr.allocatable
      || sym->attr.cray_pointee
      || sym->attr.threadprivate
      || sym->attr.is_bind_c
      || sym->attr.subref_array_pointer
      || sym->attr.assign)
    return;

  if (sym->ts.type == BT_CHARACTER)
    {
      gfc_conv_const_charlen (sym->ts.u.cl);
      if (sym->ts.u.cl->backend_decl == NULL
	  || TREE_CODE (sym->ts.u.cl->backend_decl) != INTEGER_CST)
	return;
    }
  else if (sym->ts.type == BT_DERIVED && sym->ts.u.derived->attr.alloc_comp)
    return;

  if (sym->as)
    {
      int n;

      if (sym->as->type != AS_EXPLICIT)
	return;
      for (n = 0; n < sym->as->rank; n++)
	if (sym->as->lower[n]->expr_type != EXPR_CONSTANT
	    || sym->as->upper[n] == NULL
	    || sym->as->upper[n]->expr_type != EXPR_CONSTANT)
	  return;
    }

  if (!check_constant_initializer (sym->value, &sym->ts,
				   sym->attr.dimension, false))
    return;

  if (flag_coarray == GFC_FCOARRAY_LIB && sym->attr.codimension)
    return;

  /* Create the decl for the variable or constant.  */
  decl = build_decl (input_location,
		     sym->attr.flavor == FL_PARAMETER ? CONST_DECL : VAR_DECL,
		     gfc_sym_identifier (sym), gfc_sym_type (sym));
  if (sym->attr.flavor == FL_PARAMETER)
    TREE_READONLY (decl) = 1;
  gfc_set_decl_location (decl, &sym->declared_at);
  if (sym->attr.dimension)
    GFC_DECL_PACKED_ARRAY (decl) = 1;
  DECL_CONTEXT (decl) = sym->ns->proc_name->backend_decl;
  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  if (DECL_CONTEXT (decl) && TREE_CODE (DECL_CONTEXT (decl)) == NAMESPACE_DECL)
    TREE_PUBLIC (decl) = 1;
  DECL_INITIAL (decl) = gfc_conv_initializer (sym->value, &sym->ts,
					      TREE_TYPE (decl),
					      sym->attr.dimension,
					      false, false);
  debug_hooks->early_global_decl (decl);
}


static void
generate_coarray_sym_init (gfc_symbol *sym)
{
  tree tmp, size, decl, token, desc;
  bool is_lock_type, is_event_type;
  int reg_type;
  gfc_se se;
  symbol_attribute attr;

  if (sym->attr.dummy || sym->attr.allocatable || !sym->attr.codimension
      || sym->attr.use_assoc || !sym->attr.referenced
      || sym->attr.select_type_temporary)
    return;

  decl = sym->backend_decl;
  TREE_USED(decl) = 1;
  gcc_assert (GFC_ARRAY_TYPE_P (TREE_TYPE (decl)));

  is_lock_type = sym->ts.type == BT_DERIVED
		 && sym->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
		 && sym->ts.u.derived->intmod_sym_id == ISOFORTRAN_LOCK_TYPE;

  is_event_type = sym->ts.type == BT_DERIVED
		  && sym->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
		  && sym->ts.u.derived->intmod_sym_id == ISOFORTRAN_EVENT_TYPE;

  /* FIXME: Workaround for PR middle-end/49106, cf. also PR middle-end/49108
     to make sure the variable is not optimized away.  */
  DECL_PRESERVE_P (DECL_CONTEXT (decl)) = 1;

  /* For lock types, we pass the array size as only the library knows the
     size of the variable.  */
  if (is_lock_type || is_event_type)
    size = gfc_index_one_node;
  else
    size = TYPE_SIZE_UNIT (gfc_get_element_type (TREE_TYPE (decl)));

  /* Ensure that we do not have size=0 for zero-sized arrays.  */
  size = fold_build2_loc (input_location, MAX_EXPR, size_type_node,
			  fold_convert (size_type_node, size),
			  build_int_cst (size_type_node, 1));

  if (GFC_TYPE_ARRAY_RANK (TREE_TYPE (decl)))
    {
      tmp = GFC_TYPE_ARRAY_SIZE (TREE_TYPE (decl));
      size = fold_build2_loc (input_location, MULT_EXPR, size_type_node,
			      fold_convert (size_type_node, tmp), size);
    }

  gcc_assert (GFC_TYPE_ARRAY_CAF_TOKEN (TREE_TYPE (decl)) != NULL_TREE);
  token = gfc_build_addr_expr (ppvoid_type_node,
			       GFC_TYPE_ARRAY_CAF_TOKEN (TREE_TYPE(decl)));
  if (is_lock_type)
    reg_type = sym->attr.artificial ? GFC_CAF_CRITICAL : GFC_CAF_LOCK_STATIC;
  else if (is_event_type)
    reg_type = GFC_CAF_EVENT_STATIC;
  else
    reg_type = GFC_CAF_COARRAY_STATIC;

  gfc_init_se (&se, NULL);
  desc = gfc_conv_scalar_to_descriptor (&se, decl, attr);
  gfc_add_block_to_block (&caf_init_block, &se.pre);

  tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_register, 7, size,
			     build_int_cst (integer_type_node, reg_type),
			     token, gfc_build_addr_expr (pvoid_type_node, desc),
			     null_pointer_node, /* stat.  */
			     null_pointer_node, /* errgmsg, errmsg_len.  */
			     build_int_cst (integer_type_node, 0));
  gfc_add_expr_to_block (&caf_init_block, tmp);
  gfc_add_modify (&caf_init_block, decl, fold_convert (TREE_TYPE (decl),
					  gfc_conv_descriptor_data_get (desc)));

  /* Handle "static" initializer.  */
  if (sym->value)
    {
      sym->attr.pointer = 1;
      tmp = gfc_trans_assignment (gfc_lval_expr_from_sym (sym), sym->value,
				  true, false);
      sym->attr.pointer = 0;
      gfc_add_expr_to_block (&caf_init_block, tmp);
    }
}


/* Generate constructor function to initialize static, nonallocatable
   coarrays.  */

static void
generate_coarray_init (gfc_namespace * ns __attribute((unused)))
{
  tree fndecl, tmp, decl, save_fn_decl;

  save_fn_decl = current_function_decl;
  push_function_context ();

  tmp = build_function_type_list (void_type_node, NULL_TREE);
  fndecl = build_decl (input_location, FUNCTION_DECL,
		       create_tmp_var_name ("_caf_init"), tmp);

  DECL_STATIC_CONSTRUCTOR (fndecl) = 1;
  SET_DECL_INIT_PRIORITY (fndecl, DEFAULT_INIT_PRIORITY);

  decl = build_decl (input_location, RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_CONTEXT (decl) = fndecl;
  DECL_RESULT (fndecl) = decl;

  pushdecl (fndecl);
  current_function_decl = fndecl;
  announce_function (fndecl);

  rest_of_decl_compilation (fndecl, 0, 0);
  make_decl_rtl (fndecl);
  allocate_struct_function (fndecl, false);

  pushlevel ();
  gfc_init_block (&caf_init_block);

  gfc_traverse_ns (ns, generate_coarray_sym_init);

  DECL_SAVED_TREE (fndecl) = gfc_finish_block (&caf_init_block);
  decl = getdecls ();

  poplevel (1, 1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  DECL_SAVED_TREE (fndecl)
    = build3_v (BIND_EXPR, decl, DECL_SAVED_TREE (fndecl),
                DECL_INITIAL (fndecl));
  dump_function (TDI_original, fndecl);

  cfun->function_end_locus = input_location;
  set_cfun (NULL);

  if (decl_function_context (fndecl))
    (void) cgraph_node::create (fndecl);
  else
    cgraph_node::finalize_function (fndecl, true);

  pop_function_context ();
  current_function_decl = save_fn_decl;
}


static void
create_module_nml_decl (gfc_symbol *sym)
{
  if (sym->attr.flavor == FL_NAMELIST)
    {
      tree decl = generate_namelist_decl (sym);
      pushdecl (decl);
      gcc_assert (sym->ns->proc_name->attr.flavor == FL_MODULE);
      DECL_CONTEXT (decl) = sym->ns->proc_name->backend_decl;
      rest_of_decl_compilation (decl, 1, 0);
      gfc_module_add_decl (cur_module, decl);
    }
}


/* Generate all the required code for module variables.  */

void
gfc_generate_module_vars (gfc_namespace * ns)
{
  module_namespace = ns;
  cur_module = gfc_find_module (ns->proc_name->name);

  /* Check if the frontend left the namespace in a reasonable state.  */
  gcc_assert (ns->proc_name && !ns->proc_name->tlink);

  /* Generate COMMON blocks.  */
  gfc_trans_common (ns);

  has_coarray_vars = false;

  /* Create decls for all the module variables.  */
  gfc_traverse_ns (ns, gfc_create_module_variable);
  gfc_traverse_ns (ns, create_module_nml_decl);

  if (flag_coarray == GFC_FCOARRAY_LIB && has_coarray_vars)
    generate_coarray_init (ns);

  cur_module = NULL;

  gfc_trans_use_stmts (ns);
  gfc_traverse_ns (ns, gfc_emit_parameter_debug_info);
}


static void
gfc_generate_contained_functions (gfc_namespace * parent)
{
  gfc_namespace *ns;

  /* We create all the prototypes before generating any code.  */
  for (ns = parent->contained; ns; ns = ns->sibling)
    {
      /* Skip namespaces from used modules.  */
      if (ns->parent != parent)
	continue;

      gfc_create_function_decl (ns, false);
    }

  for (ns = parent->contained; ns; ns = ns->sibling)
    {
      /* Skip namespaces from used modules.  */
      if (ns->parent != parent)
	continue;

      gfc_generate_function_code (ns);
    }
}


/* Drill down through expressions for the array specification bounds and
   character length calling generate_local_decl for all those variables
   that have not already been declared.  */

static void
generate_local_decl (gfc_symbol *);

/* Traverse expr, marking all EXPR_VARIABLE symbols referenced.  */

static bool
expr_decls (gfc_expr *e, gfc_symbol *sym,
	    int *f ATTRIBUTE_UNUSED)
{
  if (e->expr_type != EXPR_VARIABLE
	    || sym == e->symtree->n.sym
	    || e->symtree->n.sym->mark
	    || e->symtree->n.sym->ns != sym->ns)
	return false;

  generate_local_decl (e->symtree->n.sym);
  return false;
}

static void
generate_expr_decls (gfc_symbol *sym, gfc_expr *e)
{
  gfc_traverse_expr (e, sym, expr_decls, 0);
}


/* Check for dependencies in the character length and array spec.  */

static void
generate_dependency_declarations (gfc_symbol *sym)
{
  int i;

  if (sym->ts.type == BT_CHARACTER
      && sym->ts.u.cl
      && sym->ts.u.cl->length
      && sym->ts.u.cl->length->expr_type != EXPR_CONSTANT)
    generate_expr_decls (sym, sym->ts.u.cl->length);

  if (sym->as && sym->as->rank)
    {
      for (i = 0; i < sym->as->rank; i++)
	{
          generate_expr_decls (sym, sym->as->lower[i]);
          generate_expr_decls (sym, sym->as->upper[i]);
	}
    }
}


/* Generate decls for all local variables.  We do this to ensure correct
   handling of expressions which only appear in the specification of
   other functions.  */

static void
generate_local_decl (gfc_symbol * sym)
{
  if (sym->attr.flavor == FL_VARIABLE)
    {
      if (sym->attr.codimension && !sym->attr.dummy && !sym->attr.allocatable
	  && sym->attr.referenced && !sym->attr.use_assoc)
	has_coarray_vars = true;

      if (!sym->attr.dummy && !sym->ns->proc_name->attr.entry_master)
	generate_dependency_declarations (sym);

      if (sym->attr.referenced)
	gfc_get_symbol_decl (sym);

      /* Warnings for unused dummy arguments.  */
      else if (sym->attr.dummy && !sym->attr.in_namelist)
	{
	  /* INTENT(out) dummy arguments are likely meant to be set.  */
	  if (warn_unused_dummy_argument && sym->attr.intent == INTENT_OUT)
	    {
	      if (sym->ts.type != BT_DERIVED)
		gfc_warning (OPT_Wunused_dummy_argument,
			     "Dummy argument %qs at %L was declared "
			     "INTENT(OUT) but was not set",  sym->name,
			     &sym->declared_at);
	      else if (!gfc_has_default_initializer (sym->ts.u.derived)
		       && !sym->ts.u.derived->attr.zero_comp)
		gfc_warning (OPT_Wunused_dummy_argument,
			     "Derived-type dummy argument %qs at %L was "
			     "declared INTENT(OUT) but was not set and "
			     "does not have a default initializer",
			     sym->name, &sym->declared_at);
	      if (sym->backend_decl != NULL_TREE)
		TREE_NO_WARNING(sym->backend_decl) = 1;
	    }
	  else if (warn_unused_dummy_argument)
	    {
	      gfc_warning (OPT_Wunused_dummy_argument,
			   "Unused dummy argument %qs at %L", sym->name,
			   &sym->declared_at);
	      if (sym->backend_decl != NULL_TREE)
		TREE_NO_WARNING(sym->backend_decl) = 1;
	    }
	}

      /* Warn for unused variables, but not if they're inside a common
	 block or a namelist.  */
      else if (warn_unused_variable
	       && !(sym->attr.in_common || sym->mark || sym->attr.in_namelist))
	{
	  if (sym->attr.use_only)
	    {
	      gfc_warning (OPT_Wunused_variable,
			   "Unused module variable %qs which has been "
			   "explicitly imported at %L", sym->name,
			   &sym->declared_at);
	      if (sym->backend_decl != NULL_TREE)
		TREE_NO_WARNING(sym->backend_decl) = 1;
	    }
	  else if (!sym->attr.use_assoc)
	    {
	      /* Corner case: the symbol may be an entry point.  At this point,
		 it may appear to be an unused variable.  Suppress warning.  */
	      bool enter = false;
	      gfc_entry_list *el;

	      for (el = sym->ns->entries; el; el=el->next)
		if (strcmp(sym->name, el->sym->name) == 0)
		  enter = true;

	      if (!enter)
		gfc_warning (OPT_Wunused_variable,
			     "Unused variable %qs declared at %L",
			     sym->name, &sym->declared_at);
	      if (sym->backend_decl != NULL_TREE)
		TREE_NO_WARNING(sym->backend_decl) = 1;
	    }
	}

      /* For variable length CHARACTER parameters, the PARM_DECL already
	 references the length variable, so force gfc_get_symbol_decl
	 even when not referenced.  If optimize > 0, it will be optimized
	 away anyway.  But do this only after emitting -Wunused-parameter
	 warning if requested.  */
      if (sym->attr.dummy && !sym->attr.referenced
	    && sym->ts.type == BT_CHARACTER
	    && sym->ts.u.cl->backend_decl != NULL
	    && TREE_CODE (sym->ts.u.cl->backend_decl) == VAR_DECL)
	{
	  sym->attr.referenced = 1;
	  gfc_get_symbol_decl (sym);
	}

      /* INTENT(out) dummy arguments and result variables with allocatable
	 components are reset by default and need to be set referenced to
	 generate the code for nullification and automatic lengths.  */
      if (!sym->attr.referenced
	    && sym->ts.type == BT_DERIVED
	    && sym->ts.u.derived->attr.alloc_comp
	    && !sym->attr.pointer
	    && ((sym->attr.dummy && sym->attr.intent == INTENT_OUT)
		  ||
		(sym->attr.result && sym != sym->result)))
	{
	  sym->attr.referenced = 1;
	  gfc_get_symbol_decl (sym);
	}

      /* Check for dependencies in the array specification and string
	length, adding the necessary declarations to the function.  We
	mark the symbol now, as well as in traverse_ns, to prevent
	getting stuck in a circular dependency.  */
      sym->mark = 1;
    }
  else if (sym->attr.flavor == FL_PARAMETER)
    {
      if (warn_unused_parameter
           && !sym->attr.referenced)
	{
           if (!sym->attr.use_assoc)
	     gfc_warning (OPT_Wunused_parameter,
			  "Unused parameter %qs declared at %L", sym->name,
			  &sym->declared_at);
	   else if (sym->attr.use_only)
	     gfc_warning (OPT_Wunused_parameter,
			  "Unused parameter %qs which has been explicitly "
			  "imported at %L", sym->name, &sym->declared_at);
	}

      if (sym->ns
	  && sym->ns->parent
	  && sym->ns->parent->code
	  && sym->ns->parent->code->op == EXEC_BLOCK)
	{
	  if (sym->attr.referenced)
	    gfc_get_symbol_decl (sym);
	  sym->mark = 1;
	}
    }
  else if (sym->attr.flavor == FL_PROCEDURE)
    {
      /* TODO: move to the appropriate place in resolve.c.  */
      if (warn_return_type
	  && sym->attr.function
	  && sym->result
	  && sym != sym->result
	  && !sym->result->attr.referenced
	  && !sym->attr.use_assoc
	  && sym->attr.if_source != IFSRC_IFBODY)
	{
	  gfc_warning (OPT_Wreturn_type,
		       "Return value %qs of function %qs declared at "
		       "%L not set", sym->result->name, sym->name,
		        &sym->result->declared_at);

	  /* Prevents "Unused variable" warning for RESULT variables.  */
	  sym->result->mark = 1;
	}
    }

  if (sym->attr.dummy == 1)
    {
      /* Modify the tree type for scalar character dummy arguments of bind(c)
	 procedures if they are passed by value.  The tree type for them will
	 be promoted to INTEGER_TYPE for the middle end, which appears to be
	 what C would do with characters passed by-value.  The value attribute
         implies the dummy is a scalar.  */
      if (sym->attr.value == 1 && sym->backend_decl != NULL
	  && sym->ts.type == BT_CHARACTER && sym->ts.is_c_interop
	  && sym->ns->proc_name != NULL && sym->ns->proc_name->attr.is_bind_c)
	gfc_conv_scalar_char_value (sym, NULL, NULL);

      /* Unused procedure passed as dummy argument.  */
      if (sym->attr.flavor == FL_PROCEDURE)
	{
	  if (!sym->attr.referenced)
	    {
	      if (warn_unused_dummy_argument)
		gfc_warning (OPT_Wunused_dummy_argument,
			     "Unused dummy argument %qs at %L", sym->name,
			     &sym->declared_at);
	    }

	  /* Silence bogus "unused parameter" warnings from the
	     middle end.  */
	  if (sym->backend_decl != NULL_TREE)
		TREE_NO_WARNING (sym->backend_decl) = 1;
	}
    }

  /* Make sure we convert the types of the derived types from iso_c_binding
     into (void *).  */
  if (sym->attr.flavor != FL_PROCEDURE && sym->attr.is_iso_c
      && sym->ts.type == BT_DERIVED)
    sym->backend_decl = gfc_typenode_for_spec (&(sym->ts));
}


static void
generate_local_nml_decl (gfc_symbol * sym)
{
  if (sym->attr.flavor == FL_NAMELIST && !sym->attr.use_assoc)
    {
      tree decl = generate_namelist_decl (sym);
      pushdecl (decl);
    }
}


static void
generate_local_vars (gfc_namespace * ns)
{
  gfc_traverse_ns (ns, generate_local_decl);
  gfc_traverse_ns (ns, generate_local_nml_decl);
}


/* Generate a switch statement to jump to the correct entry point.  Also
   creates the label decls for the entry points.  */

static tree
gfc_trans_entry_master_switch (gfc_entry_list * el)
{
  stmtblock_t block;
  tree label;
  tree tmp;
  tree val;

  gfc_init_block (&block);
  for (; el; el = el->next)
    {
      /* Add the case label.  */
      label = gfc_build_label_decl (NULL_TREE);
      val = build_int_cst (gfc_array_index_type, el->id);
      tmp = build_case_label (val, NULL_TREE, label);
      gfc_add_expr_to_block (&block, tmp);

      /* And jump to the actual entry point.  */
      label = gfc_build_label_decl (NULL_TREE);
      tmp = build1_v (GOTO_EXPR, label);
      gfc_add_expr_to_block (&block, tmp);

      /* Save the label decl.  */
      el->label = label;
    }
  tmp = gfc_finish_block (&block);
  /* The first argument selects the entry point.  */
  val = DECL_ARGUMENTS (current_function_decl);
  tmp = fold_build3_loc (input_location, SWITCH_EXPR, NULL_TREE,
			 val, tmp, NULL_TREE);
  return tmp;
}


/* Add code to string lengths of actual arguments passed to a function against
   the expected lengths of the dummy arguments.  */

static void
add_argument_checking (stmtblock_t *block, gfc_symbol *sym)
{
  gfc_formal_arglist *formal;

  for (formal = gfc_sym_get_dummy_args (sym); formal; formal = formal->next)
    if (formal->sym && formal->sym->ts.type == BT_CHARACTER
	&& !formal->sym->ts.deferred)
      {
	enum tree_code comparison;
	tree cond;
	tree argname;
	gfc_symbol *fsym;
	gfc_charlen *cl;
	const char *message;

	fsym = formal->sym;
	cl = fsym->ts.u.cl;

	gcc_assert (cl);
	gcc_assert (cl->passed_length != NULL_TREE);
	gcc_assert (cl->backend_decl != NULL_TREE);

	/* For POINTER, ALLOCATABLE and assumed-shape dummy arguments, the
	   string lengths must match exactly.  Otherwise, it is only required
	   that the actual string length is *at least* the expected one.
	   Sequence association allows for a mismatch of the string length
	   if the actual argument is (part of) an array, but only if the
	   dummy argument is an array. (See "Sequence association" in
	   Section 12.4.1.4 for F95 and 12.4.1.5 for F2003.)  */
	if (fsym->attr.pointer || fsym->attr.allocatable
	    || (fsym->as && (fsym->as->type == AS_ASSUMED_SHAPE
			     || fsym->as->type == AS_ASSUMED_RANK)))
	  {
	    comparison = NE_EXPR;
	    message = _("Actual string length does not match the declared one"
			" for dummy argument '%s' (%ld/%ld)");
	  }
	else if (fsym->as && fsym->as->rank != 0)
	  continue;
	else
	  {
	    comparison = LT_EXPR;
	    message = _("Actual string length is shorter than the declared one"
			" for dummy argument '%s' (%ld/%ld)");
	  }

	/* Build the condition.  For optional arguments, an actual length
	   of 0 is also acceptable if the associated string is NULL, which
	   means the argument was not passed.  */
	cond = fold_build2_loc (input_location, comparison, boolean_type_node,
				cl->passed_length, cl->backend_decl);
	if (fsym->attr.optional)
	  {
	    tree not_absent;
	    tree not_0length;
	    tree absent_failed;

	    not_0length = fold_build2_loc (input_location, NE_EXPR,
					   boolean_type_node,
					   cl->passed_length,
					   build_zero_cst (gfc_charlen_type_node));
	    /* The symbol needs to be referenced for gfc_get_symbol_decl.  */
	    fsym->attr.referenced = 1;
	    not_absent = gfc_conv_expr_present (fsym);

	    absent_failed = fold_build2_loc (input_location, TRUTH_OR_EXPR,
					     boolean_type_node, not_0length,
					     not_absent);

	    cond = fold_build2_loc (input_location, TRUTH_AND_EXPR,
				    boolean_type_node, cond, absent_failed);
	  }

	/* Build the runtime check.  */
	argname = gfc_build_cstring_const (fsym->name);
	argname = gfc_build_addr_expr (pchar_type_node, argname);
	gfc_trans_runtime_check (true, false, cond, block, &fsym->declared_at,
				 message, argname,
				 fold_convert (long_integer_type_node,
					       cl->passed_length),
				 fold_convert (long_integer_type_node,
					       cl->backend_decl));
      }
}


static void
create_main_function (tree fndecl)
{
  tree old_context;
  tree ftn_main;
  tree tmp, decl, result_decl, argc, argv, typelist, arglist;
  stmtblock_t body;

  old_context = current_function_decl;

  if (old_context)
    {
      push_function_context ();
      saved_parent_function_decls = saved_function_decls;
      saved_function_decls = NULL_TREE;
    }

  /* main() function must be declared with global scope.  */
  gcc_assert (current_function_decl == NULL_TREE);

  /* Declare the function.  */
  tmp =  build_function_type_list (integer_type_node, integer_type_node,
				   build_pointer_type (pchar_type_node),
				   NULL_TREE);
  main_identifier_node = get_identifier ("main");
  ftn_main = build_decl (input_location, FUNCTION_DECL,
      			 main_identifier_node, tmp);
  DECL_EXTERNAL (ftn_main) = 0;
  TREE_PUBLIC (ftn_main) = 1;
  TREE_STATIC (ftn_main) = 1;
  DECL_ATTRIBUTES (ftn_main)
      = tree_cons (get_identifier("externally_visible"), NULL_TREE, NULL_TREE);

  /* Setup the result declaration (for "return 0").  */
  result_decl = build_decl (input_location,
			    RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_ARTIFICIAL (result_decl) = 1;
  DECL_IGNORED_P (result_decl) = 1;
  DECL_CONTEXT (result_decl) = ftn_main;
  DECL_RESULT (ftn_main) = result_decl;

  pushdecl (ftn_main);

  /* Get the arguments.  */

  arglist = NULL_TREE;
  typelist = TYPE_ARG_TYPES (TREE_TYPE (ftn_main));

  tmp = TREE_VALUE (typelist);
  argc = build_decl (input_location, PARM_DECL, get_identifier ("argc"), tmp);
  DECL_CONTEXT (argc) = ftn_main;
  DECL_ARG_TYPE (argc) = TREE_VALUE (typelist);
  TREE_READONLY (argc) = 1;
  gfc_finish_decl (argc);
  arglist = chainon (arglist, argc);

  typelist = TREE_CHAIN (typelist);
  tmp = TREE_VALUE (typelist);
  argv = build_decl (input_location, PARM_DECL, get_identifier ("argv"), tmp);
  DECL_CONTEXT (argv) = ftn_main;
  DECL_ARG_TYPE (argv) = TREE_VALUE (typelist);
  TREE_READONLY (argv) = 1;
  DECL_BY_REFERENCE (argv) = 1;
  gfc_finish_decl (argv);
  arglist = chainon (arglist, argv);

  DECL_ARGUMENTS (ftn_main) = arglist;
  current_function_decl = ftn_main;
  announce_function (ftn_main);

  rest_of_decl_compilation (ftn_main, 1, 0);
  make_decl_rtl (ftn_main);
  allocate_struct_function (ftn_main, false);
  pushlevel ();

  gfc_init_block (&body);

  /* Call some libgfortran initialization routines, call then MAIN__().  */

  /* Call _gfortran_caf_init (*argc, ***argv).  */
  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tree pint_type, pppchar_type;
      pint_type = build_pointer_type (integer_type_node);
      pppchar_type
	= build_pointer_type (build_pointer_type (pchar_type_node));

      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_init, 2,
		gfc_build_addr_expr (pint_type, argc),
		gfc_build_addr_expr (pppchar_type, argv));
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Call _gfortran_set_args (argc, argv).  */
  TREE_USED (argc) = 1;
  TREE_USED (argv) = 1;
  tmp = build_call_expr_loc (input_location,
			 gfor_fndecl_set_args, 2, argc, argv);
  gfc_add_expr_to_block (&body, tmp);

  /* Add a call to set_options to set up the runtime library Fortran
     language standard parameters.  */
  {
    tree array_type, array, var;
    vec<constructor_elt, va_gc> *v = NULL;

    /* Passing a new option to the library requires four modifications:
     + add it to the tree_cons list below
          + change the array size in the call to build_array_type
          + change the first argument to the library call
            gfor_fndecl_set_options
          + modify the library (runtime/compile_options.c)!  */

    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                            build_int_cst (integer_type_node,
                                           gfc_option.warn_std));
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                            build_int_cst (integer_type_node,
                                           gfc_option.allow_std));
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                            build_int_cst (integer_type_node, pedantic));
    /* TODO: This is the old -fdump-core option, which is unused but
       passed due to ABI compatibility; remove when bumping the
       library ABI.  */
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                            build_int_cst (integer_type_node,
                                           0));
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                            build_int_cst (integer_type_node, flag_backtrace));
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                            build_int_cst (integer_type_node, flag_sign_zero));
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                            build_int_cst (integer_type_node,
                                           (gfc_option.rtcheck
                                            & GFC_RTCHECK_BOUNDS)));
    /* TODO: This is the -frange-check option, which no longer affects
       library behavior; when bumping the library ABI this slot can be
       reused for something else. As it is the last element in the
       array, we can instead leave it out altogether.  */
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                            build_int_cst (integer_type_node, 0));
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                            build_int_cst (integer_type_node,
                                           gfc_option.fpe_summary));

    array_type = build_array_type (integer_type_node,
				   build_index_type (size_int (8)));
    array = build_constructor (array_type, v);
    TREE_CONSTANT (array) = 1;
    TREE_STATIC (array) = 1;

    /* Create a static variable to hold the jump table.  */
    var = build_decl (input_location, VAR_DECL,
		      create_tmp_var_name ("options"),
		      array_type);
    DECL_ARTIFICIAL (var) = 1;
    DECL_IGNORED_P (var) = 1;
    TREE_CONSTANT (var) = 1;
    TREE_STATIC (var) = 1;
    TREE_READONLY (var) = 1;
    DECL_INITIAL (var) = array;
    pushdecl (var);
    var = gfc_build_addr_expr (build_pointer_type (integer_type_node), var);

    tmp = build_call_expr_loc (input_location,
			   gfor_fndecl_set_options, 2,
			   build_int_cst (integer_type_node, 9), var);
    gfc_add_expr_to_block (&body, tmp);
  }

  /* If -ffpe-trap option was provided, add a call to set_fpe so that
     the library will raise a FPE when needed.  */
  if (gfc_option.fpe != 0)
    {
      tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_set_fpe, 1,
			     build_int_cst (integer_type_node,
					    gfc_option.fpe));
      gfc_add_expr_to_block (&body, tmp);
    }

  /* If this is the main program and an -fconvert option was provided,
     add a call to set_convert.  */

  if (flag_convert != GFC_FLAG_CONVERT_NATIVE)
    {
      tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_set_convert, 1,
			     build_int_cst (integer_type_node, flag_convert));
      gfc_add_expr_to_block (&body, tmp);
    }

  /* If this is the main program and an -frecord-marker option was provided,
     add a call to set_record_marker.  */

  if (flag_record_marker != 0)
    {
      tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_set_record_marker, 1,
			     build_int_cst (integer_type_node,
					    flag_record_marker));
      gfc_add_expr_to_block (&body, tmp);
    }

  if (flag_max_subrecord_length != 0)
    {
      tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_set_max_subrecord_length, 1,
			     build_int_cst (integer_type_node,
					    flag_max_subrecord_length));
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Call MAIN__().  */
  tmp = build_call_expr_loc (input_location,
			 fndecl, 0);
  gfc_add_expr_to_block (&body, tmp);

  /* Mark MAIN__ as used.  */
  TREE_USED (fndecl) = 1;

  /* Coarray: Call _gfortran_caf_finalize(void).  */
  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_finalize, 0);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* "return 0".  */
  tmp = fold_build2_loc (input_location, MODIFY_EXPR, integer_type_node,
			 DECL_RESULT (ftn_main),
			 build_int_cst (integer_type_node, 0));
  tmp = build1_v (RETURN_EXPR, tmp);
  gfc_add_expr_to_block (&body, tmp);


  DECL_SAVED_TREE (ftn_main) = gfc_finish_block (&body);
  decl = getdecls ();

  /* Finish off this function and send it for code generation.  */
  poplevel (1, 1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (ftn_main)) = ftn_main;

  DECL_SAVED_TREE (ftn_main)
    = build3_v (BIND_EXPR, decl, DECL_SAVED_TREE (ftn_main),
		DECL_INITIAL (ftn_main));

  /* Output the GENERIC tree.  */
  dump_function (TDI_original, ftn_main);

  cgraph_node::finalize_function (ftn_main, true);

  if (old_context)
    {
      pop_function_context ();
      saved_function_decls = saved_parent_function_decls;
    }
  current_function_decl = old_context;
}


/* Get the result expression for a procedure.  */

static tree
get_proc_result (gfc_symbol* sym)
{
  if (sym->attr.subroutine || sym == sym->result)
    {
      if (current_fake_result_decl != NULL)
	return TREE_VALUE (current_fake_result_decl);

      return NULL_TREE;
    }

  return sym->result->backend_decl;
}


/* Generate an appropriate return-statement for a procedure.  */

tree
gfc_generate_return (void)
{
  gfc_symbol* sym;
  tree result;
  tree fndecl;

  sym = current_procedure_symbol;
  fndecl = sym->backend_decl;

  if (TREE_TYPE (DECL_RESULT (fndecl)) == void_type_node)
    result = NULL_TREE;
  else
    {
      result = get_proc_result (sym);

      /* Set the return value to the dummy result variable.  The
	 types may be different for scalar default REAL functions
	 with -ff2c, therefore we have to convert.  */
      if (result != NULL_TREE)
	{
	  result = convert (TREE_TYPE (DECL_RESULT (fndecl)), result);
	  result = fold_build2_loc (input_location, MODIFY_EXPR,
				    TREE_TYPE (result), DECL_RESULT (fndecl),
				    result);
	}
    }

  return build1_v (RETURN_EXPR, result);
}


static void
is_from_ieee_module (gfc_symbol *sym)
{
  if (sym->from_intmod == INTMOD_IEEE_FEATURES
      || sym->from_intmod == INTMOD_IEEE_EXCEPTIONS
      || sym->from_intmod == INTMOD_IEEE_ARITHMETIC)
    seen_ieee_symbol = 1;
}


static int
is_ieee_module_used (gfc_namespace *ns)
{
  seen_ieee_symbol = 0;
  gfc_traverse_ns (ns, is_from_ieee_module);
  return seen_ieee_symbol;
}


static gfc_omp_clauses *module_oacc_clauses;


static void
add_clause (gfc_symbol *sym, gfc_omp_map_op map_op)
{
  gfc_omp_namelist *n;

  n = gfc_get_omp_namelist ();
  n->sym = sym;
  n->u.map_op = map_op;

  if (!module_oacc_clauses)
    module_oacc_clauses = gfc_get_omp_clauses ();

  if (module_oacc_clauses->lists[OMP_LIST_MAP])
    n->next = module_oacc_clauses->lists[OMP_LIST_MAP];

  module_oacc_clauses->lists[OMP_LIST_MAP] = n;
}


static void
find_module_oacc_declare_clauses (gfc_symbol *sym)
{
  if (sym->attr.use_assoc)
    {
      gfc_omp_map_op map_op;

      if (sym->attr.oacc_declare_create)
	map_op = OMP_MAP_FORCE_ALLOC;

      if (sym->attr.oacc_declare_copyin)
	map_op = OMP_MAP_FORCE_TO;

      if (sym->attr.oacc_declare_deviceptr)
	map_op = OMP_MAP_FORCE_DEVICEPTR;

      if (sym->attr.oacc_declare_device_resident)
	map_op = OMP_MAP_DEVICE_RESIDENT;

      if (sym->attr.oacc_declare_create
	  || sym->attr.oacc_declare_copyin
	  || sym->attr.oacc_declare_deviceptr
	  || sym->attr.oacc_declare_device_resident)
	{
	  sym->attr.referenced = 1;
	  add_clause (sym, map_op);
	}
    }
}


void
finish_oacc_declare (gfc_namespace *ns, gfc_symbol *sym, bool block)
{
  gfc_code *code;
  gfc_oacc_declare *oc;
  locus where = gfc_current_locus;
  gfc_omp_clauses *omp_clauses = NULL;
  gfc_omp_namelist *n, *p;

  gfc_traverse_ns (ns, find_module_oacc_declare_clauses);

  if (module_oacc_clauses && sym->attr.flavor == FL_PROGRAM)
    {
      gfc_oacc_declare *new_oc;

      new_oc = gfc_get_oacc_declare ();
      new_oc->next = ns->oacc_declare;
      new_oc->clauses = module_oacc_clauses;

      ns->oacc_declare = new_oc;
      module_oacc_clauses = NULL;
    }

  if (!ns->oacc_declare)
    return;

  for (oc = ns->oacc_declare; oc; oc = oc->next)
    {
      if (oc->module_var)
	continue;

      if (block)
	gfc_error ("Sorry, $!ACC DECLARE at %L is not allowed "
		   "in BLOCK construct", &oc->loc);


      if (oc->clauses && oc->clauses->lists[OMP_LIST_MAP])
	{
	  if (omp_clauses == NULL)
	    {
	      omp_clauses = oc->clauses;
	      continue;
	    }

	  for (n = oc->clauses->lists[OMP_LIST_MAP]; n; p = n, n = n->next)
	    ;

	  gcc_assert (p->next == NULL);

	  p->next = omp_clauses->lists[OMP_LIST_MAP];
	  omp_clauses = oc->clauses;
	}
    }

  if (!omp_clauses)
    return;

  for (n = omp_clauses->lists[OMP_LIST_MAP]; n; n = n->next)
    {
      switch (n->u.map_op)
	{
	  case OMP_MAP_DEVICE_RESIDENT:
	    n->u.map_op = OMP_MAP_FORCE_ALLOC;
	    break;

	  default:
	    break;
	}
    }

  code = XCNEW (gfc_code);
  code->op = EXEC_OACC_DECLARE;
  code->loc = where;

  code->ext.oacc_declare = gfc_get_oacc_declare ();
  code->ext.oacc_declare->clauses = omp_clauses;

  code->block = XCNEW (gfc_code);
  code->block->op = EXEC_OACC_DECLARE;
  code->block->loc = where;

  if (ns->code)
    code->block->next = ns->code;

  ns->code = code;

  return;
}


/* Generate code for a function.  */

void
gfc_generate_function_code (gfc_namespace * ns)
{
  tree fndecl;
  tree old_context;
  tree decl;
  tree tmp;
  tree fpstate = NULL_TREE;
  stmtblock_t init, cleanup;
  stmtblock_t body;
  gfc_wrapped_block try_block;
  tree recurcheckvar = NULL_TREE;
  gfc_symbol *sym;
  gfc_symbol *previous_procedure_symbol;
  int rank, ieee;
  bool is_recursive;

  sym = ns->proc_name;
  previous_procedure_symbol = current_procedure_symbol;
  current_procedure_symbol = sym;

  /* Check that the frontend isn't still using this.  */
  gcc_assert (sym->tlink == NULL);
  sym->tlink = sym;

  /* Create the declaration for functions with global scope.  */
  if (!sym->backend_decl)
    gfc_create_function_decl (ns, false);

  fndecl = sym->backend_decl;
  old_context = current_function_decl;

  if (old_context)
    {
      push_function_context ();
      saved_parent_function_decls = saved_function_decls;
      saved_function_decls = NULL_TREE;
    }

  trans_function_start (sym);

  gfc_init_block (&init);

  if (ns->entries && ns->proc_name->ts.type == BT_CHARACTER)
    {
      /* Copy length backend_decls to all entry point result
	 symbols.  */
      gfc_entry_list *el;
      tree backend_decl;

      gfc_conv_const_charlen (ns->proc_name->ts.u.cl);
      backend_decl = ns->proc_name->result->ts.u.cl->backend_decl;
      for (el = ns->entries; el; el = el->next)
	el->sym->result->ts.u.cl->backend_decl = backend_decl;
    }

  /* Translate COMMON blocks.  */
  gfc_trans_common (ns);

  /* Null the parent fake result declaration if this namespace is
     a module function or an external procedures.  */
  if ((ns->parent && ns->parent->proc_name->attr.flavor == FL_MODULE)
	|| ns->parent == NULL)
    parent_fake_result_decl = NULL_TREE;

  gfc_generate_contained_functions (ns);

  nonlocal_dummy_decls = NULL;
  nonlocal_dummy_decl_pset = NULL;

  has_coarray_vars = false;
  generate_local_vars (ns);

  if (flag_coarray == GFC_FCOARRAY_LIB && has_coarray_vars)
    generate_coarray_init (ns);

  /* Keep the parent fake result declaration in module functions
     or external procedures.  */
  if ((ns->parent && ns->parent->proc_name->attr.flavor == FL_MODULE)
	|| ns->parent == NULL)
    current_fake_result_decl = parent_fake_result_decl;
  else
    current_fake_result_decl = NULL_TREE;

  is_recursive = sym->attr.recursive
		 || (sym->attr.entry_master
		     && sym->ns->entries->sym->attr.recursive);
  if ((gfc_option.rtcheck & GFC_RTCHECK_RECURSION)
      && !is_recursive && !flag_recursive)
    {
      char * msg;

      msg = xasprintf ("Recursive call to nonrecursive procedure '%s'",
		       sym->name);
      recurcheckvar = gfc_create_var (boolean_type_node, "is_recursive");
      TREE_STATIC (recurcheckvar) = 1;
      DECL_INITIAL (recurcheckvar) = boolean_false_node;
      gfc_add_expr_to_block (&init, recurcheckvar);
      gfc_trans_runtime_check (true, false, recurcheckvar, &init,
			       &sym->declared_at, msg);
      gfc_add_modify (&init, recurcheckvar, boolean_true_node);
      free (msg);
    }

  /* Check if an IEEE module is used in the procedure.  If so, save
     the floating point state.  */
  ieee = is_ieee_module_used (ns);
  if (ieee)
    fpstate = gfc_save_fp_state (&init);

  /* Now generate the code for the body of this function.  */
  gfc_init_block (&body);

  if (TREE_TYPE (DECL_RESULT (fndecl)) != void_type_node
	&& sym->attr.subroutine)
    {
      tree alternate_return;
      alternate_return = gfc_get_fake_result_decl (sym, 0);
      gfc_add_modify (&body, alternate_return, integer_zero_node);
    }

  if (ns->entries)
    {
      /* Jump to the correct entry point.  */
      tmp = gfc_trans_entry_master_switch (ns->entries);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* If bounds-checking is enabled, generate code to check passed in actual
     arguments against the expected dummy argument attributes (e.g. string
     lengths).  */
  if ((gfc_option.rtcheck & GFC_RTCHECK_BOUNDS) && !sym->attr.is_bind_c)
    add_argument_checking (&body, sym);

  finish_oacc_declare (ns, sym, false);

  tmp = gfc_trans_code (ns->code);
  gfc_add_expr_to_block (&body, tmp);

  if (TREE_TYPE (DECL_RESULT (fndecl)) != void_type_node
      || (sym->result && sym->result != sym
	  && sym->result->ts.type == BT_DERIVED
	  && sym->result->ts.u.derived->attr.alloc_comp))
    {
      bool artificial_result_decl = false;
      tree result = get_proc_result (sym);
      gfc_symbol *rsym = sym == sym->result ? sym : sym->result;

      /* Make sure that a function returning an object with
	 alloc/pointer_components always has a result, where at least
	 the allocatable/pointer components are set to zero.  */
      if (result == NULL_TREE && sym->attr.function
	  && ((sym->result->ts.type == BT_DERIVED
	       && (sym->attr.allocatable
		   || sym->attr.pointer
		   || sym->result->ts.u.derived->attr.alloc_comp
		   || sym->result->ts.u.derived->attr.pointer_comp))
	      || (sym->result->ts.type == BT_CLASS
		  && (CLASS_DATA (sym)->attr.allocatable
		      || CLASS_DATA (sym)->attr.class_pointer
		      || CLASS_DATA (sym->result)->attr.alloc_comp
		      || CLASS_DATA (sym->result)->attr.pointer_comp))))
	{
	  artificial_result_decl = true;
	  result = gfc_get_fake_result_decl (sym, 0);
	}

      if (result != NULL_TREE && sym->attr.function && !sym->attr.pointer)
	{
	  if (sym->attr.allocatable && sym->attr.dimension == 0
	      && sym->result == sym)
	    gfc_add_modify (&init, result, fold_convert (TREE_TYPE (result),
							 null_pointer_node));
	  else if (sym->ts.type == BT_CLASS
		   && CLASS_DATA (sym)->attr.allocatable
		   && CLASS_DATA (sym)->attr.dimension == 0
		   && sym->result == sym)
	    {
	      tmp = CLASS_DATA (sym)->backend_decl;
	      tmp = fold_build3_loc (input_location, COMPONENT_REF,
				     TREE_TYPE (tmp), result, tmp, NULL_TREE);
	      gfc_add_modify (&init, tmp, fold_convert (TREE_TYPE (tmp),
							null_pointer_node));
	    }
	  else if (sym->ts.type == BT_DERIVED
		   && !sym->attr.allocatable)
	    {
	      gfc_expr *init_exp;
	      /* Arrays are not initialized using the default initializer of
		 their elements.  Therefore only check if a default
		 initializer is available when the result is scalar.  */
              init_exp = rsym->as ? NULL
                                  : gfc_generate_initializer (&rsym->ts, true);
	      if (init_exp)
		{
		  tmp = gfc_trans_structure_assign (result, init_exp, 0);
		  gfc_free_expr (init_exp);
		  gfc_add_expr_to_block (&init, tmp);
		}
	      else if (rsym->ts.u.derived->attr.alloc_comp)
		{
		  rank = rsym->as ? rsym->as->rank : 0;
		  tmp = gfc_nullify_alloc_comp (rsym->ts.u.derived, result,
						rank);
		  gfc_prepend_expr_to_block (&body, tmp);
		}
	    }
	}

      if (result == NULL_TREE || artificial_result_decl)
	{
	  /* TODO: move to the appropriate place in resolve.c.  */
	  if (warn_return_type && sym == sym->result)
	    gfc_warning (OPT_Wreturn_type,
			 "Return value of function %qs at %L not set",
			 sym->name, &sym->declared_at);
	  if (warn_return_type)
	    TREE_NO_WARNING(sym->backend_decl) = 1;
	}
      if (result != NULL_TREE)
	gfc_add_expr_to_block (&body, gfc_generate_return ());
    }

  gfc_init_block (&cleanup);

  /* Reset recursion-check variable.  */
  if ((gfc_option.rtcheck & GFC_RTCHECK_RECURSION)
      && !is_recursive && !flag_openmp && recurcheckvar != NULL_TREE)
    {
      gfc_add_modify (&cleanup, recurcheckvar, boolean_false_node);
      recurcheckvar = NULL;
    }

  /* If IEEE modules are loaded, restore the floating-point state.  */
  if (ieee)
    gfc_restore_fp_state (&cleanup, fpstate);

  /* Finish the function body and add init and cleanup code.  */
  tmp = gfc_finish_block (&body);
  gfc_start_wrapped_block (&try_block, tmp);
  /* Add code to create and cleanup arrays.  */
  gfc_trans_deferred_vars (sym, &try_block);
  gfc_add_init_cleanup (&try_block, gfc_finish_block (&init),
			gfc_finish_block (&cleanup));

  /* Add all the decls we created during processing.  */
  decl = nreverse (saved_function_decls);
  while (decl)
    {
      tree next;

      next = DECL_CHAIN (decl);
      DECL_CHAIN (decl) = NULL_TREE;
      pushdecl (decl);
      decl = next;
    }
  saved_function_decls = NULL_TREE;

  DECL_SAVED_TREE (fndecl) = gfc_finish_wrapped_block (&try_block);
  decl = getdecls ();

  /* Finish off this function and send it for code generation.  */
  poplevel (1, 1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  DECL_SAVED_TREE (fndecl)
    = build3_v (BIND_EXPR, decl, DECL_SAVED_TREE (fndecl),
		DECL_INITIAL (fndecl));

  if (nonlocal_dummy_decls)
    {
      BLOCK_VARS (DECL_INITIAL (fndecl))
	= chainon (BLOCK_VARS (DECL_INITIAL (fndecl)), nonlocal_dummy_decls);
      delete nonlocal_dummy_decl_pset;
      nonlocal_dummy_decls = NULL;
      nonlocal_dummy_decl_pset = NULL;
    }

  /* Output the GENERIC tree.  */
  dump_function (TDI_original, fndecl);

  /* Store the end of the function, so that we get good line number
     info for the epilogue.  */
  cfun->function_end_locus = input_location;

  /* We're leaving the context of this function, so zap cfun.
     It's still in DECL_STRUCT_FUNCTION, and we'll restore it in
     tree_rest_of_compilation.  */
  set_cfun (NULL);

  if (old_context)
    {
      pop_function_context ();
      saved_function_decls = saved_parent_function_decls;
    }
  current_function_decl = old_context;

  if (decl_function_context (fndecl))
    {
      /* Register this function with cgraph just far enough to get it
	 added to our parent's nested function list.
	 If there are static coarrays in this function, the nested _caf_init
	 function has already called cgraph_create_node, which also created
	 the cgraph node for this function.  */
      if (!has_coarray_vars || flag_coarray != GFC_FCOARRAY_LIB)
	(void) cgraph_node::get_create (fndecl);
    }
  else
    cgraph_node::finalize_function (fndecl, true);

  gfc_trans_use_stmts (ns);
  gfc_traverse_ns (ns, gfc_emit_parameter_debug_info);

  if (sym->attr.is_main_program)
    create_main_function (fndecl);

  current_procedure_symbol = previous_procedure_symbol;
}


void
gfc_generate_constructors (void)
{
  gcc_assert (gfc_static_ctors == NULL_TREE);
#if 0
  tree fnname;
  tree type;
  tree fndecl;
  tree decl;
  tree tmp;

  if (gfc_static_ctors == NULL_TREE)
    return;

  fnname = get_file_function_name ("I");
  type = build_function_type_list (void_type_node, NULL_TREE);

  fndecl = build_decl (input_location,
		       FUNCTION_DECL, fnname, type);
  TREE_PUBLIC (fndecl) = 1;

  decl = build_decl (input_location,
		     RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_CONTEXT (decl) = fndecl;
  DECL_RESULT (fndecl) = decl;

  pushdecl (fndecl);

  current_function_decl = fndecl;

  rest_of_decl_compilation (fndecl, 1, 0);

  make_decl_rtl (fndecl);

  allocate_struct_function (fndecl, false);

  pushlevel ();

  for (; gfc_static_ctors; gfc_static_ctors = TREE_CHAIN (gfc_static_ctors))
    {
      tmp = build_call_expr_loc (input_location,
			     TREE_VALUE (gfc_static_ctors), 0);
      DECL_SAVED_TREE (fndecl) = build_stmt (input_location, EXPR_STMT, tmp);
    }

  decl = getdecls ();
  poplevel (1, 1);

  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;
  DECL_SAVED_TREE (fndecl)
    = build3_v (BIND_EXPR, decl, DECL_SAVED_TREE (fndecl),
		DECL_INITIAL (fndecl));

  free_after_parsing (cfun);
  free_after_compilation (cfun);

  tree_rest_of_compilation (fndecl);

  current_function_decl = NULL_TREE;
#endif
}

/* Translates a BLOCK DATA program unit. This means emitting the
   commons contained therein plus their initializations. We also emit
   a globally visible symbol to make sure that each BLOCK DATA program
   unit remains unique.  */

void
gfc_generate_block_data (gfc_namespace * ns)
{
  tree decl;
  tree id;

  /* Tell the backend the source location of the block data.  */
  if (ns->proc_name)
    gfc_set_backend_locus (&ns->proc_name->declared_at);
  else
    gfc_set_backend_locus (&gfc_current_locus);

  /* Process the DATA statements.  */
  gfc_trans_common (ns);

  /* Create a global symbol with the mane of the block data.  This is to
     generate linker errors if the same name is used twice.  It is never
     really used.  */
  if (ns->proc_name)
    id = gfc_sym_mangled_function_id (ns->proc_name);
  else
    id = get_identifier ("__BLOCK_DATA__");

  decl = build_decl (input_location,
		     VAR_DECL, id, gfc_array_index_type);
  TREE_PUBLIC (decl) = 1;
  TREE_STATIC (decl) = 1;
  DECL_IGNORED_P (decl) = 1;

  pushdecl (decl);
  rest_of_decl_compilation (decl, 1, 0);
}


/* Process the local variables of a BLOCK construct.  */

void
gfc_process_block_locals (gfc_namespace* ns)
{
  tree decl;

  gcc_assert (saved_local_decls == NULL_TREE);
  has_coarray_vars = false;

  generate_local_vars (ns);

  if (flag_coarray == GFC_FCOARRAY_LIB && has_coarray_vars)
    generate_coarray_init (ns);

  decl = nreverse (saved_local_decls);
  while (decl)
    {
      tree next;

      next = DECL_CHAIN (decl);
      DECL_CHAIN (decl) = NULL_TREE;
      pushdecl (decl);
      decl = next;
    }
  saved_local_decls = NULL_TREE;
}


#include "gt-fortran-trans-decl.h"
