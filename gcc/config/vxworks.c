/* Common VxWorks target definitions for GNU compiler.
   Copyright (C) 2007-2013 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "diagnostic-core.h"
#include "output.h"
#include "tm.h"
#include "tree.h"

/* Like default_named_section_asm_out_constructor, except that even
   constructors with DEFAULT_INIT_PRIORITY must go in a numbered
   section on VxWorks.  The VxWorks runtime uses a clever trick to get
   the sentinel entry (-1) inserted at the beginning of the .ctors
   segment.  This trick will not work if we ever generate any entries
   in plain .ctors sections; we must always use .ctors.PRIORITY.  */

void
vxworks_asm_out_constructor (rtx symbol, int priority)
{
  section *sec;

  sec = get_cdtor_priority_section (priority,
				    /*constructor_p=*/true);
  assemble_addr_to_section (symbol, sec);
}

/* See comment for vxworks_asm_out_constructor.  */

void
vxworks_asm_out_destructor (rtx symbol, int priority)
{
  section *sec;

  sec = get_cdtor_priority_section (priority,
				    /*constructor_p=*/false);
  assemble_addr_to_section (symbol, sec);
}

/* Return the list of FIELD_DECLs that make up an emulated TLS
   variable's control object.  TYPE is the structure these are fields
   of and *NAME will be filled in with the structure tag that should
   be used.  */

static tree
vxworks_emutls_var_fields (tree type, tree *name)
{
  tree field, next_field;
  
  *name = get_identifier ("__tls_var");
  
  field = build_decl (BUILTINS_LOCATION, FIELD_DECL,
		      get_identifier ("size"), unsigned_type_node);
  DECL_CONTEXT (field) = type;
  next_field = field;

  field = build_decl (BUILTINS_LOCATION, FIELD_DECL,
		      get_identifier ("module_id"), unsigned_type_node);
  DECL_CONTEXT (field) = type;
  DECL_CHAIN (field) = next_field;
  next_field = field;

  field = build_decl (BUILTINS_LOCATION, FIELD_DECL,
		      get_identifier ("offset"), unsigned_type_node);
  DECL_CONTEXT (field) = type;
  DECL_CHAIN (field) = next_field;

  return field;
}

/* Return the CONSTRUCTOR to initialize an emulated TLS control
   object.  VAR is the control object.  DECL is the TLS object itself
   and TMPL_ADDR is the address (an ADDR_EXPR) of the initializer for
   that object.  */

static tree
vxworks_emutls_var_init (tree var, tree decl, tree tmpl_addr)
{
  vec<constructor_elt, va_gc> *v;
  vec_alloc (v, 3);
  
  tree type = TREE_TYPE (var);
  tree field = TYPE_FIELDS (type);
  
  constructor_elt elt = {field, fold_convert (TREE_TYPE (field), tmpl_addr)};
  v->quick_push (elt);
  
  field = DECL_CHAIN (field);
  elt.index = field;
  elt.value = build_int_cst (TREE_TYPE (field), 0);
  v->quick_push (elt);
  
  field = DECL_CHAIN (field);
  elt.index = field;
  elt.value = fold_convert (TREE_TYPE (field), DECL_SIZE_UNIT (decl));
  v->quick_push (elt);
  
  return build_constructor (type, v);
}

/* Do VxWorks-specific parts of TARGET_OPTION_OVERRIDE.  */

void
vxworks_override_options (void)
{
  /* We don't support __thread via target hooks.  */
  targetm.have_tls = false;

  targetm.emutls.get_address = "__builtin___tls_lookup";
  targetm.emutls.register_common = NULL;
  targetm.emutls.var_section = ".tls_vars";
  targetm.emutls.tmpl_section = ".tls_data";
  targetm.emutls.var_prefix = "__tls__";
  targetm.emutls.tmpl_prefix = "";
  targetm.emutls.var_fields = vxworks_emutls_var_fields;
  targetm.emutls.var_init = vxworks_emutls_var_init;
  targetm.emutls.var_align_fixed = true;
  targetm.emutls.debug_form_tls_address = true;
  
  /* We can use .ctors/.dtors sections only in RTP mode.  */
  targetm.have_ctors_dtors = TARGET_VXWORKS_RTP;

  /* PIC is only supported for RTPs.  */
  if (flag_pic && !TARGET_VXWORKS_RTP)
    error ("PIC is only supported for RTPs");

  /* Default to strict dwarf-2 to prevent potential difficulties observed with
     non-gdb debuggers on extensions > 2.  */
  if (!global_options_set.x_dwarf_strict)
    dwarf_strict = 1;

  if (!global_options_set.x_dwarf_version)
    dwarf_version = 2;
}
