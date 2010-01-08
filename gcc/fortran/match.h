/* All matcher functions.
   Copyright (C) 2003, 2005, 2007, 2008
   Free Software Foundation, Inc.
   Contributed by Steven Bosscher

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


#ifndef GFC_MATCH_H
#define GFC_MATCH_H

#include "gfortran.h"

/* gfc_new_block points to the symbol of a newly matched block.  */
extern gfc_symbol *gfc_new_block;

/* Current statement label.  Zero means no statement label.  Because
   new_st can get wiped during statement matching, we have to keep it
   separate.  */
extern gfc_st_label *gfc_statement_label;

extern int gfc_matching_procptr_assignment;
extern bool gfc_matching_prefix;

/* Default access specifier while matching procedure bindings.  */
extern gfc_access gfc_typebound_default_access;

/****************** All gfc_match* routines *****************/

/* match.c.  */

/* Generic match subroutines.  */
match gfc_match_special_char (gfc_char_t *);
match gfc_match_space (void);
match gfc_match_eos (void);
match gfc_match_small_literal_int (int *, int *);
match gfc_match_st_label (gfc_st_label **);
match gfc_match_label (void);
match gfc_match_small_int (int *);
match gfc_match_small_int_expr (int *, gfc_expr **);
match gfc_match_name (char *);
match gfc_match_name_C (char *buffer);
match gfc_match_symbol (gfc_symbol **, int);
match gfc_match_sym_tree (gfc_symtree **, int);
match gfc_match_intrinsic_op (gfc_intrinsic_op *);
match gfc_match_char (char);
match gfc_match (const char *, ...);
match gfc_match_iterator (gfc_iterator *, int);
match gfc_match_parens (void);

/* Statement matchers.  */
match gfc_match_program (void);
match gfc_match_pointer_assignment (void);
match gfc_match_assignment (void);
match gfc_match_if (gfc_statement *);
match gfc_match_else (void);
match gfc_match_elseif (void);
match gfc_match_block (void);
match gfc_match_do (void);
match gfc_match_cycle (void);
match gfc_match_exit (void);
match gfc_match_pause (void);
match gfc_match_stop (void);
match gfc_match_continue (void);
match gfc_match_assign (void);
match gfc_match_goto (void);

match gfc_match_allocate (void);
match gfc_match_nullify (void);
match gfc_match_deallocate (void);
match gfc_match_return (void);
match gfc_match_call (void);

/* We want to use this function to check for a common-block-name
   that can exist in a bind statement, so removed the "static"
   declaration of the function in match.c.
 
   TODO: should probably rename this now that it'll be globally seen to
   gfc_match_common_name.  */
match match_common_name (char *name);

match gfc_match_common (void);
match gfc_match_block_data (void);
match gfc_match_namelist (void);
match gfc_match_module (void);
match gfc_match_equivalence (void);
match gfc_match_st_function (void);
match gfc_match_case (void);
match gfc_match_select (void);
match gfc_match_select_type (void);
match gfc_match_type_is (void);
match gfc_match_class_is (void);
match gfc_match_where (gfc_statement *);
match gfc_match_elsewhere (void);
match gfc_match_forall (gfc_statement *);

/* Other functions.  */

gfc_common_head *gfc_get_common (const char *, int);

/* openmp.c.  */

/* OpenMP directive matchers.  */
match gfc_match_omp_eos (void);
match gfc_match_omp_atomic (void);
match gfc_match_omp_barrier (void);
match gfc_match_omp_critical (void);
match gfc_match_omp_do (void);
match gfc_match_omp_flush (void);
match gfc_match_omp_master (void);
match gfc_match_omp_ordered (void);
match gfc_match_omp_parallel (void);
match gfc_match_omp_parallel_do (void);
match gfc_match_omp_parallel_sections (void);
match gfc_match_omp_parallel_workshare (void);
match gfc_match_omp_sections (void);
match gfc_match_omp_single (void);
match gfc_match_omp_task (void);
match gfc_match_omp_taskwait (void);
match gfc_match_omp_threadprivate (void);
match gfc_match_omp_workshare (void);
match gfc_match_omp_end_nowait (void);
match gfc_match_omp_end_single (void);

/* decl.c.  */

match gfc_match_data (void);
match gfc_match_null (gfc_expr **);
match gfc_match_kind_spec (gfc_typespec *, bool);
match gfc_match_old_kind_spec (gfc_typespec *);
match gfc_match_decl_type_spec (gfc_typespec *, int);

match gfc_match_end (gfc_statement *);
match gfc_match_data_decl (void);
match gfc_match_formal_arglist (gfc_symbol *, int, int);
match gfc_match_procedure (void);
match gfc_match_generic (void);
match gfc_match_function_decl (void);
match gfc_match_entry (void);
match gfc_match_subroutine (void);
match gfc_match_derived_decl (void);
match gfc_match_final_decl (void);

match gfc_match_implicit_none (void);
match gfc_match_implicit (void);

void gfc_set_constant_character_len (int, gfc_expr *, int);

/* Matchers for attribute declarations.  */
match gfc_match_allocatable (void);
match gfc_match_asynchronous (void);
match gfc_match_dimension (void);
match gfc_match_external (void);
match gfc_match_gcc_attributes (void);
match gfc_match_import (void);
match gfc_match_intent (void);
match gfc_match_intrinsic (void);
match gfc_match_optional (void);
match gfc_match_parameter (void);
match gfc_match_pointer (void);
match gfc_match_protected (void);
match gfc_match_private (gfc_statement *);
match gfc_match_public (gfc_statement *);
match gfc_match_save (void);
match gfc_match_modproc (void);
match gfc_match_target (void);
match gfc_match_value (void);
match gfc_match_volatile (void);

/* decl.c.  */

/* Fortran 2003 c interop.
   TODO: some of these should be moved to another file rather than decl.c */
void set_com_block_bind_c (gfc_common_head *, int);
gfc_try set_binding_label (char *, const char *, int);
gfc_try set_verify_bind_c_sym (gfc_symbol *, int);
gfc_try set_verify_bind_c_com_block (gfc_common_head *, int);
gfc_try get_bind_c_idents (void);
match gfc_match_bind_c_stmt (void);
match gfc_match_suffix (gfc_symbol *, gfc_symbol **);
match gfc_match_bind_c (gfc_symbol *, bool);
match gfc_get_type_attr_spec (symbol_attribute *, char*);

/* primary.c.  */
match gfc_match_structure_constructor (gfc_symbol *, gfc_expr **, bool);
match gfc_match_variable (gfc_expr **, int);
match gfc_match_equiv_variable (gfc_expr **);
match gfc_match_actual_arglist (int, gfc_actual_arglist **);
match gfc_match_literal_constant (gfc_expr **, int);

/* expr.c -- FIXME: this one should be eliminated by moving the
   matcher to matchexp.c and a call to a new function in expr.c that
   only makes sure the init expr. is valid.  */
gfc_try gfc_reduce_init_expr (gfc_expr *expr);
match gfc_match_init_expr (gfc_expr **);

/* array.c.  */
match gfc_match_array_spec (gfc_array_spec **);
match gfc_match_array_ref (gfc_array_ref *, gfc_array_spec *, int);
match gfc_match_array_constructor (gfc_expr **);

/* interface.c.  */
match gfc_match_abstract_interface (void);
match gfc_match_generic_spec (interface_type *, char *, gfc_intrinsic_op *);
match gfc_match_interface (void);
match gfc_match_end_interface (void);

/* io.c.  */
match gfc_match_format (void);
match gfc_match_open (void);
match gfc_match_close (void);
match gfc_match_endfile (void);
match gfc_match_backspace (void);
match gfc_match_rewind (void);
match gfc_match_flush (void);
match gfc_match_inquire (void);
match gfc_match_read (void);
match gfc_match_wait (void);
match gfc_match_write (void);
match gfc_match_print (void);

/* matchexp.c.  */
match gfc_match_defined_op_name (char *, int);
match gfc_match_expr (gfc_expr **);

/* module.c.  */
match gfc_match_use (void);
void gfc_use_module (void);

#endif  /* GFC_MATCH_H  */

