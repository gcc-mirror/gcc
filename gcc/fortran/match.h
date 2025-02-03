/* All matcher functions.
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

/* gfc_new_block points to the symbol of a newly matched block.  */
extern gfc_symbol *gfc_new_block;

/* Current statement label.  Zero means no statement label.  Because
   new_st can get wiped during statement matching, we have to keep it
   separate.  */
extern gfc_st_label *gfc_statement_label;

extern int gfc_matching_ptr_assignment;
extern int gfc_matching_procptr_assignment;
extern bool gfc_matching_prefix;

/* Default access specifier while matching procedure bindings.  */
extern gfc_access gfc_typebound_default_access;

/****************** All gfc_match* routines *****************/

/* match.cc.  */

/* Generic match subroutines.  */
match gfc_match_special_char (gfc_char_t *);
match gfc_match_space (void);
match gfc_match_eos (void);
match gfc_match_small_literal_int (int *, int *, bool = true);
match gfc_match_st_label (gfc_st_label **);
match gfc_match_small_int (int *);
match gfc_match_name (char *, bool = true);
match gfc_match_symbol (gfc_symbol **, int);
match gfc_match_sym_tree (gfc_symtree **, int);
match gfc_match_intrinsic_op (gfc_intrinsic_op *);
match gfc_match_char (char, bool = true);
match gfc_match (const char *, ...);
match gfc_match_iterator (gfc_iterator *, int);
match gfc_match_parens (void);
match gfc_match_type_spec (gfc_typespec *);
match gfc_match_member_sep(gfc_symbol *);


/* Statement matchers.  */
match gfc_match_program (void);
match gfc_match_pointer_assignment (void);
match gfc_match_assignment (void);
match gfc_match_if (gfc_statement *);
match gfc_match_else (void);
match gfc_match_elseif (void);
match gfc_match_event_post (void);
match gfc_match_event_wait (void);
match gfc_match_critical (void);
match gfc_match_fail_image (void);
match gfc_match_change_team (void);
match gfc_match_end_team (void);
match gfc_match_form_team (void);
match gfc_match_sync_team (void);
match gfc_match_block (void);
match gfc_match_associate (void);
match gfc_match_do (void);
match gfc_match_cycle (void);
match gfc_match_exit (void);
match gfc_match_lock (void);
match gfc_match_pause (void);
match gfc_match_stop (void);
match gfc_match_error_stop (void);
match gfc_match_continue (void);
match gfc_match_assign (void);
match gfc_match_goto (void);
match gfc_match_sync_all (void);
match gfc_match_sync_images (void);
match gfc_match_sync_memory (void);
match gfc_match_unlock (void);

match gfc_match_allocate (void);
match gfc_match_nullify (void);
match gfc_match_deallocate (void);
match gfc_match_return (void);
match gfc_match_call (void);

/* We want to use this function to check for a common-block-name
   that can exist in a bind statement, so removed the "static"
   declaration of the function in match.cc. */

match gfc_match_common_name (char *name);

match gfc_match_common (void);
match gfc_match_block_data (void);
match gfc_match_namelist (void);
match gfc_match_module (void);
match gfc_match_equivalence (void);
match gfc_match_st_function (void);
match gfc_match_ptr_fcn_assign (void);
match gfc_match_case (void);
match gfc_match_select (void);
match gfc_match_select_type (void);
match gfc_match_type_is (void);
match gfc_match_class_is (void);
match gfc_match_select_rank (void);
match gfc_match_rank_is (void);
match gfc_match_where (gfc_statement *);
match gfc_match_elsewhere (void);
match gfc_match_forall (gfc_statement *);

/* Other functions.  */

gfc_common_head *gfc_get_common (const char *, int);

/* openmp.cc.  */

/* OpenACC directive matchers.  */
match gfc_match_oacc_atomic (void);
match gfc_match_oacc_cache (void);
match gfc_match_oacc_wait (void);
match gfc_match_oacc_update (void);
match gfc_match_oacc_declare (void);
match gfc_match_oacc_loop (void);
match gfc_match_oacc_host_data (void);
match gfc_match_oacc_data (void);
match gfc_match_oacc_kernels (void);
match gfc_match_oacc_kernels_loop (void);
match gfc_match_oacc_parallel (void);
match gfc_match_oacc_parallel_loop (void);
match gfc_match_oacc_serial (void);
match gfc_match_oacc_serial_loop (void);
match gfc_match_oacc_enter_data (void);
match gfc_match_oacc_exit_data (void);
match gfc_match_oacc_routine (void);

/* OpenMP directive matchers.  */
match gfc_match_omp_eos_error (void);
match gfc_match_omp_allocate (void);
match gfc_match_omp_allocators (void);
match gfc_match_omp_assume (void);
match gfc_match_omp_assumes (void);
match gfc_match_omp_atomic (void);
match gfc_match_omp_barrier (void);
match gfc_match_omp_begin_metadirective (void);
match gfc_match_omp_cancel (void);
match gfc_match_omp_cancellation_point (void);
match gfc_match_omp_critical (void);
match gfc_match_omp_declare_reduction (void);
match gfc_match_omp_declare_simd (void);
match gfc_match_omp_declare_target (void);
match gfc_match_omp_declare_variant (void);
match gfc_match_omp_depobj (void);
match gfc_match_omp_dispatch (void);
match gfc_match_omp_distribute (void);
match gfc_match_omp_distribute_parallel_do (void);
match gfc_match_omp_distribute_parallel_do_simd (void);
match gfc_match_omp_distribute_simd (void);
match gfc_match_omp_do (void);
match gfc_match_omp_do_simd (void);
match gfc_match_omp_loop (void);
match gfc_match_omp_error (void);
match gfc_match_omp_flush (void);
match gfc_match_omp_interop (void);
match gfc_match_omp_masked (void);
match gfc_match_omp_masked_taskloop (void);
match gfc_match_omp_masked_taskloop_simd (void);
match gfc_match_omp_master (void);
match gfc_match_omp_master_taskloop (void);
match gfc_match_omp_master_taskloop_simd (void);
match gfc_match_omp_metadirective (void);
match gfc_match_omp_nothing (void);
match gfc_match_omp_ordered (void);
match gfc_match_omp_ordered_depend (void);
match gfc_match_omp_parallel (void);
match gfc_match_omp_parallel_do (void);
match gfc_match_omp_parallel_do_simd (void);
match gfc_match_omp_parallel_loop (void);
match gfc_match_omp_parallel_masked (void);
match gfc_match_omp_parallel_masked_taskloop (void);
match gfc_match_omp_parallel_masked_taskloop_simd (void);
match gfc_match_omp_parallel_master (void);
match gfc_match_omp_parallel_master_taskloop (void);
match gfc_match_omp_parallel_master_taskloop_simd (void);
match gfc_match_omp_parallel_sections (void);
match gfc_match_omp_parallel_workshare (void);
match gfc_match_omp_requires (void);
match gfc_match_omp_scope (void);
match gfc_match_omp_scan (void);
match gfc_match_omp_sections (void);
match gfc_match_omp_simd (void);
match gfc_match_omp_single (void);
match gfc_match_omp_target (void);
match gfc_match_omp_target_data (void);
match gfc_match_omp_target_enter_data (void);
match gfc_match_omp_target_exit_data (void);
match gfc_match_omp_target_parallel (void);
match gfc_match_omp_target_parallel_do (void);
match gfc_match_omp_target_parallel_do_simd (void);
match gfc_match_omp_target_parallel_loop (void);
match gfc_match_omp_target_simd (void);
match gfc_match_omp_target_teams (void);
match gfc_match_omp_target_teams_distribute (void);
match gfc_match_omp_target_teams_distribute_parallel_do (void);
match gfc_match_omp_target_teams_distribute_parallel_do_simd (void);
match gfc_match_omp_target_teams_distribute_simd (void);
match gfc_match_omp_target_teams_loop (void);
match gfc_match_omp_target_update (void);
match gfc_match_omp_task (void);
match gfc_match_omp_taskgroup (void);
match gfc_match_omp_taskloop (void);
match gfc_match_omp_taskloop_simd (void);
match gfc_match_omp_taskwait (void);
match gfc_match_omp_taskyield (void);
match gfc_match_omp_teams (void);
match gfc_match_omp_teams_distribute (void);
match gfc_match_omp_teams_distribute_parallel_do (void);
match gfc_match_omp_teams_distribute_parallel_do_simd (void);
match gfc_match_omp_teams_distribute_simd (void);
match gfc_match_omp_teams_loop (void);
match gfc_match_omp_threadprivate (void);
match gfc_match_omp_tile (void);
match gfc_match_omp_unroll (void);
match gfc_match_omp_workshare (void);
match gfc_match_omp_end_critical (void);
match gfc_match_omp_end_nowait (void);
match gfc_match_omp_end_single (void);

/* decl.cc.  */

match gfc_match_data (void);
match gfc_match_null (gfc_expr **);
match gfc_match_kind_spec (gfc_typespec *, bool);
match gfc_match_decl_type_spec (gfc_typespec *, int);

match gfc_match_end (gfc_statement *);
match gfc_match_data_decl (void);
match gfc_match_formal_arglist (gfc_symbol *, int, int, bool = false);
match gfc_match_procedure (void);
match gfc_match_generic (void);
match gfc_match_function_decl (void);
match gfc_match_entry (void);
match gfc_match_subroutine (void);
match gfc_match_submod_proc (void);
match gfc_match_map (void);
match gfc_match_union (void);
match gfc_match_structure_decl (void);
match gfc_match_derived_decl (void);
match gfc_match_final_decl (void);
match gfc_match_type (gfc_statement *);

match gfc_match_implicit_none (void);
match gfc_match_implicit (void);

void gfc_set_constant_character_len (gfc_charlen_t, gfc_expr *,
				     gfc_charlen_t);

/* Matchers for attribute declarations.  */
match gfc_match_allocatable (void);
match gfc_match_asynchronous (void);
match gfc_match_automatic (void);
match gfc_match_codimension (void);
match gfc_match_contiguous (void);
match gfc_match_dimension (void);
match gfc_match_external (void);
match gfc_match_gcc_attributes (void);
match gfc_match_gcc_builtin (void);
match gfc_match_gcc_ivdep (void);
match gfc_match_gcc_novector (void);
match gfc_match_gcc_unroll (void);
match gfc_match_gcc_vector (void);
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
match gfc_match_static (void);
match gfc_match_modproc (void);
match gfc_match_target (void);
match gfc_match_value (void);
match gfc_match_volatile (void);

/* decl.cc.  */

/* Fortran 2003 c interop.
   TODO: some of these should be moved to another file rather than decl.cc */
match gfc_match_bind_c_stmt (void);
match gfc_match_bind_c (gfc_symbol *, bool);

/* primary.cc.  */
match gfc_match_structure_constructor (gfc_symbol *, gfc_symtree *, gfc_expr **);
match gfc_match_variable (gfc_expr **, int);
match gfc_match_equiv_variable (gfc_expr **);
match gfc_match_actual_arglist (int, gfc_actual_arglist **, bool = false);
match gfc_match_literal_constant (gfc_expr **, int);

/* expr.cc -- FIXME: this one should be eliminated by moving the
   matcher to matchexp.cc and a call to a new function in expr.cc that
   only makes sure the init expr. is valid.  */
bool gfc_reduce_init_expr (gfc_expr *expr);
match gfc_match_init_expr (gfc_expr **);

/* array.cc.  */
match gfc_match_array_spec (gfc_array_spec **, bool, bool);
match gfc_match_array_ref (gfc_array_ref *, gfc_array_spec *, int, int,
			   bool = false);
match gfc_match_array_constructor (gfc_expr **);

/* interface.cc.  */
match gfc_match_abstract_interface (void);
match gfc_match_generic_spec (interface_type *, char *, gfc_intrinsic_op *);
match gfc_match_interface (void);
match gfc_match_end_interface (void);

/* io.cc.  */
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

/* matchexp.cc.  */
match gfc_match_defined_op_name (char *, int);
match gfc_match_expr (gfc_expr **);

/* module.cc.  */
match gfc_match_use (void);
match gfc_match_submodule (void);
void gfc_use_modules (void);

#endif  /* GFC_MATCH_H  */

