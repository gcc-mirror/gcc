/* Header for statement translation functions
   Copyright (C) 2002-2025 Free Software Foundation, Inc.
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

/* Statement translators (gfc_trans_*) return a fully translated tree.
   Calls gfc_trans_*.  */
tree gfc_trans_code (gfc_code *);

/* Wrapper function used to pass a check condition for implied DO loops.  */
tree gfc_trans_code_cond (gfc_code *, tree);

/* All other gfc_trans_* should only need be called by gfc_trans_code */

/* trans-expr.cc */
tree gfc_trans_assign (gfc_code *);
tree gfc_trans_pointer_assign (gfc_code *);
tree gfc_trans_init_assign (gfc_code *);
tree gfc_trans_class_init_assign (gfc_code *);

/* trans-stmt.cc */
tree gfc_trans_cycle (gfc_code *);
tree gfc_trans_critical (gfc_code *);
tree gfc_trans_exit (gfc_code *);
tree gfc_trans_label_assign (gfc_code *);
tree gfc_trans_label_here (gfc_code *);
tree gfc_trans_goto (gfc_code *);
tree gfc_trans_entry (gfc_code *);
tree gfc_trans_pause (gfc_code *);
tree gfc_trans_stop (gfc_code *, bool);
tree gfc_trans_call (gfc_code *, bool, tree, tree, bool);
tree gfc_trans_return (gfc_code *);
tree gfc_trans_if (gfc_code *);
tree gfc_trans_arithmetic_if (gfc_code *);
tree gfc_trans_block_construct (gfc_code *);
tree gfc_trans_do (gfc_code *, tree);
tree gfc_trans_do_concurrent (gfc_code *);
tree gfc_trans_do_while (gfc_code *);
tree gfc_trans_select (gfc_code *);
tree gfc_trans_select_type (gfc_code *);
tree gfc_trans_select_rank (gfc_code *);
tree gfc_trans_sync (gfc_code *, gfc_exec_op);
tree gfc_trans_lock_unlock (gfc_code *, gfc_exec_op);
tree gfc_trans_event_post_wait (gfc_code *, gfc_exec_op);
tree gfc_trans_fail_image (gfc_code *);
tree gfc_trans_forall (gfc_code *);
tree gfc_trans_form_team (gfc_code *);
tree gfc_trans_change_team (gfc_code *);
tree gfc_trans_end_team (gfc_code *);
tree gfc_trans_sync_team (gfc_code *);
tree gfc_trans_where (gfc_code *);
tree gfc_trans_allocate (gfc_code *, gfc_omp_namelist *);
tree gfc_trans_deallocate (gfc_code *);

/* trans-openmp.cc */
tree gfc_trans_omp_directive (gfc_code *);
void gfc_trans_omp_declare_simd (gfc_namespace *);
void gfc_trans_omp_declare_variant (gfc_namespace *);
tree gfc_trans_omp_metadirective (gfc_code *code);
tree gfc_trans_oacc_directive (gfc_code *);
tree gfc_trans_oacc_declare (gfc_namespace *);

/* trans-io.cc */
tree gfc_trans_open (gfc_code *);
tree gfc_trans_close (gfc_code *);
tree gfc_trans_read (gfc_code *);
tree gfc_trans_write (gfc_code *);
tree gfc_trans_iolength (gfc_code *);
tree gfc_trans_backspace (gfc_code *);
tree gfc_trans_endfile (gfc_code *);
tree gfc_trans_inquire (gfc_code *);
tree gfc_trans_rewind (gfc_code *);
tree gfc_trans_flush (gfc_code *);

tree gfc_trans_transfer (gfc_code *);
tree gfc_trans_dt_end (gfc_code *);
tree gfc_trans_wait (gfc_code *);
