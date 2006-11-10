/* Header for statement translation functions
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Paul Brook

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* Statement translators (gfc_trans_*) return a fully translated tree.
   Calls gfc_trans_*.  */
tree gfc_trans_code (gfc_code *);

/* All other gfc_trans_* should only need be called by gfc_trans_code */

/* trans-expr.c */
tree gfc_trans_assign (gfc_code *);
tree gfc_trans_pointer_assign (gfc_code *);
tree gfc_trans_init_assign (gfc_code *);

/* trans-stmt.c */
tree gfc_trans_cycle (gfc_code *);
tree gfc_trans_exit (gfc_code *);
tree gfc_trans_label_assign (gfc_code *);
tree gfc_trans_label_here (gfc_code *);
tree gfc_trans_goto (gfc_code *);
tree gfc_trans_entry (gfc_code *);
tree gfc_trans_pause (gfc_code *);
tree gfc_trans_stop (gfc_code *);
tree gfc_trans_call (gfc_code *, bool);
tree gfc_trans_return (gfc_code *);
tree gfc_trans_if (gfc_code *);
tree gfc_trans_arithmetic_if (gfc_code *);
tree gfc_trans_do (gfc_code *);
tree gfc_trans_do_while (gfc_code *);
tree gfc_trans_select (gfc_code *);
tree gfc_trans_forall (gfc_code *);
tree gfc_trans_where (gfc_code *);
tree gfc_trans_allocate (gfc_code *);
tree gfc_trans_deallocate (gfc_code *);
tree gfc_trans_deallocate_array (tree);

/* trans-io.c */
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
