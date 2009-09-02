/* Common subexpression elimination for GNU compiler.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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

/* Describe a value.  */
typedef struct GTY(()) cselib_val_struct {
  /* The hash value.  */
  unsigned int value;

  /* A VALUE rtx that points back to this structure.  */
  rtx val_rtx;

  /* All rtl expressions that hold this value at the current time during a
     scan.  */
  struct elt_loc_list *locs;

  /* If this value is used as an address, points to a list of values that
     use it as an address in a MEM.  */
  struct elt_list *addr_list;

  struct cselib_val_struct *next_containing_mem;
} cselib_val;

/* A list of rtl expressions that hold the same value.  */
struct GTY(()) elt_loc_list {
  /* Next element in the list.  */
  struct elt_loc_list *next;
  /* An rtl expression that holds the value.  */
  rtx loc;
  /* The insn that made the equivalence.  */
  rtx setting_insn;
};

/* A list of cselib_val structures.  */
struct GTY(()) elt_list {
  struct elt_list *next;
  cselib_val *elt;
};

/* Describe a single set that is part of an insn.  */
struct cselib_set
{
  rtx src;
  rtx dest;
  cselib_val *src_elt;
  cselib_val *dest_addr_elt;
};

extern void (*cselib_discard_hook) (cselib_val *);
extern void (*cselib_record_sets_hook) (rtx insn, struct cselib_set *sets,
					int n_sets);

extern cselib_val *cselib_lookup (rtx, enum machine_mode, int);
extern void cselib_init (bool record_memory);
extern void cselib_clear_table (void);
extern void cselib_finish (void);
extern void cselib_process_insn (rtx);
extern enum machine_mode cselib_reg_set_mode (const_rtx);
extern int rtx_equal_for_cselib_p (rtx, rtx);
extern int references_value_p (const_rtx, int);
extern rtx cselib_expand_value_rtx (rtx, bitmap, int);
typedef rtx (*cselib_expand_callback)(rtx, bitmap, int, void *);
extern rtx cselib_expand_value_rtx_cb (rtx, bitmap, int,
				       cselib_expand_callback, void*);
extern rtx cselib_subst_to_values (rtx);
extern void cselib_invalidate_rtx (rtx);

extern void cselib_reset_table_with_next_value (unsigned int);
extern unsigned int cselib_get_next_unknown_value (void);
extern void cselib_preserve_value (cselib_val *);
extern bool cselib_preserved_value_p (cselib_val *);
extern void cselib_preserve_only_values (bool);

extern void dump_cselib_table (FILE *);
