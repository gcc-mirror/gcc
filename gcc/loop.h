/* Loop optimization definitions for GNU C-Compiler
   Copyright (C) 1991 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Get the luid of an insn.  Catch the error of trying to reference the LUID
   of an insn added during loop, since these don't have LUIDs.  */

#define INSN_LUID(INSN)			\
  (INSN_UID (INSN) < max_uid_for_loop ? uid_luid[INSN_UID (INSN)] \
   : (abort (), -1))

/* A "basic induction variable" or biv is a pseudo reg that is set
   (within this loop) only by incrementing or decrementing it.  */
/* A "general induction variable" or giv is a pseudo reg whose
   value is a linear function of a biv.  */

/* Bivs are recognized by `basic_induction_var';
   Givs by `general_induct_var'.  */

/* An enum for the two different types of givs, those that are used
   as memory addresses and those that are calculated into registers.  */
enum g_types { DEST_ADDR, DEST_REG };

/* A `struct induction' is created for every instruction that sets
   an induction variable (either a biv or a giv).  */

struct induction
{
  rtx insn;			/* The insn that sets a biv or giv */
  rtx new_reg;			/* New register, containing strength reduced
				   version of this giv.  */
  rtx src_reg;			/* Biv from which this giv is computed.
				   (If this is a biv, then this is the biv.) */
  enum g_types giv_type;	/* Indicate whether DEST_ADDR or DEST_REG */
  rtx dest_reg;			/* Destination register for insn: this is the
				   register which was the biv or giv.
				   For a biv, this equals src_reg.
				   For a DEST_ADDR type giv, this is 0.  */
  rtx *location;		/* Place in the insn where this giv occurs.
				   If GIV_TYPE is DEST_REG, this is 0.  */
  enum machine_mode mode;	/* The mode of this biv or giv */
  enum machine_mode mem_mode;	/* For DEST_ADDR, mode of the memory object. */
  rtx mult_val;			/* Multiplicative factor for src_reg.  */
  rtx add_val;			/* Additive constant for that product.  */
  int benefit;			/* Gain from eliminating this insn.  */
  rtx final_value;		/* If the giv is used outside the loop, and its
				   final value could be calculated, it is put
				   here, and the giv is made replaceable.  Set
				   the giv to this value before the loop.  */
  unsigned replaceable : 1;	/* 1 if we can substitute the strength-reduced
				   variable for the original variable.
				   0 means they must be kept separate and the
				   new one must be copied into the old pseudo
				   reg each time the old one is set.  */
  unsigned not_replaceable : 1;	/* Used to prevent duplicating work.  This is
				   1 if we know that the giv definitely can
				   not be made replaceable, in which case we
				   don't bother checking the variable again
				   even if further info is available.
				   Both this and the above can be zero.  */
  unsigned ignore : 1;		/* 1 prohibits further processing of giv */
  unsigned always_computable : 1;/* 1 if this set occurs each iteration */
  unsigned maybe_multiple : 1;	/* Only used for a biv and  1 if this biv
				   update may be done multiple times per
				   iteration. */
  unsigned cant_derive : 1;	/* For giv's, 1 if this giv cannot derive
				   another giv.  This occurs in many cases
				   where a giv's lifetime spans an update to
				   a biv. */
  unsigned combined_with : 1;	/* 1 if this giv has been combined with.  It
				   then cannot combine with any other giv.  */
  unsigned maybe_dead : 1;	/* 1 if this giv might be dead.  In that case,
				   we won't use it to eliminate a biv, it
				   would probably lose. */
  int lifetime;			/* Length of life of this giv */
  int times_used;		/* # times this giv is used. */
  rtx derive_adjustment;	/* If nonzero, is an adjustment to be
				   subtracted from add_val when this giv
				   derives another.  This occurs when the
				   giv spans a biv update by incrementation. */
  struct induction *next_iv;	/* For givs, links together all givs that are
				   based on the same biv.  For bivs, links
				   together all biv entries that refer to the
				   same biv register.  */
  struct induction *same;	/* If this giv has been combined with another
				   giv, this points to the base giv.  The base
				   giv will have COMBINED_WITH non-zero.  */
  HOST_WIDE_INT const_adjust;	/* Used by loop unrolling, when an address giv
				   is split, and a constant is eliminated from
				   the address, the -constant is stored here
				   for later use. */
};

/* A `struct iv_class' is created for each biv.  */

struct iv_class {
  int regno;			/* Pseudo reg which is the biv.  */
  int biv_count;		/* Number of insns setting this reg.  */
  struct induction *biv;	/* List of all insns that set this reg.  */
  int giv_count;		/* Number of DEST_REG givs computed from this
				   biv.  The resulting count is only used in
				   check_dbra_loop.  */
  struct induction *giv;	/* List of all insns that compute a giv
				   from this reg.  */
  int total_benefit;		/* Sum of BENEFITs of all those givs */
  rtx initial_value;		/* Value of reg at loop start */
  rtx initial_test;		/* Test performed on BIV before loop */
  struct iv_class *next;	/* Links all class structures together */
  rtx init_insn;		/* insn which initializes biv, 0 if none. */
  rtx init_set;			/* SET of INIT_INSN, if any. */
  unsigned incremented : 1;	/* 1 if somewhere incremented/decremented */
  unsigned eliminable : 1;	/* 1 if plausible candidate for elimination. */
  unsigned nonneg : 1;		/* 1 if we added a REG_NONNEG note for this. */
  unsigned reversed : 1;	/* 1 if we reversed the loop that this
				   biv controls. */
};

/* Definitions used by the basic induction variable discovery code.  */
enum iv_mode { UNKNOWN_INDUCT, BASIC_INDUCT, NOT_BASIC_INDUCT,
		 GENERAL_INDUCT };

/* Variables declared in loop.c, but also needed in unroll.c.  */

extern int *uid_luid;
extern int max_uid_for_loop;
extern int *uid_loop_num;
extern int *loop_outer_loop;
extern rtx *loop_number_exit_labels;
extern unsigned HOST_WIDE_INT loop_n_iterations;
extern int max_reg_before_loop;

extern FILE *loop_dump_stream;

extern enum iv_mode *reg_iv_type;
extern struct induction **reg_iv_info;
extern struct iv_class **reg_biv_class;
extern struct iv_class *loop_iv_list;

/* Forward declarations for non-static functions declared in loop.c and
   unroll.c.  */
int invariant_p ();
rtx get_condition_for_loop ();
void emit_iv_add_mult ();

/* Forward declarations for non-static functions declared in stmt.c.  */
void find_loop_tree_blocks ();
void unroll_block_trees ();

void unroll_loop ();
rtx biv_total_increment ();
unsigned HOST_WIDE_INT loop_iterations ();
rtx final_biv_value ();
rtx final_giv_value ();
void emit_unrolled_add ();
