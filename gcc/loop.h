/* Loop optimization definitions for GNU C-Compiler
   Copyright (C) 1991, 1995, 1998, 1999, 2000 Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "varray.h"
#include "basic-block.h"

/* Get the loop info pointer of a loop.  */
#define LOOP_INFO(LOOP) ((struct loop_info *) (LOOP)->aux) 

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
				/* For a biv, this is the place where add_val
				   was found.  */
  enum machine_mode mode;	/* The mode of this biv or giv */
  enum machine_mode mem_mode;	/* For DEST_ADDR, mode of the memory object. */
  rtx mult_val;			/* Multiplicative factor for src_reg.  */
  rtx add_val;			/* Additive constant for that product.  */
  int benefit;			/* Gain from eliminating this insn.  */
  rtx final_value;		/* If the giv is used outside the loop, and its
				   final value could be calculated, it is put
				   here, and the giv is made replaceable.  Set
				   the giv to this value before the loop.  */
  unsigned combined_with;	/* The number of givs this giv has been
				   combined with.  If nonzero, this giv
				   cannot combine with any other giv.  */
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
  unsigned always_computable : 1;/* 1 if this value is computable every
				    iteration.  */
  unsigned always_executed : 1; /* 1 if this set occurs each iteration.  */
  unsigned maybe_multiple : 1;	/* Only used for a biv and  1 if this biv
				   update may be done multiple times per
				   iteration. */
  unsigned cant_derive : 1;	/* For giv's, 1 if this giv cannot derive
				   another giv.  This occurs in many cases
				   where a giv's lifetime spans an update to
				   a biv. */
  unsigned maybe_dead : 1;	/* 1 if this giv might be dead.  In that case,
				   we won't use it to eliminate a biv, it
				   would probably lose. */
  unsigned auto_inc_opt : 1;	/* 1 if this giv had its increment output next
				   to it to try to form an auto-inc address. */
  unsigned unrolled : 1;	/* 1 if new register has been allocated and
				   initialized in unrolled loop.  */
  unsigned shared : 1;
  unsigned no_const_addval : 1; /* 1 if add_val does not contain a const. */
  unsigned multi_insn_incr : 1;	/* 1 if multiple insns updated the biv.  */
  int lifetime;			/* Length of life of this giv */
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
  struct induction *derived_from;/* For a giv, if we decided to derive this
				   giv from another one.  */
  HOST_WIDE_INT const_adjust;	/* Used by loop unrolling, when an address giv
				   is split, and a constant is eliminated from
				   the address, the -constant is stored here
				   for later use. */
  int ix;			/* Used by recombine_givs, as n index into
				   the stats array.  */
  struct induction *same_insn;	/* If there are multiple identical givs in
				   the same insn, then all but one have this
				   field set, and they all point to the giv
				   that doesn't have this field set.  */
  rtx last_use;			/* For a giv made from a biv increment, this is
				   a substitute for the lifetime information. */
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

/* Information required to calculate the number of loop iterations. 
   This is set by loop_iterations.  */

struct loop_info
{
  /* Nonzero if there is a subroutine call in the current loop.  */
  int has_call;
  /* Nonzero if there is a volatile memory reference in the current
     loop.  */
  int has_volatile;
  /* Nonzero if there is a tablejump in the current loop.  */
  int has_tablejump;
  /* Nonzero if there are ways to leave the loop other than falling
     off the end.  */
  int has_multiple_exit_targets;
  /* Nonzero if there is an indirect jump in the current function.  */
  int has_indirect_jump;
  /* Register or constant initial loop value.  */
  rtx initial_value;
  /* Register or constant value used for comparison test.  */
  rtx comparison_value;
  /* Register or constant approximate final value.  */
  rtx final_value;
  /* Register or constant initial loop value with term common to
     final_value removed.  */
  rtx initial_equiv_value;
  /* Register or constant final loop value with term common to
     initial_value removed.  */
  rtx final_equiv_value;
  /* Register corresponding to iteration variable.  */
  rtx iteration_var;
  /* Constant loop increment.  */
  rtx increment;
  enum rtx_code comparison_code;
  /* Holds the number of loop iterations.  It is zero if the number
     could not be calculated.  Must be unsigned since the number of
     iterations can be as high as 2^wordsize - 1.  For loops with a
     wider iterator, this number will be zero if the number of loop
     iterations is too large for an unsigned integer to hold.  */
  unsigned HOST_WIDE_INT n_iterations;
  /* The number of times the loop body was unrolled.  */
  unsigned int unroll_number;
  int used_count_register;
};

/* Definitions used by the basic induction variable discovery code.  */
enum iv_mode { UNKNOWN_INDUCT, BASIC_INDUCT, NOT_BASIC_INDUCT,
		 GENERAL_INDUCT };

/* Variables declared in loop.c, but also needed in unroll.c.  */

extern int *uid_luid;
extern int max_uid_for_loop;
extern int max_reg_before_loop;
extern struct loop **uid_loop;
extern FILE *loop_dump_stream;

extern varray_type reg_iv_type;
extern varray_type reg_iv_info;

#define REG_IV_TYPE(n) \
  (*(enum iv_mode *) &VARRAY_INT(reg_iv_type, (n)))
#define REG_IV_INFO(n) \
  (*(struct induction **) &VARRAY_GENERIC_PTR(reg_iv_info, (n)))

extern struct iv_class **reg_biv_class;
extern struct iv_class *loop_iv_list;

extern int first_increment_giv, last_increment_giv;

/* Forward declarations for non-static functions declared in loop.c and
   unroll.c.  */
int loop_invariant_p PARAMS ((const struct loop *, rtx));
rtx get_condition_for_loop PARAMS ((const struct loop *, rtx));
void emit_iv_add_mult PARAMS ((rtx, rtx, rtx, rtx, rtx));
rtx express_from PARAMS ((struct induction *, struct induction *));

void unroll_loop PARAMS ((struct loop *, int, rtx, int));
rtx biv_total_increment PARAMS ((struct iv_class *));
unsigned HOST_WIDE_INT loop_iterations PARAMS ((struct loop *));
int precondition_loop_p PARAMS ((const struct loop *,
			       rtx *, rtx *, rtx *, 
			       enum machine_mode *mode));
rtx final_biv_value PARAMS ((const struct loop *, struct iv_class *));
rtx final_giv_value PARAMS ((const struct loop *, struct induction *));
void emit_unrolled_add PARAMS ((rtx, rtx, rtx));
int back_branch_in_range_p PARAMS ((const struct loop *, rtx));

int loop_insn_first_p PARAMS ((rtx, rtx));

