/* Loop optimization definitions for GNU C-Compiler
   Copyright (C) 1991, 1995, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "bitmap.h"
#include "sbitmap.h"
#include "hard-reg-set.h"
#include "basic-block.h"

/* Flags passed to loop_optimize.  */
#define LOOP_UNROLL 1
#define LOOP_BCT 2
#define LOOP_PREFETCH 4

/* Get the loop info pointer of a loop.  */
#define LOOP_INFO(LOOP) ((struct loop_info *) (LOOP)->aux)

/* Get a pointer to the loop movables structure.  */
#define LOOP_MOVABLES(LOOP) (&LOOP_INFO (LOOP)->movables)

/* Get a pointer to the loop registers structure.  */
#define LOOP_REGS(LOOP) (&LOOP_INFO (LOOP)->regs)

/* Get a pointer to the loop induction variables structure.  */
#define LOOP_IVS(LOOP) (&LOOP_INFO (LOOP)->ivs)

/* Get the luid of an insn.  Catch the error of trying to reference the LUID
   of an insn added during loop, since these don't have LUIDs.  */

#define INSN_LUID(INSN)			\
  (INSN_UID (INSN) < max_uid_for_loop ? uid_luid[INSN_UID (INSN)] \
   : (abort (), -1))

#define REGNO_FIRST_LUID(REGNO) uid_luid[REGNO_FIRST_UID (REGNO)]
#define REGNO_LAST_LUID(REGNO) uid_luid[REGNO_LAST_UID (REGNO)]


/* A "basic induction variable" or biv is a pseudo reg that is set
   (within this loop) only by incrementing or decrementing it.  */
/* A "general induction variable" or giv is a pseudo reg whose
   value is a linear function of a biv.  */

/* Bivs are recognized by `basic_induction_var';
   Givs by `general_induction_var'.  */

/* An enum for the two different types of givs, those that are used
   as memory addresses and those that are calculated into registers.  */
enum g_types
{
  DEST_ADDR,
  DEST_REG
};


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
  rtx mem;			/* For DEST_ADDR, the memory object.  */
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
				   iteration.  */
  unsigned cant_derive : 1;	/* For giv's, 1 if this giv cannot derive
				   another giv.  This occurs in many cases
				   where a giv's lifetime spans an update to
				   a biv.  */
  unsigned maybe_dead : 1;	/* 1 if this giv might be dead.  In that case,
				   we won't use it to eliminate a biv, it
				   would probably lose.  */
  unsigned auto_inc_opt : 1;	/* 1 if this giv had its increment output next
				   to it to try to form an auto-inc address.  */
  unsigned unrolled : 1;	/* 1 if new register has been allocated and
				   initialized in unrolled loop.  */
  unsigned shared : 1;
  unsigned no_const_addval : 1; /* 1 if add_val does not contain a const.  */
  int lifetime;			/* Length of life of this giv */
  rtx derive_adjustment;	/* If nonzero, is an adjustment to be
				   subtracted from add_val when this giv
				   derives another.  This occurs when the
				   giv spans a biv update by incrementation.  */
  rtx ext_dependent;		/* If nonzero, is a sign or zero extension
				   if a biv on which this giv is dependent.  */
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
				   for later use.  */
  struct induction *same_insn;	/* If there are multiple identical givs in
				   the same insn, then all but one have this
				   field set, and they all point to the giv
				   that doesn't have this field set.  */
  rtx last_use;			/* For a giv made from a biv increment, this is
				   a substitute for the lifetime information.  */
};


/* A `struct iv_class' is created for each biv.  */

struct iv_class
{
  unsigned int regno;		/* Pseudo reg which is the biv.  */
  int biv_count;		/* Number of insns setting this reg.  */
  struct induction *biv;	/* List of all insns that set this reg.  */
  int giv_count;		/* Number of DEST_REG givs computed from this
				   biv.  The resulting count is only used in
				   check_dbra_loop.  */
  struct induction *giv;	/* List of all insns that compute a giv
				   from this reg.  */
  int total_benefit;		/* Sum of BENEFITs of all those givs.  */
  rtx initial_value;		/* Value of reg at loop start.  */
  rtx initial_test;		/* Test performed on BIV before loop.  */
  rtx final_value;		/* Value of reg at loop end, if known.  */
  struct iv_class *next;	/* Links all class structures together.  */
  rtx init_insn;		/* insn which initializes biv, 0 if none.  */
  rtx init_set;			/* SET of INIT_INSN, if any.  */
  unsigned incremented : 1;	/* 1 if somewhere incremented/decremented */
  unsigned eliminable : 1;	/* 1 if plausible candidate for
                                   elimination.  */
  unsigned nonneg : 1;		/* 1 if we added a REG_NONNEG note for
                                   this.  */
  unsigned reversed : 1;	/* 1 if we reversed the loop that this
				   biv controls.  */
  unsigned all_reduced : 1;	/* 1 if all givs using this biv have
                                   been reduced.  */
};


/* Definitions used by the basic induction variable discovery code.  */
enum iv_mode
{
  UNKNOWN_INDUCT,
  BASIC_INDUCT,
  NOT_BASIC_INDUCT,
  GENERAL_INDUCT
};


/* A `struct iv' is created for every register.  */

struct iv
{
  enum iv_mode type;
  union 
  {
    struct iv_class *class;
    struct induction *info;
  } iv;
};


#define REG_IV_TYPE(ivs, n) ivs->regs[n].type
#define REG_IV_INFO(ivs, n) ivs->regs[n].iv.info
#define REG_IV_CLASS(ivs, n) ivs->regs[n].iv.class


struct loop_ivs
{
  /* Indexed by register number, contains pointer to `struct
     iv' if register is an induction variable.  */
  struct iv *regs;

  /* Size of regs array.  */
  unsigned int n_regs;

  /* The head of a list which links together (via the next field)
     every iv class for the current loop.  */
  struct iv_class *list;
};


typedef struct loop_mem_info
{
  rtx mem;      /* The MEM itself.  */
  rtx reg;      /* Corresponding pseudo, if any.  */
  int optimize; /* Nonzero if we can optimize access to this MEM.  */
} loop_mem_info;



struct loop_reg
{
  /* Number of times the reg is set during the loop being scanned.
     During code motion, a negative value indicates a reg that has
     been made a candidate; in particular -2 means that it is an
     candidate that we know is equal to a constant and -1 means that
     it is an candidate not known equal to a constant.  After code
     motion, regs moved have 0 (which is accurate now) while the
     failed candidates have the original number of times set.

     Therefore, at all times, == 0 indicates an invariant register;
     < 0 a conditionally invariant one.  */
  int set_in_loop;

  /* Original value of set_in_loop; same except that this value
     is not set negative for a reg whose sets have been made candidates
     and not set to 0 for a reg that is moved.  */
  int n_times_set;

  /* Contains the insn in which a register was used if it was used
     exactly once; contains const0_rtx if it was used more than once.  */
  rtx single_usage;

  /* Nonzero indicates that the register cannot be moved or strength
     reduced.  */
  char may_not_optimize;

  /* Nonzero means reg N has already been moved out of one loop.
     This reduces the desire to move it out of another.  */
  char moved_once;
};


struct loop_regs
{
  int num;			/* Number of regs used in table.  */
  int size;			/* Size of table.  */
  struct loop_reg *array;	/* Register usage info. array.  */
  int multiple_uses;		/* Nonzero if a reg has multiple uses.  */
};



struct loop_movables
{
  /* Head of movable chain.  */
  struct movable *head;
  /* Last movable in chain.  */
  struct movable *last;
};


/* Information pertaining to a loop.  */

struct loop_info
{
  /* Nonzero if there is a subroutine call in the current loop.  */
  int has_call;
  /* Nonzero if there is a libcall in the current loop.  */
  int has_libcall;
  /* Nonzero if there is a non constant call in the current loop.  */
  int has_nonconst_call;
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
  /* The loop iterator induction variable.  */
  struct iv_class *iv;
  /* List of MEMs that are stored in this loop.  */
  rtx store_mems;
  /* Array of MEMs that are used (read or written) in this loop, but
     cannot be aliased by anything in this loop, except perhaps
     themselves.  In other words, if mems[i] is altered during
     the loop, it is altered by an expression that is rtx_equal_p to
     it.  */
  loop_mem_info *mems;
  /* The index of the next available slot in MEMS.  */
  int mems_idx;
  /* The number of elements allocated in MEMS.  */
  int mems_allocated;
  /* Nonzero if we don't know what MEMs were changed in the current
     loop.  This happens if the loop contains a call (in which case
     `has_call' will also be set) or if we store into more than
     NUM_STORES MEMs.  */
  int unknown_address_altered;
  /* The above doesn't count any readonly memory locations that are
     stored.  This does.  */
  int unknown_constant_address_altered;
  /* Count of memory write instructions discovered in the loop.  */
  int num_mem_sets;
  /* The insn where the first of these was found.  */
  rtx first_loop_store_insn;
  /* The chain of movable insns in loop.  */
  struct loop_movables movables;
  /* The registers used the in loop.  */
  struct loop_regs regs;
  /* The induction variable information in loop.  */
  struct loop_ivs ivs;
  /* Non-zero if call is in pre_header extended basic block.  */
  int pre_header_has_call;
};


/* Variables declared in loop.c, but also needed in unroll.c.  */

extern int *uid_luid;
extern int max_uid_for_loop;
extern unsigned int max_reg_before_loop;
extern struct loop **uid_loop;
extern FILE *loop_dump_stream;


/* Forward declarations for non-static functions declared in loop.c and
   unroll.c.  */
int loop_invariant_p PARAMS ((const struct loop *, rtx));
rtx get_condition_for_loop PARAMS ((const struct loop *, rtx));
void loop_iv_add_mult_hoist PARAMS ((const struct loop *, rtx, rtx, rtx, rtx));
void loop_iv_add_mult_sink PARAMS ((const struct loop *, rtx, rtx, rtx, rtx));
void loop_iv_add_mult_emit_before PARAMS ((const struct loop *, rtx, 
					   rtx, rtx, rtx,
					   basic_block, rtx));
rtx express_from PARAMS ((struct induction *, struct induction *));
rtx extend_value_for_giv PARAMS ((struct induction *, rtx));

void unroll_loop PARAMS ((struct loop *, int, int));
rtx biv_total_increment PARAMS ((const struct iv_class *));
unsigned HOST_WIDE_INT loop_iterations PARAMS ((struct loop *));
int precondition_loop_p PARAMS ((const struct loop *,
				 rtx *, rtx *, rtx *,
				 enum machine_mode *mode));
rtx final_biv_value PARAMS ((const struct loop *, struct iv_class *));
rtx final_giv_value PARAMS ((const struct loop *, struct induction *));
void emit_unrolled_add PARAMS ((rtx, rtx, rtx));
int back_branch_in_range_p PARAMS ((const struct loop *, rtx));

int loop_insn_first_p PARAMS ((rtx, rtx));
typedef rtx (*loop_insn_callback) PARAMS ((struct loop *, rtx, int, int));
void for_each_insn_in_loop PARAMS ((struct loop *, loop_insn_callback));
rtx loop_insn_emit_before PARAMS((const struct loop *, basic_block, 
				  rtx, rtx));
rtx loop_insn_sink PARAMS((const struct loop *, rtx));
rtx loop_insn_hoist PARAMS((const struct loop *, rtx));

/* Forward declarations for non-static functions declared in doloop.c.  */
int doloop_optimize PARAMS ((const struct loop *));
