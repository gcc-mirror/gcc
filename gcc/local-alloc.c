/* Allocate registers within a basic block, for GNU compiler.
   Copyright (C) 1987, 88, 91, 93, 94, 1995 Free Software Foundation, Inc.

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


/* Allocation of hard register numbers to pseudo registers is done in
   two passes.  In this pass we consider only regs that are born and
   die once within one basic block.  We do this one basic block at a
   time.  Then the next pass allocates the registers that remain.
   Two passes are used because this pass uses methods that work only
   on linear code, but that do a better job than the general methods
   used in global_alloc, and more quickly too.

   The assignments made are recorded in the vector reg_renumber
   whose space is allocated here.  The rtl code itself is not altered.

   We assign each instruction in the basic block a number
   which is its order from the beginning of the block.
   Then we can represent the lifetime of a pseudo register with
   a pair of numbers, and check for conflicts easily.
   We can record the availability of hard registers with a
   HARD_REG_SET for each instruction.  The HARD_REG_SET
   contains 0 or 1 for each hard reg.

   To avoid register shuffling, we tie registers together when one
   dies by being copied into another, or dies in an instruction that
   does arithmetic to produce another.  The tied registers are
   allocated as one.  Registers with different reg class preferences
   can never be tied unless the class preferred by one is a subclass
   of the one preferred by the other.

   Tying is represented with "quantity numbers".
   A non-tied register is given a new quantity number.
   Tied registers have the same quantity number.
   
   We have provision to exempt registers, even when they are contained
   within the block, that can be tied to others that are not contained in it.
   This is so that global_alloc could process them both and tie them then.
   But this is currently disabled since tying in global_alloc is not
   yet implemented.  */

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "flags.h"
#include "basic-block.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"

/* Pseudos allocated here cannot be reallocated by global.c if the hard
   register is used as a spill register.  So we don't allocate such pseudos
   here if their preferred class is likely to be used by spills.

   On most machines, the appropriate test is if the class has one
   register, so we default to that.  */

#ifndef CLASS_LIKELY_SPILLED_P
#define CLASS_LIKELY_SPILLED_P(CLASS) (reg_class_size[(int) (CLASS)] == 1)
#endif

/* Next quantity number available for allocation.  */

static int next_qty;

/* In all the following vectors indexed by quantity number.  */

/* Element Q is the hard reg number chosen for quantity Q,
   or -1 if none was found.  */

static short *qty_phys_reg;

/* We maintain two hard register sets that indicate suggested hard registers
   for each quantity.  The first, qty_phys_copy_sugg, contains hard registers
   that are tied to the quantity by a simple copy.  The second contains all
   hard registers that are tied to the quantity via an arithmetic operation.

   The former register set is given priority for allocation.  This tends to
   eliminate copy insns.  */

/* Element Q is a set of hard registers that are suggested for quantity Q by
   copy insns.  */

static HARD_REG_SET *qty_phys_copy_sugg;

/* Element Q is a set of hard registers that are suggested for quantity Q by
   arithmetic insns.  */

static HARD_REG_SET *qty_phys_sugg;

/* Element Q is the number of suggested registers in qty_phys_copy_sugg.  */

static short *qty_phys_num_copy_sugg;

/* Element Q is the number of suggested registers in qty_phys_sugg. */

static short *qty_phys_num_sugg;

/* Element Q is the number of refs to quantity Q.  */

static int *qty_n_refs;

/* Element Q is a reg class contained in (smaller than) the
   preferred classes of all the pseudo regs that are tied in quantity Q.
   This is the preferred class for allocating that quantity.  */

static enum reg_class *qty_min_class;

/* Insn number (counting from head of basic block)
   where quantity Q was born.  -1 if birth has not been recorded.  */

static int *qty_birth;

/* Insn number (counting from head of basic block)
   where quantity Q died.  Due to the way tying is done,
   and the fact that we consider in this pass only regs that die but once,
   a quantity can die only once.  Each quantity's life span
   is a set of consecutive insns.  -1 if death has not been recorded.  */

static int *qty_death;

/* Number of words needed to hold the data in quantity Q.
   This depends on its machine mode.  It is used for these purposes:
   1. It is used in computing the relative importances of qtys,
      which determines the order in which we look for regs for them.
   2. It is used in rules that prevent tying several registers of
      different sizes in a way that is geometrically impossible
      (see combine_regs).  */

static int *qty_size;

/* This holds the mode of the registers that are tied to qty Q,
   or VOIDmode if registers with differing modes are tied together.  */

static enum machine_mode *qty_mode;

/* Number of times a reg tied to qty Q lives across a CALL_INSN.  */

static int *qty_n_calls_crossed;

/* Register class within which we allocate qty Q if we can't get
   its preferred class.  */

static enum reg_class *qty_alternate_class;

/* Element Q is the SCRATCH expression for which this quantity is being
   allocated or 0 if this quantity is allocating registers.  */

static rtx *qty_scratch_rtx;

/* Element Q is nonzero if this quantity has been used in a SUBREG
   that changes its size.  */

static char *qty_changes_size;

/* Element Q is the register number of one pseudo register whose
   reg_qty value is Q, or -1 is this quantity is for a SCRATCH.  This
   register should be the head of the chain maintained in reg_next_in_qty.  */

static int *qty_first_reg;

/* If (REG N) has been assigned a quantity number, is a register number
   of another register assigned the same quantity number, or -1 for the
   end of the chain.  qty_first_reg point to the head of this chain.  */

static int *reg_next_in_qty;

/* reg_qty[N] (where N is a pseudo reg number) is the qty number of that reg
   if it is >= 0,
   of -1 if this register cannot be allocated by local-alloc,
   or -2 if not known yet.

   Note that if we see a use or death of pseudo register N with
   reg_qty[N] == -2, register N must be local to the current block.  If
   it were used in more than one block, we would have reg_qty[N] == -1.
   This relies on the fact that if reg_basic_block[N] is >= 0, register N
   will not appear in any other block.  We save a considerable number of
   tests by exploiting this.

   If N is < FIRST_PSEUDO_REGISTER, reg_qty[N] is undefined and should not
   be referenced.  */

static int *reg_qty;

/* The offset (in words) of register N within its quantity.
   This can be nonzero if register N is SImode, and has been tied
   to a subreg of a DImode register.  */

static char *reg_offset;

/* Vector of substitutions of register numbers,
   used to map pseudo regs into hardware regs.
   This is set up as a result of register allocation.
   Element N is the hard reg assigned to pseudo reg N,
   or is -1 if no hard reg was assigned.
   If N is a hard reg number, element N is N.  */

short *reg_renumber;

/* Set of hard registers live at the current point in the scan
   of the instructions in a basic block.  */

static HARD_REG_SET regs_live;

/* Each set of hard registers indicates registers live at a particular
   point in the basic block.  For N even, regs_live_at[N] says which
   hard registers are needed *after* insn N/2 (i.e., they may not
   conflict with the outputs of insn N/2 or the inputs of insn N/2 + 1.

   If an object is to conflict with the inputs of insn J but not the
   outputs of insn J + 1, we say it is born at index J*2 - 1.  Similarly,
   if it is to conflict with the outputs of insn J but not the inputs of
   insn J + 1, it is said to die at index J*2 + 1.  */

static HARD_REG_SET *regs_live_at;

int *scratch_block;
rtx *scratch_list;
int scratch_list_length;
static int scratch_index;

/* Communicate local vars `insn_number' and `insn'
   from `block_alloc' to `reg_is_set', `wipe_dead_reg', and `alloc_qty'.  */
static int this_insn_number;
static rtx this_insn;

static void alloc_qty		PROTO((int, enum machine_mode, int, int));
static void alloc_qty_for_scratch PROTO((rtx, int, rtx, int, int));
static void validate_equiv_mem_from_store PROTO((rtx, rtx));
static int validate_equiv_mem	PROTO((rtx, rtx, rtx));
static int memref_referenced_p	PROTO((rtx, rtx));
static int memref_used_between_p PROTO((rtx, rtx, rtx));
static void optimize_reg_copy_1	PROTO((rtx, rtx, rtx));
static void optimize_reg_copy_2	PROTO((rtx, rtx, rtx));
static void update_equiv_regs	PROTO((void));
static void block_alloc		PROTO((int));
static int qty_sugg_compare    	PROTO((int, int));
static int qty_sugg_compare_1	PROTO((int *, int *));
static int qty_compare    	PROTO((int, int));
static int qty_compare_1	PROTO((int *, int *));
static int combine_regs		PROTO((rtx, rtx, int, int, rtx, int));
static int reg_meets_class_p	PROTO((int, enum reg_class));
static int reg_classes_overlap_p PROTO((enum reg_class, enum reg_class,
					int));
static void update_qty_class	PROTO((int, int));
static void reg_is_set		PROTO((rtx, rtx));
static void reg_is_born		PROTO((rtx, int));
static void wipe_dead_reg	PROTO((rtx, int));
static int find_free_reg	PROTO((enum reg_class, enum machine_mode,
				       int, int, int, int, int));
static void mark_life		PROTO((int, enum machine_mode, int));
static void post_mark_life	PROTO((int, enum machine_mode, int, int, int));
static int no_conflict_p	PROTO((rtx, rtx, rtx));
static int requires_inout	PROTO((char *));

/* Allocate a new quantity (new within current basic block)
   for register number REGNO which is born at index BIRTH
   within the block.  MODE and SIZE are info on reg REGNO.  */

static void
alloc_qty (regno, mode, size, birth)
     int regno;
     enum machine_mode mode;
     int size, birth;
{
  register int qty = next_qty++;

  reg_qty[regno] = qty;
  reg_offset[regno] = 0;
  reg_next_in_qty[regno] = -1;

  qty_first_reg[qty] = regno;
  qty_size[qty] = size;
  qty_mode[qty] = mode;
  qty_birth[qty] = birth;
  qty_n_calls_crossed[qty] = reg_n_calls_crossed[regno];
  qty_min_class[qty] = reg_preferred_class (regno);
  qty_alternate_class[qty] = reg_alternate_class (regno);
  qty_n_refs[qty] = reg_n_refs[regno];
  qty_changes_size[qty] = reg_changes_size[regno];
}

/* Similar to `alloc_qty', but allocates a quantity for a SCRATCH rtx
   used as operand N in INSN.  We assume here that the SCRATCH is used in
   a CLOBBER.  */

static void
alloc_qty_for_scratch (scratch, n, insn, insn_code_num, insn_number)
     rtx scratch;
     int n;
     rtx insn;
     int insn_code_num, insn_number;
{
  register int qty;
  enum reg_class class;
  char *p, c;
  int i;

#ifdef REGISTER_CONSTRAINTS
  /* If we haven't yet computed which alternative will be used, do so now.
     Then set P to the constraints for that alternative.  */
  if (which_alternative == -1)
    if (! constrain_operands (insn_code_num, 0))
      return;

  for (p = insn_operand_constraint[insn_code_num][n], i = 0;
       *p && i < which_alternative; p++)
    if (*p == ',')
      i++;

  /* Compute the class required for this SCRATCH.  If we don't need a
     register, the class will remain NO_REGS.  If we guessed the alternative
     number incorrectly, reload will fix things up for us.  */

  class = NO_REGS;
  while ((c = *p++) != '\0' && c != ',')
    switch (c)
      {
      case '=':  case '+':  case '?':
      case '#':  case '&':  case '!':
      case '*':  case '%':  
      case '0':  case '1':  case '2':  case '3':  case '4':
      case 'm':  case '<':  case '>':  case 'V':  case 'o':
      case 'E':  case 'F':  case 'G':  case 'H':
      case 's':  case 'i':  case 'n':
      case 'I':  case 'J':  case 'K':  case 'L':
      case 'M':  case 'N':  case 'O':  case 'P':
#ifdef EXTRA_CONSTRAINT
      case 'Q':  case 'R':  case 'S':  case 'T':  case 'U':
#endif
      case 'p':
	/* These don't say anything we care about.  */
	break;

      case 'X':
	/* We don't need to allocate this SCRATCH.  */
	return;

      case 'g': case 'r':
	class = reg_class_subunion[(int) class][(int) GENERAL_REGS];
	break;

      default:
	class
	  = reg_class_subunion[(int) class][(int) REG_CLASS_FROM_LETTER (c)];
	break;
      }

  if (class == NO_REGS)
    return;

#else /* REGISTER_CONSTRAINTS */

  class = GENERAL_REGS;
#endif
  

  qty = next_qty++;

  qty_first_reg[qty] = -1;
  qty_scratch_rtx[qty] = scratch;
  qty_size[qty] = GET_MODE_SIZE (GET_MODE (scratch));
  qty_mode[qty] = GET_MODE (scratch);
  qty_birth[qty] = 2 * insn_number - 1;
  qty_death[qty] = 2 * insn_number + 1;
  qty_n_calls_crossed[qty] = 0;
  qty_min_class[qty] = class;
  qty_alternate_class[qty] = NO_REGS;
  qty_n_refs[qty] = 1;
  qty_changes_size[qty] = 0;
}

/* Main entry point of this file.  */

void
local_alloc ()
{
  register int b, i;
  int max_qty;

  /* Leaf functions and non-leaf functions have different needs.
     If defined, let the machine say what kind of ordering we
     should use.  */
#ifdef ORDER_REGS_FOR_LOCAL_ALLOC
  ORDER_REGS_FOR_LOCAL_ALLOC;
#endif

  /* Promote REG_EQUAL notes to REG_EQUIV notes and adjust status of affected
     registers.  */
  update_equiv_regs ();

  /* This sets the maximum number of quantities we can have.  Quantity
     numbers start at zero and we can have one for each pseudo plus the
     number of SCRATCHes in the largest block, in the worst case.  */
  max_qty = (max_regno - FIRST_PSEUDO_REGISTER) + max_scratch;

  /* Allocate vectors of temporary data.
     See the declarations of these variables, above,
     for what they mean.  */

  /* There can be up to MAX_SCRATCH * N_BASIC_BLOCKS SCRATCHes to allocate.
     Instead of allocating this much memory from now until the end of
     reload, only allocate space for MAX_QTY SCRATCHes.  If there are more
     reload will allocate them.  */

  scratch_list_length = max_qty;
  scratch_list = (rtx *) xmalloc (scratch_list_length * sizeof (rtx));
  bzero ((char *) scratch_list, scratch_list_length * sizeof (rtx));
  scratch_block = (int *) xmalloc (scratch_list_length * sizeof (int));
  bzero ((char *) scratch_block, scratch_list_length * sizeof (int));
  scratch_index = 0;

  qty_phys_reg = (short *) alloca (max_qty * sizeof (short));
  qty_phys_copy_sugg
    = (HARD_REG_SET *) alloca (max_qty * sizeof (HARD_REG_SET));
  qty_phys_num_copy_sugg = (short *) alloca (max_qty * sizeof (short));
  qty_phys_sugg = (HARD_REG_SET *) alloca (max_qty * sizeof (HARD_REG_SET));
  qty_phys_num_sugg = (short *) alloca (max_qty * sizeof (short));
  qty_birth = (int *) alloca (max_qty * sizeof (int));
  qty_death = (int *) alloca (max_qty * sizeof (int));
  qty_scratch_rtx = (rtx *) alloca (max_qty * sizeof (rtx));
  qty_first_reg = (int *) alloca (max_qty * sizeof (int));
  qty_size = (int *) alloca (max_qty * sizeof (int));
  qty_mode
    = (enum machine_mode *) alloca (max_qty * sizeof (enum machine_mode));
  qty_n_calls_crossed = (int *) alloca (max_qty * sizeof (int));
  qty_min_class
    = (enum reg_class *) alloca (max_qty * sizeof (enum reg_class));
  qty_alternate_class
    = (enum reg_class *) alloca (max_qty * sizeof (enum reg_class));
  qty_n_refs = (int *) alloca (max_qty * sizeof (int));
  qty_changes_size = (char *) alloca (max_qty * sizeof (char));

  reg_qty = (int *) alloca (max_regno * sizeof (int));
  reg_offset = (char *) alloca (max_regno * sizeof (char));
  reg_next_in_qty = (int *) alloca (max_regno * sizeof (int));

  reg_renumber = (short *) oballoc (max_regno * sizeof (short));
  for (i = 0; i < max_regno; i++)
    reg_renumber[i] = -1;

  /* Determine which pseudo-registers can be allocated by local-alloc.
     In general, these are the registers used only in a single block and
     which only die once.  However, if a register's preferred class has only
     a few entries, don't allocate this register here unless it is preferred
     or nothing since retry_global_alloc won't be able to move it to
     GENERAL_REGS if a reload register of this class is needed.

     We need not be concerned with which block actually uses the register
     since we will never see it outside that block.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    {
      if (reg_basic_block[i] >= 0 && reg_n_deaths[i] == 1
	  && (reg_alternate_class (i) == NO_REGS
	      || ! CLASS_LIKELY_SPILLED_P (reg_preferred_class (i))))
	reg_qty[i] = -2;
      else
	reg_qty[i] = -1;
    }

  /* Force loop below to initialize entire quantity array.  */
  next_qty = max_qty;

  /* Allocate each block's local registers, block by block.  */

  for (b = 0; b < n_basic_blocks; b++)
    {
      /* NEXT_QTY indicates which elements of the `qty_...'
	 vectors might need to be initialized because they were used
	 for the previous block; it is set to the entire array before
	 block 0.  Initialize those, with explicit loop if there are few,
	 else with bzero and bcopy.  Do not initialize vectors that are
	 explicit set by `alloc_qty'.  */

      if (next_qty < 6)
	{
	  for (i = 0; i < next_qty; i++)
	    {
	      qty_scratch_rtx[i] = 0;
	      CLEAR_HARD_REG_SET (qty_phys_copy_sugg[i]);
	      qty_phys_num_copy_sugg[i] = 0;
	      CLEAR_HARD_REG_SET (qty_phys_sugg[i]);
	      qty_phys_num_sugg[i] = 0;
	    }
	}
      else
	{
#define CLEAR(vector)  \
	  bzero ((char *) (vector), (sizeof (*(vector))) * next_qty);

	  CLEAR (qty_scratch_rtx);
	  CLEAR (qty_phys_copy_sugg);
	  CLEAR (qty_phys_num_copy_sugg);
	  CLEAR (qty_phys_sugg);
	  CLEAR (qty_phys_num_sugg);
	}

      next_qty = 0;

      block_alloc (b);
#ifdef USE_C_ALLOCA
      alloca (0);
#endif
    }
}

/* Depth of loops we are in while in update_equiv_regs.  */
static int loop_depth;

/* Used for communication between the following two functions: contains
   a MEM that we wish to ensure remains unchanged.  */
static rtx equiv_mem;

/* Set nonzero if EQUIV_MEM is modified.  */
static int equiv_mem_modified;

/* If EQUIV_MEM is modified by modifying DEST, indicate that it is modified.
   Called via note_stores.  */

static void
validate_equiv_mem_from_store (dest, set)
     rtx dest;
     rtx set;
{
  if ((GET_CODE (dest) == REG
       && reg_overlap_mentioned_p (dest, equiv_mem))
      || (GET_CODE (dest) == MEM
	  && true_dependence (dest, equiv_mem)))
    equiv_mem_modified = 1;
}

/* Verify that no store between START and the death of REG invalidates
   MEMREF.  MEMREF is invalidated by modifying a register used in MEMREF,
   by storing into an overlapping memory location, or with a non-const
   CALL_INSN.

   Return 1 if MEMREF remains valid.  */

static int
validate_equiv_mem (start, reg, memref)
     rtx start;
     rtx reg;
     rtx memref;
{
  rtx insn;
  rtx note;

  equiv_mem = memref;
  equiv_mem_modified = 0;

  /* If the memory reference has side effects or is volatile, it isn't a
     valid equivalence.  */
  if (side_effects_p (memref))
    return 0;

  for (insn = start; insn && ! equiv_mem_modified; insn = NEXT_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;

      if (find_reg_note (insn, REG_DEAD, reg))
	return 1;

      if (GET_CODE (insn) == CALL_INSN && ! RTX_UNCHANGING_P (memref)
	  && ! CONST_CALL_P (insn))
	return 0;

      note_stores (PATTERN (insn), validate_equiv_mem_from_store);

      /* If a register mentioned in MEMREF is modified via an
	 auto-increment, we lose the equivalence.  Do the same if one
	 dies; although we could extend the life, it doesn't seem worth
	 the trouble.  */

      for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	if ((REG_NOTE_KIND (note) == REG_INC
	     || REG_NOTE_KIND (note) == REG_DEAD)
	    && GET_CODE (XEXP (note, 0)) == REG
	    && reg_overlap_mentioned_p (XEXP (note, 0), memref))
	  return 0;
    }

  return 0;
}

/* TRUE if X references a memory location that would be affected by a store
   to MEMREF.  */

static int
memref_referenced_p (memref, x)
     rtx x;
     rtx memref;
{
  int i, j;
  char *fmt;
  enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case REG:
    case CONST_INT:
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
    case PC:
    case CC0:
    case HIGH:
    case LO_SUM:
      return 0;

    case MEM:
      if (true_dependence (memref, x))
	return 1;
      break;

    case SET:
      /* If we are setting a MEM, it doesn't count (its address does), but any
	 other SET_DEST that has a MEM in it is referencing the MEM.  */
      if (GET_CODE (SET_DEST (x)) == MEM)
	{
	  if (memref_referenced_p (memref, XEXP (SET_DEST (x), 0)))
	    return 1;
	}
      else if (memref_referenced_p (memref, SET_DEST (x)))
	return 1;

      return memref_referenced_p (memref, SET_SRC (x));
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    switch (fmt[i])
      {
      case 'e':
	if (memref_referenced_p (memref, XEXP (x, i)))
	  return 1;
	break;
      case 'E':
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (memref_referenced_p (memref, XVECEXP (x, i, j)))
	    return 1;
	break;
      }

  return 0;
}

/* TRUE if some insn in the range (START, END] references a memory location
   that would be affected by a store to MEMREF.  */

static int
memref_used_between_p (memref, start, end)
     rtx memref;
     rtx start;
     rtx end;
{
  rtx insn;

  for (insn = NEXT_INSN (start); insn != NEXT_INSN (end);
       insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	&& memref_referenced_p (memref, PATTERN (insn)))
      return 1;

  return 0;
}

/* INSN is a copy from SRC to DEST, both registers, and SRC does not die
   in INSN.

   Search forward to see if SRC dies before either it or DEST is modified,
   but don't scan past the end of a basic block.  If so, we can replace SRC
   with DEST and let SRC die in INSN. 

   This will reduce the number of registers live in that range and may enable
   DEST to be tied to SRC, thus often saving one register in addition to a
   register-register copy.  */

static void
optimize_reg_copy_1 (insn, dest, src)
     rtx insn;
     rtx dest;
     rtx src;
{
  rtx p, q;
  rtx note;
  rtx dest_death = 0;
  int sregno = REGNO (src);
  int dregno = REGNO (dest);

  if (sregno == dregno
#ifdef SMALL_REGISTER_CLASSES
      /* We don't want to mess with hard regs if register classes are small. */
      || sregno < FIRST_PSEUDO_REGISTER || dregno < FIRST_PSEUDO_REGISTER
#endif
      /* We don't see all updates to SP if they are in an auto-inc memory
	 reference, so we must disallow this optimization on them.  */
      || sregno == STACK_POINTER_REGNUM || dregno == STACK_POINTER_REGNUM)
    return;

  for (p = NEXT_INSN (insn); p; p = NEXT_INSN (p))
    {
      if (GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN
	  || (GET_CODE (p) == NOTE
	      && (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG
		  || NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_END)))
	break;

      if (GET_RTX_CLASS (GET_CODE (p)) != 'i')
	continue;

      if (reg_set_p (src, p) || reg_set_p (dest, p)
	  /* Don't change a USE of a register.  */
	  || (GET_CODE (PATTERN (p)) == USE
	      && reg_overlap_mentioned_p (src, XEXP (PATTERN (p), 0))))
	break;

      /* See if all of SRC dies in P.  This test is slightly more
	 conservative than it needs to be. */
      if ((note = find_regno_note (p, REG_DEAD, sregno)) != 0
	  && GET_MODE (XEXP (note, 0)) == GET_MODE (src))
	{
	  int failed = 0;
	  int length = 0;
	  int d_length = 0;
	  int n_calls = 0;
	  int d_n_calls = 0;

	  /* We can do the optimization.  Scan forward from INSN again,
	     replacing regs as we go.  Set FAILED if a replacement can't
	     be done.  In that case, we can't move the death note for SRC.
	     This should be rare.  */

	  /* Set to stop at next insn.  */
	  for (q = next_real_insn (insn);
	       q != next_real_insn (p);
	       q = next_real_insn (q))
	    {
	      if (reg_overlap_mentioned_p (src, PATTERN (q)))
		{
		  /* If SRC is a hard register, we might miss some
		     overlapping registers with validate_replace_rtx,
		     so we would have to undo it.  We can't if DEST is
		     present in the insn, so fail in that combination
		     of cases.  */
		  if (sregno < FIRST_PSEUDO_REGISTER
		      && reg_mentioned_p (dest, PATTERN (q)))
		    failed = 1;

		  /* Replace all uses and make sure that the register
		     isn't still present.  */
		  else if (validate_replace_rtx (src, dest, q)
			   && (sregno >= FIRST_PSEUDO_REGISTER
			       || ! reg_overlap_mentioned_p (src,
							     PATTERN (q))))
		    {
		      /* We assume that a register is used exactly once per
			 insn in the updates below.  If this is not correct,
			 no great harm is done.  */
		      if (sregno >= FIRST_PSEUDO_REGISTER)
			reg_n_refs[sregno] -= loop_depth;
		      if (dregno >= FIRST_PSEUDO_REGISTER)
			reg_n_refs[dregno] += loop_depth;
		    }
		  else
		    {
		      validate_replace_rtx (dest, src, q);
		      failed = 1;
		    }
		}

	      /* Count the insns and CALL_INSNs passed.  If we passed the
		 death note of DEST, show increased live length.  */
	      length++;
	      if (dest_death)
		d_length++;

	      /* If the insn in which SRC dies is a CALL_INSN, don't count it
		 as a call that has been crossed.  Otherwise, count it.  */
	      if (q != p && GET_CODE (q) == CALL_INSN)
		{
		  n_calls++;
		  if (dest_death)
		    d_n_calls++;
		}

	      /* If DEST dies here, remove the death note and save it for
		 later.  Make sure ALL of DEST dies here; again, this is
		 overly conservative.  */
	      if (dest_death == 0
		  && (dest_death = find_regno_note (q, REG_DEAD, dregno)) != 0
		  && GET_MODE (XEXP (dest_death, 0)) == GET_MODE (dest))
		remove_note (q, dest_death);
	    }

	  if (! failed)
	    {
	      if (sregno >= FIRST_PSEUDO_REGISTER)
		{
		  reg_live_length[sregno] -= length;
		  /* reg_live_length is only an approximation after combine
		     if sched is not run, so make sure that we still have
		     a reasonable value.  */
		  if (reg_live_length[sregno] < 2)
		    reg_live_length[sregno] = 2;
		  reg_n_calls_crossed[sregno] -= n_calls;
		}

	      if (dregno >= FIRST_PSEUDO_REGISTER)
		{
		  reg_live_length[dregno] += d_length;
		  reg_n_calls_crossed[dregno] += d_n_calls;
		}

	      /* Move death note of SRC from P to INSN.  */
	      remove_note (p, note);
	      XEXP (note, 1) = REG_NOTES (insn);
	      REG_NOTES (insn) = note;
	    }

	  /* Put death note of DEST on P if we saw it die.  */
	  if (dest_death)
	    {
	      XEXP (dest_death, 1) = REG_NOTES (p);
	      REG_NOTES (p) = dest_death;
	    }

	  return;
	}

      /* If SRC is a hard register which is set or killed in some other
	 way, we can't do this optimization.  */
      else if (sregno < FIRST_PSEUDO_REGISTER
	       && dead_or_set_p (p, src))
	break;
    }
}

/* INSN is a copy of SRC to DEST, in which SRC dies.  See if we now have
   a sequence of insns that modify DEST followed by an insn that sets
   SRC to DEST in which DEST dies, with no prior modification of DEST.
   (There is no need to check if the insns in between actually modify
   DEST.  We should not have cases where DEST is not modified, but
   the optimization is safe if no such modification is detected.)
   In that case, we can replace all uses of DEST, starting with INSN and
   ending with the set of SRC to DEST, with SRC.  We do not do this
   optimization if a CALL_INSN is crossed unless SRC already crosses a
   call.

   It is assumed that DEST and SRC are pseudos; it is too complicated to do
   this for hard registers since the substitutions we may make might fail.  */

static void
optimize_reg_copy_2 (insn, dest, src)
     rtx insn;
     rtx dest;
     rtx src;
{
  rtx p, q;
  rtx set;
  int sregno = REGNO (src);
  int dregno = REGNO (dest);

  for (p = NEXT_INSN (insn); p; p = NEXT_INSN (p))
    {
      if (GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN
	  || (GET_CODE (p) == NOTE
	      && (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG
		  || NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_END)))
	break;

      if (GET_RTX_CLASS (GET_CODE (p)) != 'i')
	continue;

      set = single_set (p);
      if (set && SET_SRC (set) == dest && SET_DEST (set) == src
	  && find_reg_note (p, REG_DEAD, dest))
	{
	  /* We can do the optimization.  Scan forward from INSN again,
	     replacing regs as we go.  */

	  /* Set to stop at next insn.  */
	  for (q = insn; q != NEXT_INSN (p); q = NEXT_INSN (q))
	    if (GET_RTX_CLASS (GET_CODE (q)) == 'i')
	      {
		if (reg_mentioned_p (dest, PATTERN (q)))
		  {
		    PATTERN (q) = replace_rtx (PATTERN (q), dest, src);

		    /* We assume that a register is used exactly once per
		       insn in the updates below.  If this is not correct,
		       no great harm is done.  */
		    reg_n_refs[dregno] -= loop_depth;
		    reg_n_refs[sregno] += loop_depth;
		  }


	      if (GET_CODE (q) == CALL_INSN)
		{
		  reg_n_calls_crossed[dregno]--;
		  reg_n_calls_crossed[sregno]++;
		}
	      }

	  remove_note (p, find_reg_note (p, REG_DEAD, dest));
	  reg_n_deaths[dregno]--;
	  remove_note (insn, find_reg_note (insn, REG_DEAD, src));
	  reg_n_deaths[sregno]--;
	  return;
	}

      if (reg_set_p (src, p)
	  || (GET_CODE (p) == CALL_INSN && reg_n_calls_crossed[sregno] == 0))
	break;
    }
}
	      
/* Find registers that are equivalent to a single value throughout the
   compilation (either because they can be referenced in memory or are set once
   from a single constant).  Lower their priority for a register.

   If such a register is only referenced once, try substituting its value
   into the using insn.  If it succeeds, we can eliminate the register
   completely.  */

static void
update_equiv_regs ()
{
  rtx *reg_equiv_init_insn = (rtx *) alloca (max_regno * sizeof (rtx *));
  rtx *reg_equiv_replacement = (rtx *) alloca (max_regno * sizeof (rtx *));
  rtx insn;

  bzero ((char *) reg_equiv_init_insn, max_regno * sizeof (rtx *));
  bzero ((char *) reg_equiv_replacement, max_regno * sizeof (rtx *));

  init_alias_analysis ();

  loop_depth = 1;

  /* Scan the insns and find which registers have equivalences.  Do this
     in a separate scan of the insns because (due to -fcse-follow-jumps)
     a register can be set below its use.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx note;
      rtx set = single_set (insn);
      rtx dest;
      int regno;

      if (GET_CODE (insn) == NOTE)
	{
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	    loop_depth++;
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	    loop_depth--;
	}

      /* If this insn contains more (or less) than a single SET, ignore it.  */
      if (set == 0)
	continue;

      dest = SET_DEST (set);

      /* If this sets a MEM to the contents of a REG that is only used
	 in a single basic block, see if the register is always equivalent
	 to that memory location and if moving the store from INSN to the
	 insn that set REG is safe.  If so, put a REG_EQUIV note on the
	 initializing insn.  */

      if (GET_CODE (dest) == MEM && GET_CODE (SET_SRC (set)) == REG
	  && (regno = REGNO (SET_SRC (set))) >= FIRST_PSEUDO_REGISTER
	  && reg_basic_block[regno] >= 0
	  && reg_equiv_init_insn[regno] != 0
	  && validate_equiv_mem (reg_equiv_init_insn[regno], SET_SRC (set),
				 dest)
	  && ! memref_used_between_p (SET_DEST (set),
				      reg_equiv_init_insn[regno], insn))
	REG_NOTES (reg_equiv_init_insn[regno])
	  = gen_rtx (EXPR_LIST, REG_EQUIV, dest,
		     REG_NOTES (reg_equiv_init_insn[regno]));

      /* If this is a register-register copy where SRC is not dead, see if we
	 can optimize it.  */
      if (flag_expensive_optimizations && GET_CODE (dest) == REG
	  && GET_CODE (SET_SRC (set)) == REG
	  && ! find_reg_note (insn, REG_DEAD, SET_SRC (set)))
	optimize_reg_copy_1 (insn, dest, SET_SRC (set));

      /* Similarly for a pseudo-pseudo copy when SRC is dead.  */
      else if (flag_expensive_optimizations && GET_CODE (dest) == REG
	       && REGNO (dest) >= FIRST_PSEUDO_REGISTER
	       && GET_CODE (SET_SRC (set)) == REG
	       && REGNO (SET_SRC (set)) >= FIRST_PSEUDO_REGISTER
	       && find_reg_note (insn, REG_DEAD, SET_SRC (set)))
	optimize_reg_copy_2 (insn, dest, SET_SRC (set));

      /* Otherwise, we only handle the case of a pseudo register being set
	 once.  */
      if (GET_CODE (dest) != REG
	  || (regno = REGNO (dest)) < FIRST_PSEUDO_REGISTER
	  || reg_n_sets[regno] != 1)
	continue;

      note = find_reg_note (insn, REG_EQUAL, NULL_RTX);

      /* Record this insn as initializing this register.  */
      reg_equiv_init_insn[regno] = insn;

      /* If this register is known to be equal to a constant, record that
	 it is always equivalent to the constant.  */
      if (note && CONSTANT_P (XEXP (note, 0)))
	PUT_MODE (note, (enum machine_mode) REG_EQUIV);

      /* If this insn introduces a "constant" register, decrease the priority
	 of that register.  Record this insn if the register is only used once
	 more and the equivalence value is the same as our source.

	 The latter condition is checked for two reasons:  First, it is an
	 indication that it may be more efficient to actually emit the insn
	 as written (if no registers are available, reload will substitute
	 the equivalence).  Secondly, it avoids problems with any registers
	 dying in this insn whose death notes would be missed.

	 If we don't have a REG_EQUIV note, see if this insn is loading
	 a register used only in one basic block from a MEM.  If so, and the
	 MEM remains unchanged for the life of the register, add a REG_EQUIV
	 note.  */
	 
      note = find_reg_note (insn, REG_EQUIV, NULL_RTX);

      if (note == 0 && reg_basic_block[regno] >= 0
	  && GET_CODE (SET_SRC (set)) == MEM
	  && validate_equiv_mem (insn, dest, SET_SRC (set)))
	REG_NOTES (insn) = note = gen_rtx (EXPR_LIST, REG_EQUIV, SET_SRC (set),
					   REG_NOTES (insn));

      /* Don't mess with things live during setjmp.  */
      if (note && reg_live_length[regno] >= 0)
	{
	  int regno = REGNO (dest);

	  /* Note that the statement below does not affect the priority
	     in local-alloc!  */
	  reg_live_length[regno] *= 2;

	  /* If the register is referenced exactly twice, meaning it is set
	     once and used once, indicate that the reference may be replaced
	     by the equivalence we computed above.  If the register is only
	     used in one basic block, this can't succeed or combine would
	     have done it.

	     It would be nice to use "loop_depth * 2" in the compare
	     below.  Unfortunately, LOOP_DEPTH need not be constant within
	     a basic block so this would be too complicated.

	     This case normally occurs when a parameter is read from memory
	     and then used exactly once, not in a loop.  */

	  if (reg_n_refs[regno] == 2
	      && reg_basic_block[regno] < 0
	      && rtx_equal_p (XEXP (note, 0), SET_SRC (set)))
	    reg_equiv_replacement[regno] = SET_SRC (set);
	}
    }

  /* Now scan all regs killed in an insn to see if any of them are registers
     only used that once.  If so, see if we can replace the reference with
     the equivalent from.  If we can, delete the initializing reference
     and this register will go away.  */
  for (insn = next_active_insn (get_insns ());
       insn;
       insn = next_active_insn (insn))
    {
      rtx link;

      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	if (REG_NOTE_KIND (link) == REG_DEAD
	    /* Make sure this insn still refers to the register.  */
	    && reg_mentioned_p (XEXP (link, 0), PATTERN (insn)))
	  {
	    int regno = REGNO (XEXP (link, 0));

	    if (reg_equiv_replacement[regno]
		&& validate_replace_rtx (regno_reg_rtx[regno],
					 reg_equiv_replacement[regno], insn))
	      {
		rtx equiv_insn = reg_equiv_init_insn[regno];

		remove_death (regno, insn);
		reg_n_refs[regno] = 0;
		PUT_CODE (equiv_insn, NOTE);
		NOTE_LINE_NUMBER (equiv_insn) = NOTE_INSN_DELETED;
		NOTE_SOURCE_FILE (equiv_insn) = 0;
	      }
	  }
    }
}

/* Allocate hard regs to the pseudo regs used only within block number B.
   Only the pseudos that die but once can be handled.  */

static void
block_alloc (b)
     int b;
{
  register int i, q;
  register rtx insn;
  rtx note;
  int insn_number = 0;
  int insn_count = 0;
  int max_uid = get_max_uid ();
  int *qty_order;
  int no_conflict_combined_regno = -1;
  /* Counter to prevent allocating more SCRATCHes than can be stored
     in SCRATCH_LIST.  */
  int scratches_allocated = scratch_index;

  /* Count the instructions in the basic block.  */

  insn = basic_block_end[b];
  while (1)
    {
      if (GET_CODE (insn) != NOTE)
	if (++insn_count > max_uid)
	  abort ();
      if (insn == basic_block_head[b])
	break;
      insn = PREV_INSN (insn);
    }

  /* +2 to leave room for a post_mark_life at the last insn and for
     the birth of a CLOBBER in the first insn.  */
  regs_live_at = (HARD_REG_SET *) alloca ((2 * insn_count + 2)
					  * sizeof (HARD_REG_SET));
  bzero ((char *) regs_live_at, (2 * insn_count + 2) * sizeof (HARD_REG_SET));

  /* Initialize table of hardware registers currently live.  */

#ifdef HARD_REG_SET
  regs_live = *basic_block_live_at_start[b];
#else
  COPY_HARD_REG_SET (regs_live, basic_block_live_at_start[b]);
#endif

  /* This loop scans the instructions of the basic block
     and assigns quantities to registers.
     It computes which registers to tie.  */

  insn = basic_block_head[b];
  while (1)
    {
      register rtx body = PATTERN (insn);

      if (GET_CODE (insn) != NOTE)
	insn_number++;

      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  register rtx link, set;
	  register int win = 0;
	  register rtx r0, r1;
	  int combined_regno = -1;
	  int i;
	  int insn_code_number = recog_memoized (insn);

	  this_insn_number = insn_number;
	  this_insn = insn;

	  if (insn_code_number >= 0)
	    insn_extract (insn);
	  which_alternative = -1;

	  /* Is this insn suitable for tying two registers?
	     If so, try doing that.
	     Suitable insns are those with at least two operands and where
	     operand 0 is an output that is a register that is not
	     earlyclobber.

	     We can tie operand 0 with some operand that dies in this insn.
	     First look for operands that are required to be in the same
	     register as operand 0.  If we find such, only try tying that
	     operand or one that can be put into that operand if the
	     operation is commutative.  If we don't find an operand
	     that is required to be in the same register as operand 0,
	     we can tie with any operand.

	     Subregs in place of regs are also ok.

	     If tying is done, WIN is set nonzero.  */

	  if (insn_code_number >= 0
#ifdef REGISTER_CONSTRAINTS
	      && insn_n_operands[insn_code_number] > 1
	      && insn_operand_constraint[insn_code_number][0][0] == '='
	      && insn_operand_constraint[insn_code_number][0][1] != '&'
#else
	      && GET_CODE (PATTERN (insn)) == SET
	      && rtx_equal_p (SET_DEST (PATTERN (insn)), recog_operand[0])
#endif
	      )
	    {
#ifdef REGISTER_CONSTRAINTS
	      /* If non-negative, is an operand that must match operand 0.  */
	      int must_match_0 = -1;
	      /* Counts number of alternatives that require a match with
		 operand 0.  */
	      int n_matching_alts = 0;

	      for (i = 1; i < insn_n_operands[insn_code_number]; i++)
		{
		  char *p = insn_operand_constraint[insn_code_number][i];
		  int this_match = (requires_inout (p));

		  n_matching_alts += this_match;
		  if (this_match == insn_n_alternatives[insn_code_number])
		    must_match_0 = i;
		}
#endif

	      r0 = recog_operand[0];
	      for (i = 1; i < insn_n_operands[insn_code_number]; i++)
		{
#ifdef REGISTER_CONSTRAINTS
		  /* Skip this operand if we found an operand that
		     must match operand 0 and this operand isn't it
		     and can't be made to be it by commutativity.  */

		  if (must_match_0 >= 0 && i != must_match_0
		      && ! (i == must_match_0 + 1
			    && insn_operand_constraint[insn_code_number][i-1][0] == '%')
		      && ! (i == must_match_0 - 1
			    && insn_operand_constraint[insn_code_number][i][0] == '%'))
		    continue;

		  /* Likewise if each alternative has some operand that
		     must match operand zero.  In that case, skip any 
		     operand that doesn't list operand 0 since we know that
		     the operand always conflicts with operand 0.  We
		     ignore commutatity in this case to keep things simple.  */
		  if (n_matching_alts == insn_n_alternatives[insn_code_number]
		      && (0 == requires_inout
			  (insn_operand_constraint[insn_code_number][i])))
		    continue;
#endif

		  r1 = recog_operand[i];

		  /* If the operand is an address, find a register in it.
		     There may be more than one register, but we only try one
		     of them.  */
		  if (
#ifdef REGISTER_CONSTRAINTS
		      insn_operand_constraint[insn_code_number][i][0] == 'p'
#else
		      insn_operand_address_p[insn_code_number][i]
#endif
		      )
		    while (GET_CODE (r1) == PLUS || GET_CODE (r1) == MULT)
		      r1 = XEXP (r1, 0);

		  if (GET_CODE (r0) == REG || GET_CODE (r0) == SUBREG)
		    {
		      /* We have two priorities for hard register preferences.
			 If we have a move insn or an insn whose first input
			 can only be in the same register as the output, give
			 priority to an equivalence found from that insn.  */
		      int may_save_copy
			= ((SET_DEST (body) == r0 && SET_SRC (body) == r1)
#ifdef REGISTER_CONSTRAINTS
			   || (r1 == recog_operand[i] && must_match_0 >= 0)
#endif
			   );
		      
		      if (GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG)
			win = combine_regs (r1, r0, may_save_copy,
					    insn_number, insn, 0);
		    }
		  if (win)
		    break;
		}
	    }

	  /* Recognize an insn sequence with an ultimate result
	     which can safely overlap one of the inputs.
	     The sequence begins with a CLOBBER of its result,
	     and ends with an insn that copies the result to itself
	     and has a REG_EQUAL note for an equivalent formula.
	     That note indicates what the inputs are.
	     The result and the input can overlap if each insn in
	     the sequence either doesn't mention the input
	     or has a REG_NO_CONFLICT note to inhibit the conflict.

	     We do the combining test at the CLOBBER so that the
	     destination register won't have had a quantity number
	     assigned, since that would prevent combining.  */

	  if (GET_CODE (PATTERN (insn)) == CLOBBER
	      && (r0 = XEXP (PATTERN (insn), 0),
		  GET_CODE (r0) == REG)
	      && (link = find_reg_note (insn, REG_LIBCALL, NULL_RTX)) != 0
	      && XEXP (link, 0) != 0
	      && GET_CODE (XEXP (link, 0)) == INSN
	      && (set = single_set (XEXP (link, 0))) != 0
	      && SET_DEST (set) == r0 && SET_SRC (set) == r0
	      && (note = find_reg_note (XEXP (link, 0), REG_EQUAL,
					NULL_RTX)) != 0)
	    {
	      if (r1 = XEXP (note, 0), GET_CODE (r1) == REG
		  /* Check that we have such a sequence.  */
		  && no_conflict_p (insn, r0, r1))
		win = combine_regs (r1, r0, 1, insn_number, insn, 1);
	      else if (GET_RTX_FORMAT (GET_CODE (XEXP (note, 0)))[0] == 'e'
		       && (r1 = XEXP (XEXP (note, 0), 0),
			   GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG)
		       && no_conflict_p (insn, r0, r1))
		win = combine_regs (r1, r0, 0, insn_number, insn, 1);

	      /* Here we care if the operation to be computed is
		 commutative.  */
	      else if ((GET_CODE (XEXP (note, 0)) == EQ
			|| GET_CODE (XEXP (note, 0)) == NE
			|| GET_RTX_CLASS (GET_CODE (XEXP (note, 0))) == 'c')
		       && (r1 = XEXP (XEXP (note, 0), 1),
			   (GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG))
		       && no_conflict_p (insn, r0, r1))
		win = combine_regs (r1, r0, 0, insn_number, insn, 1);

	      /* If we did combine something, show the register number
		 in question so that we know to ignore its death.  */
	      if (win)
		no_conflict_combined_regno = REGNO (r1);
	    }

	  /* If registers were just tied, set COMBINED_REGNO
	     to the number of the register used in this insn
	     that was tied to the register set in this insn.
	     This register's qty should not be "killed".  */

	  if (win)
	    {
	      while (GET_CODE (r1) == SUBREG)
		r1 = SUBREG_REG (r1);
	      combined_regno = REGNO (r1);
	    }

	  /* Mark the death of everything that dies in this instruction,
	     except for anything that was just combined.  */

	  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	    if (REG_NOTE_KIND (link) == REG_DEAD
		&& GET_CODE (XEXP (link, 0)) == REG
		&& combined_regno != REGNO (XEXP (link, 0))
		&& (no_conflict_combined_regno != REGNO (XEXP (link, 0))
		    || ! find_reg_note (insn, REG_NO_CONFLICT, XEXP (link, 0))))
	      wipe_dead_reg (XEXP (link, 0), 0);

	  /* Allocate qty numbers for all registers local to this block
	     that are born (set) in this instruction.
	     A pseudo that already has a qty is not changed.  */

	  note_stores (PATTERN (insn), reg_is_set);

	  /* If anything is set in this insn and then unused, mark it as dying
	     after this insn, so it will conflict with our outputs.  This
	     can't match with something that combined, and it doesn't matter
	     if it did.  Do this after the calls to reg_is_set since these
	     die after, not during, the current insn.  */

	  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	    if (REG_NOTE_KIND (link) == REG_UNUSED
		&& GET_CODE (XEXP (link, 0)) == REG)
	      wipe_dead_reg (XEXP (link, 0), 1);

	  /* Allocate quantities for any SCRATCH operands of this insn.  */

	  if (insn_code_number >= 0)
	    for (i = 0; i < insn_n_operands[insn_code_number]; i++)
	      if (GET_CODE (recog_operand[i]) == SCRATCH
		  && scratches_allocated++ < scratch_list_length)
		alloc_qty_for_scratch (recog_operand[i], i, insn,
				       insn_code_number, insn_number);

	  /* If this is an insn that has a REG_RETVAL note pointing at a 
	     CLOBBER insn, we have reached the end of a REG_NO_CONFLICT
	     block, so clear any register number that combined within it.  */
	  if ((note = find_reg_note (insn, REG_RETVAL, NULL_RTX)) != 0
	      && GET_CODE (XEXP (note, 0)) == INSN
	      && GET_CODE (PATTERN (XEXP (note, 0))) == CLOBBER)
	    no_conflict_combined_regno = -1;
	}

      /* Set the registers live after INSN_NUMBER.  Note that we never
	 record the registers live before the block's first insn, since no
	 pseudos we care about are live before that insn.  */

      IOR_HARD_REG_SET (regs_live_at[2 * insn_number], regs_live);
      IOR_HARD_REG_SET (regs_live_at[2 * insn_number + 1], regs_live);

      if (insn == basic_block_end[b])
	break;

      insn = NEXT_INSN (insn);
    }

  /* Now every register that is local to this basic block
     should have been given a quantity, or else -1 meaning ignore it.
     Every quantity should have a known birth and death.  

     Order the qtys so we assign them registers in order of the
     number of suggested registers they need so we allocate those with
     the most restrictive needs first.  */

  qty_order = (int *) alloca (next_qty * sizeof (int));
  for (i = 0; i < next_qty; i++)
    qty_order[i] = i;

#define EXCHANGE(I1, I2)  \
  { i = qty_order[I1]; qty_order[I1] = qty_order[I2]; qty_order[I2] = i; }

  switch (next_qty)
    {
    case 3:
      /* Make qty_order[2] be the one to allocate last.  */
      if (qty_sugg_compare (0, 1) > 0)
	EXCHANGE (0, 1);
      if (qty_sugg_compare (1, 2) > 0)
	EXCHANGE (2, 1);

      /* ... Fall through ... */
    case 2:
      /* Put the best one to allocate in qty_order[0].  */
      if (qty_sugg_compare (0, 1) > 0)
	EXCHANGE (0, 1);

      /* ... Fall through ... */

    case 1:
    case 0:
      /* Nothing to do here.  */
      break;

    default:
      qsort (qty_order, next_qty, sizeof (int), qty_sugg_compare_1);
    }

  /* Try to put each quantity in a suggested physical register, if it has one.
     This may cause registers to be allocated that otherwise wouldn't be, but
     this seems acceptable in local allocation (unlike global allocation).  */
  for (i = 0; i < next_qty; i++)
    {
      q = qty_order[i];
      if (qty_phys_num_sugg[q] != 0 || qty_phys_num_copy_sugg[q] != 0)
	qty_phys_reg[q] = find_free_reg (qty_min_class[q], qty_mode[q], q,
					 0, 1, qty_birth[q], qty_death[q]);
      else
	qty_phys_reg[q] = -1;
    }

  /* Order the qtys so we assign them registers in order of 
     decreasing length of life.  Normally call qsort, but if we 
     have only a very small number of quantities, sort them ourselves.  */

  for (i = 0; i < next_qty; i++)
    qty_order[i] = i;

#define EXCHANGE(I1, I2)  \
  { i = qty_order[I1]; qty_order[I1] = qty_order[I2]; qty_order[I2] = i; }

  switch (next_qty)
    {
    case 3:
      /* Make qty_order[2] be the one to allocate last.  */
      if (qty_compare (0, 1) > 0)
	EXCHANGE (0, 1);
      if (qty_compare (1, 2) > 0)
	EXCHANGE (2, 1);

      /* ... Fall through ... */
    case 2:
      /* Put the best one to allocate in qty_order[0].  */
      if (qty_compare (0, 1) > 0)
	EXCHANGE (0, 1);

      /* ... Fall through ... */

    case 1:
    case 0:
      /* Nothing to do here.  */
      break;

    default:
      qsort (qty_order, next_qty, sizeof (int), qty_compare_1);
    }

  /* Now for each qty that is not a hardware register,
     look for a hardware register to put it in.
     First try the register class that is cheapest for this qty,
     if there is more than one class.  */

  for (i = 0; i < next_qty; i++)
    {
      q = qty_order[i];
      if (qty_phys_reg[q] < 0)
	{
	  if (N_REG_CLASSES > 1)
	    {
	      qty_phys_reg[q] = find_free_reg (qty_min_class[q], 
					       qty_mode[q], q, 0, 0,
					       qty_birth[q], qty_death[q]);
	      if (qty_phys_reg[q] >= 0)
		continue;
	    }

	  if (qty_alternate_class[q] != NO_REGS)
	    qty_phys_reg[q] = find_free_reg (qty_alternate_class[q],
					     qty_mode[q], q, 0, 0,
					     qty_birth[q], qty_death[q]);
	}
    }

  /* Now propagate the register assignments
     to the pseudo regs belonging to the qtys.  */

  for (q = 0; q < next_qty; q++)
    if (qty_phys_reg[q] >= 0)
      {
	for (i = qty_first_reg[q]; i >= 0; i = reg_next_in_qty[i])
	  reg_renumber[i] = qty_phys_reg[q] + reg_offset[i];
	if (qty_scratch_rtx[q])
	  {
	    if (GET_CODE (qty_scratch_rtx[q]) == REG)
	      abort ();
	    PUT_CODE (qty_scratch_rtx[q], REG);
	    REGNO (qty_scratch_rtx[q]) = qty_phys_reg[q];

	    scratch_block[scratch_index] = b;
	    scratch_list[scratch_index++] = qty_scratch_rtx[q];

	    /* Must clear the USED field, because it will have been set by
	       copy_rtx_if_shared, but the leaf_register code expects that
	       it is zero in all REG rtx.  copy_rtx_if_shared does not set the
	       used bit for REGs, but does for SCRATCHes.  */
	    qty_scratch_rtx[q]->used = 0;
	  }
      }
}

/* Compare two quantities' priority for getting real registers.
   We give shorter-lived quantities higher priority.
   Quantities with more references are also preferred, as are quantities that
   require multiple registers.  This is the identical prioritization as
   done by global-alloc.

   We used to give preference to registers with *longer* lives, but using
   the same algorithm in both local- and global-alloc can speed up execution
   of some programs by as much as a factor of three!  */

static int
qty_compare (q1, q2)
     int q1, q2;
{
  /* Note that the quotient will never be bigger than
     the value of floor_log2 times the maximum number of
     times a register can occur in one insn (surely less than 100).
     Multiplying this by 10000 can't overflow.  */
  register int pri1
    = (((double) (floor_log2 (qty_n_refs[q1]) * qty_n_refs[q1] * qty_size[q1])
	/ (qty_death[q1] - qty_birth[q1]))
       * 10000);
  register int pri2
    = (((double) (floor_log2 (qty_n_refs[q2]) * qty_n_refs[q2] * qty_size[q2])
	/ (qty_death[q2] - qty_birth[q2]))
       * 10000);
  return pri2 - pri1;
}

static int
qty_compare_1 (q1, q2)
     int *q1, *q2;
{
  register int tem;

  /* Note that the quotient will never be bigger than
     the value of floor_log2 times the maximum number of
     times a register can occur in one insn (surely less than 100).
     Multiplying this by 10000 can't overflow.  */
  register int pri1
    = (((double) (floor_log2 (qty_n_refs[*q1]) * qty_n_refs[*q1]
		  * qty_size[*q1])
	/ (qty_death[*q1] - qty_birth[*q1]))
       * 10000);
  register int pri2
    = (((double) (floor_log2 (qty_n_refs[*q2]) * qty_n_refs[*q2]
		  * qty_size[*q2])
	/ (qty_death[*q2] - qty_birth[*q2]))
       * 10000);

  tem = pri2 - pri1;
  if (tem != 0) return tem;
  /* If qtys are equally good, sort by qty number,
     so that the results of qsort leave nothing to chance.  */
  return *q1 - *q2;
}

/* Compare two quantities' priority for getting real registers.  This version
   is called for quantities that have suggested hard registers.  First priority
   goes to quantities that have copy preferences, then to those that have
   normal preferences.  Within those groups, quantities with the lower
   number of preferences have the highest priority.  Of those, we use the same
   algorithm as above.  */

static int
qty_sugg_compare (q1, q2)
     int q1, q2;
{
  register int sugg1 = (qty_phys_num_copy_sugg[q1]
			? qty_phys_num_copy_sugg[q1]
			: qty_phys_num_sugg[q1] * FIRST_PSEUDO_REGISTER);
  register int sugg2 = (qty_phys_num_copy_sugg[q2]
			? qty_phys_num_copy_sugg[q2]
			: qty_phys_num_sugg[q2] * FIRST_PSEUDO_REGISTER);
  /* Note that the quotient will never be bigger than
     the value of floor_log2 times the maximum number of
     times a register can occur in one insn (surely less than 100).
     Multiplying this by 10000 can't overflow.  */
  register int pri1
    = (((double) (floor_log2 (qty_n_refs[q1]) * qty_n_refs[q1] * qty_size[q1])
	/ (qty_death[q1] - qty_birth[q1]))
       * 10000);
  register int pri2
    = (((double) (floor_log2 (qty_n_refs[q2]) * qty_n_refs[q2] * qty_size[q2])
	/ (qty_death[q2] - qty_birth[q2]))
       * 10000);

  if (sugg1 != sugg2)
    return sugg1 - sugg2;
  
  return pri2 - pri1;
}

static int
qty_sugg_compare_1 (q1, q2)
     int *q1, *q2;
{
  register int sugg1 = (qty_phys_num_copy_sugg[*q1]
			? qty_phys_num_copy_sugg[*q1]
			: qty_phys_num_sugg[*q1] * FIRST_PSEUDO_REGISTER);
  register int sugg2 = (qty_phys_num_copy_sugg[*q2]
			? qty_phys_num_copy_sugg[*q2]
			: qty_phys_num_sugg[*q2] * FIRST_PSEUDO_REGISTER);

  /* Note that the quotient will never be bigger than
     the value of floor_log2 times the maximum number of
     times a register can occur in one insn (surely less than 100).
     Multiplying this by 10000 can't overflow.  */
  register int pri1
    = (((double) (floor_log2 (qty_n_refs[*q1]) * qty_n_refs[*q1]
		  * qty_size[*q1])
	/ (qty_death[*q1] - qty_birth[*q1]))
       * 10000);
  register int pri2
    = (((double) (floor_log2 (qty_n_refs[*q2]) * qty_n_refs[*q2]
		  * qty_size[*q2])
	/ (qty_death[*q2] - qty_birth[*q2]))
       * 10000);

  if (sugg1 != sugg2)
    return sugg1 - sugg2;
  
  if (pri1 != pri2)
    return pri2 - pri1;

  /* If qtys are equally good, sort by qty number,
     so that the results of qsort leave nothing to chance.  */
  return *q1 - *q2;
}

/* Attempt to combine the two registers (rtx's) USEDREG and SETREG.
   Returns 1 if have done so, or 0 if cannot.

   Combining registers means marking them as having the same quantity
   and adjusting the offsets within the quantity if either of
   them is a SUBREG).

   We don't actually combine a hard reg with a pseudo; instead
   we just record the hard reg as the suggestion for the pseudo's quantity.
   If we really combined them, we could lose if the pseudo lives
   across an insn that clobbers the hard reg (eg, movstr).

   ALREADY_DEAD is non-zero if USEDREG is known to be dead even though
   there is no REG_DEAD note on INSN.  This occurs during the processing
   of REG_NO_CONFLICT blocks.

   MAY_SAVE_COPYCOPY is non-zero if this insn is simply copying USEDREG to
   SETREG or if the input and output must share a register.
   In that case, we record a hard reg suggestion in QTY_PHYS_COPY_SUGG.
   
   There are elaborate checks for the validity of combining.  */

   
static int
combine_regs (usedreg, setreg, may_save_copy, insn_number, insn, already_dead)
     rtx usedreg, setreg;
     int may_save_copy;
     int insn_number;
     rtx insn;
     int already_dead;
{
  register int ureg, sreg;
  register int offset = 0;
  int usize, ssize;
  register int sqty;

  /* Determine the numbers and sizes of registers being used.  If a subreg
     is present that does not change the entire register, don't consider
     this a copy insn.  */

  while (GET_CODE (usedreg) == SUBREG)
    {
      if (GET_MODE_SIZE (GET_MODE (SUBREG_REG (usedreg))) > UNITS_PER_WORD)
	may_save_copy = 0;
      offset += SUBREG_WORD (usedreg);
      usedreg = SUBREG_REG (usedreg);
    }
  if (GET_CODE (usedreg) != REG)
    return 0;
  ureg = REGNO (usedreg);
  usize = REG_SIZE (usedreg);

  while (GET_CODE (setreg) == SUBREG)
    {
      if (GET_MODE_SIZE (GET_MODE (SUBREG_REG (setreg))) > UNITS_PER_WORD)
	may_save_copy = 0;
      offset -= SUBREG_WORD (setreg);
      setreg = SUBREG_REG (setreg);
    }
  if (GET_CODE (setreg) != REG)
    return 0;
  sreg = REGNO (setreg);
  ssize = REG_SIZE (setreg);

  /* If UREG is a pseudo-register that hasn't already been assigned a
     quantity number, it means that it is not local to this block or dies
     more than once.  In either event, we can't do anything with it.  */
  if ((ureg >= FIRST_PSEUDO_REGISTER && reg_qty[ureg] < 0)
      /* Do not combine registers unless one fits within the other.  */
      || (offset > 0 && usize + offset > ssize)
      || (offset < 0 && usize + offset < ssize)
      /* Do not combine with a smaller already-assigned object
	 if that smaller object is already combined with something bigger. */
      || (ssize > usize && ureg >= FIRST_PSEUDO_REGISTER
	  && usize < qty_size[reg_qty[ureg]])
      /* Can't combine if SREG is not a register we can allocate.  */
      || (sreg >= FIRST_PSEUDO_REGISTER && reg_qty[sreg] == -1)
      /* Don't combine with a pseudo mentioned in a REG_NO_CONFLICT note.
	 These have already been taken care of.  This probably wouldn't
	 combine anyway, but don't take any chances.  */
      || (ureg >= FIRST_PSEUDO_REGISTER
	  && find_reg_note (insn, REG_NO_CONFLICT, usedreg))
      /* Don't tie something to itself.  In most cases it would make no
	 difference, but it would screw up if the reg being tied to itself
	 also dies in this insn.  */
      || ureg == sreg
      /* Don't try to connect two different hardware registers.  */
      || (ureg < FIRST_PSEUDO_REGISTER && sreg < FIRST_PSEUDO_REGISTER)
      /* Don't connect two different machine modes if they have different
	 implications as to which registers may be used.  */
      || !MODES_TIEABLE_P (GET_MODE (usedreg), GET_MODE (setreg)))
    return 0;

  /* Now, if UREG is a hard reg and SREG is a pseudo, record the hard reg in
     qty_phys_sugg for the pseudo instead of tying them.

     Return "failure" so that the lifespan of UREG is terminated here;
     that way the two lifespans will be disjoint and nothing will prevent
     the pseudo reg from being given this hard reg.  */

  if (ureg < FIRST_PSEUDO_REGISTER)
    {
      /* Allocate a quantity number so we have a place to put our
	 suggestions.  */
      if (reg_qty[sreg] == -2)
	reg_is_born (setreg, 2 * insn_number);

      if (reg_qty[sreg] >= 0)
	{
	  if (may_save_copy
	      && ! TEST_HARD_REG_BIT (qty_phys_copy_sugg[reg_qty[sreg]], ureg))
	    {
	      SET_HARD_REG_BIT (qty_phys_copy_sugg[reg_qty[sreg]], ureg);
	      qty_phys_num_copy_sugg[reg_qty[sreg]]++;
	    }
	  else if (! TEST_HARD_REG_BIT (qty_phys_sugg[reg_qty[sreg]], ureg))
	    {
	      SET_HARD_REG_BIT (qty_phys_sugg[reg_qty[sreg]], ureg);
	      qty_phys_num_sugg[reg_qty[sreg]]++;
	    }
	}
      return 0;
    }

  /* Similarly for SREG a hard register and UREG a pseudo register.  */

  if (sreg < FIRST_PSEUDO_REGISTER)
    {
      if (may_save_copy
	  && ! TEST_HARD_REG_BIT (qty_phys_copy_sugg[reg_qty[ureg]], sreg))
	{
	  SET_HARD_REG_BIT (qty_phys_copy_sugg[reg_qty[ureg]], sreg);
	  qty_phys_num_copy_sugg[reg_qty[ureg]]++;
	}
      else if (! TEST_HARD_REG_BIT (qty_phys_sugg[reg_qty[ureg]], sreg))
	{
	  SET_HARD_REG_BIT (qty_phys_sugg[reg_qty[ureg]], sreg);
	  qty_phys_num_sugg[reg_qty[ureg]]++;
	}
      return 0;
    }

  /* At this point we know that SREG and UREG are both pseudos.
     Do nothing if SREG already has a quantity or is a register that we
     don't allocate.  */
  if (reg_qty[sreg] >= -1
      /* If we are not going to let any regs live across calls,
	 don't tie a call-crossing reg to a non-call-crossing reg.  */
      || (current_function_has_nonlocal_label
	  && ((reg_n_calls_crossed[ureg] > 0)
	      != (reg_n_calls_crossed[sreg] > 0))))
    return 0;

  /* We don't already know about SREG, so tie it to UREG
     if this is the last use of UREG, provided the classes they want
     are compatible.  */

  if ((already_dead || find_regno_note (insn, REG_DEAD, ureg))
      && reg_meets_class_p (sreg, qty_min_class[reg_qty[ureg]]))
    {
      /* Add SREG to UREG's quantity.  */
      sqty = reg_qty[ureg];
      reg_qty[sreg] = sqty;
      reg_offset[sreg] = reg_offset[ureg] + offset;
      reg_next_in_qty[sreg] = qty_first_reg[sqty];
      qty_first_reg[sqty] = sreg;

      /* If SREG's reg class is smaller, set qty_min_class[SQTY].  */
      update_qty_class (sqty, sreg);

      /* Update info about quantity SQTY.  */
      qty_n_calls_crossed[sqty] += reg_n_calls_crossed[sreg];
      qty_n_refs[sqty] += reg_n_refs[sreg];
      if (usize < ssize)
	{
	  register int i;

	  for (i = qty_first_reg[sqty]; i >= 0; i = reg_next_in_qty[i])
	    reg_offset[i] -= offset;

	  qty_size[sqty] = ssize;
	  qty_mode[sqty] = GET_MODE (setreg);
	}
    }
  else
    return 0;

  return 1;
}

/* Return 1 if the preferred class of REG allows it to be tied
   to a quantity or register whose class is CLASS.
   True if REG's reg class either contains or is contained in CLASS.  */

static int
reg_meets_class_p (reg, class)
     int reg;
     enum reg_class class;
{
  register enum reg_class rclass = reg_preferred_class (reg);
  return (reg_class_subset_p (rclass, class)
	  || reg_class_subset_p (class, rclass));
}

/* Return 1 if the two specified classes have registers in common.
   If CALL_SAVED, then consider only call-saved registers.  */

static int
reg_classes_overlap_p (c1, c2, call_saved)
     register enum reg_class c1;
     register enum reg_class c2;
     int call_saved;
{
  HARD_REG_SET c;
  int i;

  COPY_HARD_REG_SET (c, reg_class_contents[(int) c1]);
  AND_HARD_REG_SET (c, reg_class_contents[(int) c2]);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT (c, i)
	&& (! call_saved || ! call_used_regs[i]))
      return 1;

  return 0;
}

/* Update the class of QTY assuming that REG is being tied to it.  */

static void
update_qty_class (qty, reg)
     int qty;
     int reg;
{
  enum reg_class rclass = reg_preferred_class (reg);
  if (reg_class_subset_p (rclass, qty_min_class[qty]))
    qty_min_class[qty] = rclass;

  rclass = reg_alternate_class (reg);
  if (reg_class_subset_p (rclass, qty_alternate_class[qty]))
    qty_alternate_class[qty] = rclass;

  if (reg_changes_size[reg])
    qty_changes_size[qty] = 1;
}

/* Handle something which alters the value of an rtx REG.

   REG is whatever is set or clobbered.  SETTER is the rtx that
   is modifying the register.

   If it is not really a register, we do nothing.
   The file-global variables `this_insn' and `this_insn_number'
   carry info from `block_alloc'.  */

static void
reg_is_set (reg, setter)
     rtx reg;
     rtx setter;
{
  /* Note that note_stores will only pass us a SUBREG if it is a SUBREG of
     a hard register.  These may actually not exist any more.  */

  if (GET_CODE (reg) != SUBREG
      && GET_CODE (reg) != REG)
    return;

  /* Mark this register as being born.  If it is used in a CLOBBER, mark
     it as being born halfway between the previous insn and this insn so that
     it conflicts with our inputs but not the outputs of the previous insn.  */

  reg_is_born (reg, 2 * this_insn_number - (GET_CODE (setter) == CLOBBER));
}

/* Handle beginning of the life of register REG.
   BIRTH is the index at which this is happening.  */

static void
reg_is_born (reg, birth)
     rtx reg;
     int birth;
{
  register int regno;
     
  if (GET_CODE (reg) == SUBREG)
    regno = REGNO (SUBREG_REG (reg)) + SUBREG_WORD (reg);
  else
    regno = REGNO (reg);

  if (regno < FIRST_PSEUDO_REGISTER)
    {
      mark_life (regno, GET_MODE (reg), 1);

      /* If the register was to have been born earlier that the present
	 insn, mark it as live where it is actually born.  */
      if (birth < 2 * this_insn_number)
	post_mark_life (regno, GET_MODE (reg), 1, birth, 2 * this_insn_number);
    }
  else
    {
      if (reg_qty[regno] == -2)
	alloc_qty (regno, GET_MODE (reg), PSEUDO_REGNO_SIZE (regno), birth);

      /* If this register has a quantity number, show that it isn't dead.  */
      if (reg_qty[regno] >= 0)
	qty_death[reg_qty[regno]] = -1;
    }
}

/* Record the death of REG in the current insn.  If OUTPUT_P is non-zero,
   REG is an output that is dying (i.e., it is never used), otherwise it
   is an input (the normal case).
   If OUTPUT_P is 1, then we extend the life past the end of this insn.  */

static void
wipe_dead_reg (reg, output_p)
     register rtx reg;
     int output_p;
{
  register int regno = REGNO (reg);

  /* If this insn has multiple results,
     and the dead reg is used in one of the results,
     extend its life to after this insn,
     so it won't get allocated together with any other result of this insn.  */
  if (GET_CODE (PATTERN (this_insn)) == PARALLEL
      && !single_set (this_insn))
    {
      int i;
      for (i = XVECLEN (PATTERN (this_insn), 0) - 1; i >= 0; i--)
	{
	  rtx set = XVECEXP (PATTERN (this_insn), 0, i);
	  if (GET_CODE (set) == SET
	      && GET_CODE (SET_DEST (set)) != REG
	      && !rtx_equal_p (reg, SET_DEST (set))
	      && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	    output_p = 1;
	}
    }

  /* If this register is used in an auto-increment address, then extend its
     life to after this insn, so that it won't get allocated together with
     the result of this insn.  */
  if (! output_p && find_regno_note (this_insn, REG_INC, regno))
    output_p = 1;

  if (regno < FIRST_PSEUDO_REGISTER)
    {
      mark_life (regno, GET_MODE (reg), 0);

      /* If a hard register is dying as an output, mark it as in use at
	 the beginning of this insn (the above statement would cause this
	 not to happen).  */
      if (output_p)
	post_mark_life (regno, GET_MODE (reg), 1,
			2 * this_insn_number, 2 * this_insn_number+ 1);
    }

  else if (reg_qty[regno] >= 0)
    qty_death[reg_qty[regno]] = 2 * this_insn_number + output_p;
}

/* Find a block of SIZE words of hard regs in reg_class CLASS
   that can hold something of machine-mode MODE
     (but actually we test only the first of the block for holding MODE)
   and still free between insn BORN_INDEX and insn DEAD_INDEX,
   and return the number of the first of them.
   Return -1 if such a block cannot be found. 
   If QTY crosses calls, insist on a register preserved by calls,
   unless ACCEPT_CALL_CLOBBERED is nonzero.

   If JUST_TRY_SUGGESTED is non-zero, only try to see if the suggested
   register is available.  If not, return -1.  */

static int
find_free_reg (class, mode, qty, accept_call_clobbered, just_try_suggested,
	       born_index, dead_index)
     enum reg_class class;
     enum machine_mode mode;
     int qty;
     int accept_call_clobbered;
     int just_try_suggested;
     int born_index, dead_index;
{
  register int i, ins;
#ifdef HARD_REG_SET
  register		/* Declare it register if it's a scalar.  */
#endif
    HARD_REG_SET used, first_used;
#ifdef ELIMINABLE_REGS
  static struct {int from, to; } eliminables[] = ELIMINABLE_REGS;
#endif

  /* Validate our parameters.  */
  if (born_index < 0 || born_index > dead_index)
    abort ();

  /* Don't let a pseudo live in a reg across a function call
     if we might get a nonlocal goto.  */
  if (current_function_has_nonlocal_label
      && qty_n_calls_crossed[qty] > 0)
    return -1;

  if (accept_call_clobbered)
    COPY_HARD_REG_SET (used, call_fixed_reg_set);
  else if (qty_n_calls_crossed[qty] == 0)
    COPY_HARD_REG_SET (used, fixed_reg_set);
  else
    COPY_HARD_REG_SET (used, call_used_reg_set);

  for (ins = born_index; ins < dead_index; ins++)
    IOR_HARD_REG_SET (used, regs_live_at[ins]);

  IOR_COMPL_HARD_REG_SET (used, reg_class_contents[(int) class]);

  /* Don't use the frame pointer reg in local-alloc even if
     we may omit the frame pointer, because if we do that and then we
     need a frame pointer, reload won't know how to move the pseudo
     to another hard reg.  It can move only regs made by global-alloc.

     This is true of any register that can be eliminated.  */
#ifdef ELIMINABLE_REGS
  for (i = 0; i < sizeof eliminables / sizeof eliminables[0]; i++)
    SET_HARD_REG_BIT (used, eliminables[i].from);
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
  /* If FRAME_POINTER_REGNUM is not a real register, then protect the one
     that it might be eliminated into. */
  SET_HARD_REG_BIT (used, HARD_FRAME_POINTER_REGNUM);
#endif
#else
  SET_HARD_REG_BIT (used, FRAME_POINTER_REGNUM);
#endif

#ifdef CLASS_CANNOT_CHANGE_SIZE
  if (qty_changes_size[qty])
    IOR_HARD_REG_SET (used,
		      reg_class_contents[(int) CLASS_CANNOT_CHANGE_SIZE]);
#endif

  /* Normally, the registers that can be used for the first register in
     a multi-register quantity are the same as those that can be used for
     subsequent registers.  However, if just trying suggested registers,
     restrict our consideration to them.  If there are copy-suggested
     register, try them.  Otherwise, try the arithmetic-suggested
     registers.  */
  COPY_HARD_REG_SET (first_used, used);

  if (just_try_suggested)
    {
      if (qty_phys_num_copy_sugg[qty] != 0)
	IOR_COMPL_HARD_REG_SET (first_used, qty_phys_copy_sugg[qty]);
      else
	IOR_COMPL_HARD_REG_SET (first_used, qty_phys_sugg[qty]);
    }

  /* If all registers are excluded, we can't do anything.  */
  GO_IF_HARD_REG_SUBSET (reg_class_contents[(int) ALL_REGS], first_used, fail);

  /* If at least one would be suitable, test each hard reg.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
#ifdef REG_ALLOC_ORDER
      int regno = reg_alloc_order[i];
#else
      int regno = i;
#endif
      if (! TEST_HARD_REG_BIT (first_used, regno)
	  && HARD_REGNO_MODE_OK (regno, mode))
	{
	  register int j;
	  register int size1 = HARD_REGNO_NREGS (regno, mode);
	  for (j = 1; j < size1 && ! TEST_HARD_REG_BIT (used, regno + j); j++);
	  if (j == size1)
	    {
	      /* Mark that this register is in use between its birth and death
		 insns.  */
	      post_mark_life (regno, mode, 1, born_index, dead_index);
	      return regno;
	    }
#ifndef REG_ALLOC_ORDER
	  i += j;		/* Skip starting points we know will lose */
#endif
	}
    }

 fail:

  /* If we are just trying suggested register, we have just tried copy-
     suggested registers, and there are arithmetic-suggested registers,
     try them.  */
  
  /* If it would be profitable to allocate a call-clobbered register
     and save and restore it around calls, do that.  */
  if (just_try_suggested && qty_phys_num_copy_sugg[qty] != 0
      && qty_phys_num_sugg[qty] != 0)
    {
      /* Don't try the copy-suggested regs again.  */
      qty_phys_num_copy_sugg[qty] = 0;
      return find_free_reg (class, mode, qty, accept_call_clobbered, 1,
			    born_index, dead_index);
    }

  /* We need not check to see if the current function has nonlocal
     labels because we don't put any pseudos that are live over calls in
     registers in that case.  */

  if (! accept_call_clobbered
      && flag_caller_saves
      && ! just_try_suggested
      && qty_n_calls_crossed[qty] != 0
      && CALLER_SAVE_PROFITABLE (qty_n_refs[qty], qty_n_calls_crossed[qty]))
    {
      i = find_free_reg (class, mode, qty, 1, 0, born_index, dead_index);
      if (i >= 0)
	caller_save_needed = 1;
      return i;
    }
  return -1;
}

/* Mark that REGNO with machine-mode MODE is live starting from the current
   insn (if LIFE is non-zero) or dead starting at the current insn (if LIFE
   is zero).  */

static void
mark_life (regno, mode, life)
     register int regno;
     enum machine_mode mode;
     int life;
{
  register int j = HARD_REGNO_NREGS (regno, mode);
  if (life)
    while (--j >= 0)
      SET_HARD_REG_BIT (regs_live, regno + j);
  else
    while (--j >= 0)
      CLEAR_HARD_REG_BIT (regs_live, regno + j);
}

/* Mark register number REGNO (with machine-mode MODE) as live (if LIFE
   is non-zero) or dead (if LIFE is zero) from insn number BIRTH (inclusive)
   to insn number DEATH (exclusive).  */

static void
post_mark_life (regno, mode, life, birth, death)
     int regno;
     enum machine_mode mode;
     int life, birth, death;
{
  register int j = HARD_REGNO_NREGS (regno, mode);
#ifdef HARD_REG_SET
  register		/* Declare it register if it's a scalar.  */
#endif
    HARD_REG_SET this_reg;

  CLEAR_HARD_REG_SET (this_reg);
  while (--j >= 0)
    SET_HARD_REG_BIT (this_reg, regno + j);

  if (life)
    while (birth < death)
      {
	IOR_HARD_REG_SET (regs_live_at[birth], this_reg);
	birth++;
      }
  else
    while (birth < death)
      {
	AND_COMPL_HARD_REG_SET (regs_live_at[birth], this_reg);
	birth++;
      }
}

/* INSN is the CLOBBER insn that starts a REG_NO_NOCONFLICT block, R0
   is the register being clobbered, and R1 is a register being used in
   the equivalent expression.

   If R1 dies in the block and has a REG_NO_CONFLICT note on every insn
   in which it is used, return 1.

   Otherwise, return 0.  */

static int
no_conflict_p (insn, r0, r1)
     rtx insn, r0, r1;
{
  int ok = 0;
  rtx note = find_reg_note (insn, REG_LIBCALL, NULL_RTX);
  rtx p, last;

  /* If R1 is a hard register, return 0 since we handle this case
     when we scan the insns that actually use it.  */

  if (note == 0
      || (GET_CODE (r1) == REG && REGNO (r1) < FIRST_PSEUDO_REGISTER)
      || (GET_CODE (r1) == SUBREG && GET_CODE (SUBREG_REG (r1)) == REG
	  && REGNO (SUBREG_REG (r1)) < FIRST_PSEUDO_REGISTER))
    return 0;

  last = XEXP (note, 0);

  for (p = NEXT_INSN (insn); p && p != last; p = NEXT_INSN (p))
    if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
      {
	if (find_reg_note (p, REG_DEAD, r1))
	  ok = 1;

	if (reg_mentioned_p (r1, PATTERN (p))
	    && ! find_reg_note (p, REG_NO_CONFLICT, r1))
	  return 0;
      }
      
  return ok;
}

#ifdef REGISTER_CONSTRAINTS

/* Return the number of alternatives for which the constraint string P
   indicates that the operand must be equal to operand 0 and that no register
   is acceptable.  */

static int
requires_inout (p)
     char *p;
{
  char c;
  int found_zero = 0;
  int reg_allowed = 0;
  int num_matching_alts = 0;

  while (c = *p++)
    switch (c)
      {
      case '=':  case '+':  case '?':
      case '#':  case '&':  case '!':
      case '*':  case '%':
      case '1':  case '2':  case '3':  case '4':
      case 'm':  case '<':  case '>':  case 'V':  case 'o':
      case 'E':  case 'F':  case 'G':  case 'H':
      case 's':  case 'i':  case 'n':
      case 'I':  case 'J':  case 'K':  case 'L':
      case 'M':  case 'N':  case 'O':  case 'P':
#ifdef EXTRA_CONSTRAINT
      case 'Q':  case 'R':  case 'S':  case 'T':  case 'U':
#endif
      case 'X':
	/* These don't say anything we care about.  */
	break;

      case ',':
	if (found_zero && ! reg_allowed)
	  num_matching_alts++;

	found_zero = reg_allowed = 0;
	break;

      case '0':
	found_zero = 1;
	break;

      case 'p':
      case 'g': case 'r':
      default:
	reg_allowed = 1;
	break;
      }

  if (found_zero && ! reg_allowed)
    num_matching_alts++;

  return num_matching_alts;
}
#endif /* REGISTER_CONSTRAINTS */

void
dump_local_alloc (file)
     FILE *file;
{
  register int i;
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] != -1)
      fprintf (file, ";; Register %d in %d.\n", i, reg_renumber[i]);
}
