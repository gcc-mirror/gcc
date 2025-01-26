/* Support for avr-passes.def for AVR 8-bit microcontrollers.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#define INCLUDE_ARRAY
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "intl.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "cfganal.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "explow.h"
#include "cfgrtl.h"
#include "context.h"
#include "tree-pass.h"
#include "insn-attr.h"
#include "tm-constrs.h"


#define CONST_INT_OR_FIXED_P(X) (CONST_INT_P (X) || CONST_FIXED_P (X))

#define FIRST_GPR (AVR_TINY ? REG_18 : REG_2)


// Emit pattern PAT, and ICE when the insn is not valid / not recognized.

static rtx_insn *
emit_valid_insn (rtx pat)
{
  rtx_insn *insn = emit_insn (pat);

  if (! valid_insn_p (insn))  // Also runs recog().
    fatal_insn ("emit unrecognizable insn", insn);

  return insn;
}

// Emit a single_set with an optional scratch operand.  This function
// asserts that the new insn is valid and recognized.

static rtx_insn *
emit_valid_move_clobbercc (rtx dest, rtx src, rtx scratch = NULL_RTX)
{
  rtx pat = scratch
    ? gen_gen_move_clobbercc_scratch (dest, src, scratch)
    : gen_gen_move_clobbercc (dest, src);

  return emit_valid_insn (pat);
}


namespace
{

/////////////////////////////////////////////////////////////////////////////
// Before we start with the very code, introduce some helpers that are
// quite generic, though up to now only avr-fuse-add makes use of them.

/* Get the next / previous NONDEBUG_INSN_P after INSN in basic block BB.
   This assumes we are in CFG layout mode so that BLOCK_FOR_INSN()
   can be used.  */

static rtx_insn *
next_nondebug_insn_bb (basic_block bb, rtx_insn *insn, bool forward = true)
{
  while (insn)
    {
      insn = forward ? NEXT_INSN (insn) : PREV_INSN (insn);

      if (insn && NONDEBUG_INSN_P (insn))
	return BLOCK_FOR_INSN (insn) == bb ? insn : nullptr;
    }

  return insn;
}

static rtx_insn *
prev_nondebug_insn_bb (basic_block bb, rtx_insn *insn)
{
  return next_nondebug_insn_bb (bb, insn, false);
}


/* Like `single_set' with the addition that it sets REGNO_SCRATCH when the
   insn is a single_set with a QImode scratch register.  When the insn has
   no QImode scratch or just a scratch:QI, then set REGNO_SCRATCH = 0.
   The assumption is that the function is only used after the splits for
   REG_CC so that the pattern is a parallel with 2 elements (INSN has no
   scratch operand), or 3 elements (INSN does have a scratch operand).  */

static rtx
single_set_with_scratch (rtx_insn *insn, int &regno_scratch)
{
  regno_scratch = 0;

  if (! INSN_P (insn))
    return NULL_RTX;

  rtx set, clo, reg, pat = PATTERN (insn);

  // Search for SET + CLOBBER(QI) + CLOBBER(CC).
  if (GET_CODE (pat) == PARALLEL
      && XVECLEN (pat, 0) == 3
      && GET_CODE (set = XVECEXP (pat, 0, 0)) == SET
      // At this pass, all insn are endowed with clobber(CC).
      && GET_CODE (clo = XVECEXP (pat, 0, 2)) == CLOBBER
      && GET_MODE (XEXP (clo, 0)) == CCmode
      && GET_CODE (clo = XVECEXP (pat, 0, 1)) == CLOBBER
      && REG_P (reg = XEXP (clo, 0))
      && GET_MODE (reg) == QImode)
    {
      regno_scratch = REGNO (reg);
      return set;
    }

  return single_set (insn);
}


// One bit for each GRP in REG_0 ... REG_31.
using gprmask_t = uint32_t;

// True when this is a valid GPR number for ordinary code, e.g.
// registers wider than 2 bytes have to start at an exven regno.
// TMP_REG and ZERO_REG are not considered valid, even though
// the C source can use register vars with them.
static inline bool
gpr_regno_p (int regno, int n_bytes = 1)
{
  return (IN_RANGE (regno, FIRST_GPR, REG_32 - n_bytes)
	  // Size in { 1, 2, 3, 4, 8 } bytes.
	  && ((1u << n_bytes) & 0x11e)
	  // Registers >= 2 bytes start at an even regno.
	  && (n_bytes == 1 || regno % 2 == 0));
}

// There are cases where the C source defines local reg vars
// for R1 etc.  The assumption is that this is handled before
// calling this function, e.g. by skipping code when a register
// overlaps with a fixed register.
static inline gprmask_t
regmask (int regno, int size)
{
  gcc_checking_assert (gpr_regno_p (regno, size));
  gprmask_t bits = (1u << size) - 1;

  return bits << regno;
}

// Mask for hard register X that's some GPR, including fixed regs like R0.
static gprmask_t
regmask (rtx x)
{
  gcc_assert (REG_P (x));
  gprmask_t bits = (1u << GET_MODE_SIZE (GET_MODE (x))) - 1;

  return bits << REGNO (x);
}


// Whether X has bits in the range [B0 ... B1]
static inline bool
has_bits_in (gprmask_t x, int b0, int b1)
{
  if (b0 > b1 || b0 > 31 || b1 < 0)
    return false;

  const gprmask_t m = (2u << (b1 - b0)) - 1;
  return x & (m << b0);
}


template<typename T>
T bad_case ()
{
  gcc_unreachable ();
}

#define select false ? bad_case


namespace AVRasm
{
  // Returns true when we a scratch reg is needed in order to get
  // (siged or unsigned) 8-bit value VAL in some GPR.
  // When it's about costs rather than the sheer requirement for a
  // scratch, see also AVRasm::constant_cost.
  static inline bool ldi_needs_scratch (int regno, int val)
  {
    return regno < REG_16 && IN_RANGE (val & 0xff, 2, 254);
  }

  // Return a byte value x >= 0 such that  x <code> y == x for all y, or -1.
  static inline int neutral_val (rtx_code code)
  {
    return select<int>()
      : code == AND ? 0xff
      : code == IOR ? 0x00
      : code == XOR ? 0x00
      : code == PLUS ? 0
      : -1;
  }

  // When there exists a value x such that the image of the function
  //   y -> y <code> x  has order 1, then return that x.  Else return -1.
  static inline int image1_val (rtx_code code)
  {
    return select<int>()
      : code == AND ? 0x00
      : code == IOR ? 0xff
      : -1;
  }

  // Cost of 8-bit binary operation  x o= VAL  provided a scratch is
  // available as needed.
  static int constant_cost (rtx_code code, int regno, uint8_t val)
  {
    bool needs_scratch_p = select<bool>()
      : code == PLUS ? regno < REG_16 && val != 1 && val != 0xff
      : code == XOR ? val != 0xff && (regno < REG_16 || val != 0x80)
      : code == IOR ? regno < REG_16
      : code == AND ? regno < REG_16 && val != 0
      : code == SET ? regno < REG_16 && val != 0
      : bad_case<bool> ();

    return val == AVRasm::neutral_val (code)
      ? 0
      : 1 + needs_scratch_p;
  }
}; // AVRasm


// Returns the mode mask for a mode size of SIZE bytes.
static uint64_t size_to_mask (int size)
{
  return ((uint64_t) 2 << (8 * size - 1)) - 1;
}

// Return the scalar int mode for a modesize of 1, 2, 3, 4 or 8 bytes.
static machine_mode size_to_mode (int size)
{
  return select<machine_mode>()
    : size == 1 ? QImode
    : size == 2 ? HImode
    : size == 3 ? PSImode
    : size == 4 ? SImode
    : size == 8 ? DImode
    : bad_case<machine_mode> ();
}


//////////////////////////////////////////////////////////////////////////////
// Optimize moves after reload: -mfuse-move=<0,23>

/* The purpose of this pass is to perform optimizations after reload
   like the following ones:

   Without optimization		     |	 With optimization
   ====================		     |	 =================

   long long fn_zero (void)	    (1)
   {
      return 0;
   }

   ldi r18, 0	  ;  movqi_insn	     |	 ldi r18, 0	;  movqi_insn
   ldi r19, 0	  ;  movqi_insn	     |	 ldi r19, 0	;  movqi_insn
   ldi r20, 0	  ;  movqi_insn	     |	 movw r20, r18	;  *movhi
   ldi r21, 0	  ;  movqi_insn	     |
   ldi r22, 0	  ;  movqi_insn	     |	 movw r22, r18	;  *movhi
   ldi r23, 0	  ;  movqi_insn	     |
   ldi r24, 0	  ;  movqi_insn	     |	 movw r24, r18	;  *movhi
   ldi r25, 0	  ;  movqi_insn	     |
   ret				     |	 ret

   int fn_eq0 (char c)		    (2)
   {
       return c == 0;
   }

   mov r18, r24	   ;  movqi_insn     |	 mov r18, r24	;  movqi_insn
   ldi r24, 1	   ;  *movhi	     |	 ldi r24, 1	;  *movhi
   ldi r25, 0			     |	 ldi r25, 0
   cp  r18, ZERO   ;  cmpqi3	     |	 cpse r18, ZERO ;  peephole
   breq .+4	   ;  branch	     |
   ldi r24, 0	   ;  *movhi	     |	 ldi r24, 0	;  movqi_insn
   ldi r25, 0			     |
   ret				     |	 ret

   int a, b;			    (3)

   void fn_store_ab (void)
   {
       a = 1;
       b = -1;
   }

   ldi r24, 1	   ;  *movhi	     |	ldi r24, 1	 ;  *movhi
   ldi r25, 0			     |	ldi r25, 0
   sts a+1, r25	   ;  *movhi	     |	sts a+1, r25	 ;  *movhi
   sts a,   r24			     |	sts a,	 r24
   ldi r24, -1	   ;  *movhi	     |	sbiw r24, 2	 ;  *addhi3
   ldi r25, -1			     |
   sts b+1, r25	   ;  *movhi	     |	sts b+1, r25	 ;  *movhi
   sts b,   r24			     |	sts b,	 r24
   ret				     |	ret

   unsigned fn_crc (unsigned x, unsigned y)   (4)
   {
       for (char i = 8; i--; x <<= 1)
	   y ^= (x ^ y) & 0x80 ? 79U : 0U;
       return y;
   }

   movw r18, r24   ;  *movhi	     |	movw r18, r24	 ;  *movhi
   movw r24, r22   ;  *movhi	     |	movw r24, r22	 ;  *movhi
   ldi	r22, 8	   ;  movqi_insn     |	ldi  r22, 8	 ;  movqi_insn
  .L13:				     | .L13:
   movw r30, r18   ;  *movhi	     |	movw r30, r18	 ;  *movhi
   eor	r30, r24   ;  *xorqi3	     |	eor  r30, r24	 ;  *xorqi3
   eor	r31, r25   ;  *xorqi3	     |	eor  r31, r25	 ;  *xorqi3
   mov	r20, r30   ;  *andhi3	     |	mov  r20, r30	 ;  *andqi3
   andi r20, 1<<7		     |	andi r20, 1<<7
   clr	r21			     |
   sbrs r30, 7	   ;  *sbrx_branchhi |	sbrc r30, 7	 ;  *sbrx_branchhi
   rjmp .+4			     |
   ldi	r20, 79	   ;  movqi_insn     |	ldi  r20, 79	 ;  movqi_insn
   ldi	r21, 0	   ;  movqi_insn     |
   eor	r24, r20   ;  *xorqi3	     |	eor r24, r20	 ;  *xorqi3
   eor	r25, r21   ;  *xorqi3	     |
   lsl	r18	   ;  *ashlhi3_const |	lsl  r18	 ;  *ashlhi3_const
   rol	r19			     |	rol  r19
   subi r22, 1	   ;  *op8.for.cczn.p|	subi r22, 1	 ;  *op8.for.cczn.plus
   brne .L13	   ;  branch_ZN	     |	brne .L13	 ;  branch_ZN
   ret				     |	ret

   #define SPDR (*(uint8_t volatile*) 0x2c)     (5)

   void fn_PR49807 (long big)
   {
       SPDR = big >> 24;
       SPDR = big >> 16;
       SPDR = big >> 8;
       SPDR = big;
   }

   movw r20, r22   ;  *movhi	     |	movw r20, r22	 ;  *movhi
   movw r22, r24   ;  *movhi	     |	movw r22, r24	 ;  *movhi
   mov	r24, r23   ;  *ashrsi3_const |
   clr	r27			     |
   sbrc r24,7			     |
   com	r27			     |
   mov	r25, r27		     |
   mov	r26, r27		     |
   out	0xc, r24   ;  movqi_insn     |	out 0xc, r23	 ;  movqi_insn
   movw r24, r22   ;  *ashrsi3_const |
   clr	r27			     |
   sbrc r25, 7			     |
   com	r27			     |
   mov	r26, r27		     |
   out	0xc, r24   ;  movqi_insn     |	out 0xc, r24	 ;  movqi_insn
   clr	r27	   ;  *ashrsi3_const |
   sbrc r23, 7			     |
   dec	r27			     |
   mov	r26, r23		     |
   mov	r25, r22		     |
   mov	r24, r21		     |
   out	0xc, r24   ;  movqi_insn     |	out 0xc, r21	 ;  movqi_insn
   out	0xc, r20   ;  movqi_insn     |	out 0xc, r20	 ;  movqi_insn
   ret				     |	ret

   The insns of each basic block are traversed from first to last.
   Each insn is optimized on its own, or may be fused with the
   previous insn like in example (1).
      As the insns are traversed, memento_t keeps track of known values
   held in the GPRs (general purpse registers) R2 ... R31 by simulating
   the effect of the current insn in memento_t.apply_insn().
      The basic blocks are traversed in reverse post order so as to
   maximize the chance that GPRs from all preceding blocks are known,
   which is the case in example (2).  The traversal of the basic block
   is performed by bbinfo_t.optimize_one_function().
      bbinfo_t.optimize_one_block() traverses the insns of a BB and tries
   the following optimizations:

   bbinfo_t::try_fuse_p
      Try to fuse two 8-bit insns to one MOVW like in (1).

   bbinfo_t::try_simplify_p
      Only perform the simplest optimizations that don't impede the
      traceability of the generated code, which are:
      - Transform operations like  Rn = Rn=0 ^ Rm  to  Rn = Rm.
      - Remove insns that are no-ops like  Rn = Rn ^ Rm=0.

   bbinfo_t::try_bin_arg1_p
      In insns like  EOR Rn, arg1  where arg1 is known or is a reg that
      dies in the insn, *and* there is a different register Rm that's
      known to contain the same value, then arg1 is replaced with Rm.

   bbinfo_t::try_split_ldi_p
      Tries to simplify loads of constants like in examples (1), (2) and (3).
      It may use arithmetic instructions like AND with registers that
      are holding known values when this is profitable.

   bbinfo_t::try_split_any_p
      Split all insns where the operation can be performed on individual
      bytes, like andsi3.  In example (4) the andhi3 can be optimized
      to an andqi3.

   bbinfo_t::try_mem0_p
      Try to fuse a mem = reg insn to mem = __zero_reg__.
      This should only occur when -msplit-ldst is on, but may
      also occur with pushes since push<mode>1 splits them.
*/


// A basic block with additional information like the GPR state.
// The main entry point for the pass.  Runs various strategies
// like try_fuse, try_simplify, try_bin_arg1, try_split_ldi, try_split_any
// depending on -mfuse-add=<0,11>.
struct bbinfo_t;

// Additional insn information on a  REG = non-memory  single_set insn
// for quick access.  Only valid when the m_size member is non-zero.
struct insninfo_t;

// Helper classes with data needed by the try_xxx optimizers.
struct optimize_data_t;
struct insn_optimize_data_t;

// Records which GPRs R0 ... R31 are holding a known value,
// and which values these are.
struct memento_t;

// Abstract Interpretation of expressions.
// absint_val_t represents an 8-bit value that equals the content of
//    some GPR, or equals some known value (or both, or none of them).
// absint_byte_t represents an 8-bit entity that is equivalent to
//    an absint_val_t, or is equivalent to some (unary or binary) operation
//    on absint_val_t's like NOT, AND, IOR, XOR that operate bit-wise (and
//    hence also byte-wise).
// absint_t represents an array of absint_byte_t's.  When some insn is applied
//    to a GPR state, then memento_t.apply_insn() represents the RHS of
//    a single_set as an absint_t, and then applies that result to the GPRs.
//    For example, in  int y = x << 8  the representation is  x = [r25; r24]
//    and  RHS = [r24; 00].
struct absint_val_t;
class absint_byte_t;
struct absint_t;

// A ply_t is a potential step towards an optimal sequence to load a constant
// value into a multi-byte register.  A ply_t loosely relates to one AVR
// instruction, but it may also represent a sequence of instructions.
// For example, loading a constant into a lower register when no sratch reg
// is available may take up to 4 instructions.  There is no 1:1 correspondence
// to insns, either.
//    try_split_ldi determines the best sequence of ply_t's by means of a
// brute-force search with tree pruning:  It's much too complicated to
// construct a good sequence directly, but there are many conditions that
// good sequence will satisfy, implemented in bbinfo_t::find_plies.
struct ply_t;
struct plies_t;

// The maximal number of ply_t's in any conceivable optimal solution
// that is better than what a vanilla mov<mode> generates.
// This is 6 for modes <= 4 and 8 for modes == 8.
static constexpr int N_BEST_PLYS = 8;

#define FUSE_MOVE_MAX_MODESIZE 8

#include "avr-passes-fuse-move.h"

// Static members.

gprmask_t memento_t::fixed_regs_mask;

// Statistics.
int ply_t::n_ply_ts;
int ply_t::max_n_ply_ts;
int plies_t::max_n_plies;

bbinfo_t *bbinfo_t::bb_info;
int bbinfo_t::tick;
bbinfo_t::find_plies_data_t *bbinfo_t::fpd;

// Which optimizations should be performed.
bool bbinfo_t::try_fuse_p;
bool bbinfo_t::try_bin_arg1_p;
bool bbinfo_t::try_split_ldi_p;
bool bbinfo_t::try_split_any_p;
bool bbinfo_t::try_simplify_p;
bool bbinfo_t::use_arith_p;
bool bbinfo_t::use_set_some_p;
bool bbinfo_t::try_mem0_p;


// Abstract Interpretation of expressions.
// A bunch of absint_byte_t's.

struct absint_t
{
  static constexpr int eq_size = FUSE_MOVE_MAX_MODESIZE;
  std::array<absint_byte_t, eq_size> eq;

  rtx xexp = NULL_RTX;
  rtx xexp_new = NULL_RTX;

  absint_byte_t &operator[] (int i)
  {
    gcc_assert (IN_RANGE (i, 0, absint_t::eq_size - 1));
    return eq[i];
  }

  const absint_byte_t &operator[] (int i) const
  {
    gcc_assert (IN_RANGE (i, 0, absint_t::eq_size - 1));
    return eq[i];
  }

  absint_t () {}

  absint_t (rtx xold)
    : xexp(xold)
  {}

  absint_t (rtx xold, rtx xnew, int n_bytes)
    : xexp(xold), xexp_new(xnew)
  {
    gcc_assert (n_bytes <= eq_size);
    if (xnew)
      for (int i = 0; i < n_bytes; ++i)
	eq[i].learn_val8 (avr_uint8 (xnew, i));
  }

  // CODE != UNKNOWN: Maximal index of a byte with code CODE, or -1.
  // CODE == UNKNOWN: Maximal index of a byte with known CODE, or -1.
  int max_knows (rtx_code code = UNKNOWN) const
  {
    for (int i = eq_size - 1; i >= 0; --i)
      if ((code == UNKNOWN && ! eq[i].can (UNKNOWN))
	  || (code != UNKNOWN && eq[i].can (code)))
	return i;
    return -1;
  }

  // CODE != UNKNOWN: Maximal i such that all bytes < i have code CODE.
  // CODE == UNKNOWN: Maximal i such that all bytes < i have code != UNKNOWN.
  int end_knows (rtx_code code = UNKNOWN) const
  {
    for (int i = 0; i < eq_size; ++i)
      if ((code == UNKNOWN && eq[i].can (UNKNOWN))
	  || (code != UNKNOWN && ! eq[i].can (code)))
	return i;
    return eq_size;
  }

  // Number of bytes for which there is usable information.
  int popcount () const
  {
    int pop = 0;
    for (int i = 0; i < eq_size; ++i)
      pop += ! eq[i].can (UNKNOWN);
    return pop;
  }

  // Get the value under the assumption that all eq[].val8 are known.
  uint64_t get_value (int n_bytes, bool strict = true) const
  {
    gcc_assert (IN_RANGE (n_bytes, 1, eq_size));
    gcc_assert (! strict || end_knows (CONST_INT) >= n_bytes);

    uint64_t val = 0;
    for (int i = n_bytes - 1; i >= 0; --i)
      val = 256 * val + eq[i].val8 (strict);
    return val;
  }

  // Get n-byte value as a const_int, or NULL_RTX when (partially) unknown.
  rtx get_value_as_const_int (int n_bytes) const
  {
    gcc_checking_assert (gpr_regno_p (REG_24, n_bytes));

    if (end_knows (CONST_INT) < n_bytes)
      return NULL_RTX;

    const uint64_t val = get_value (n_bytes);
    const machine_mode mode = size_to_mode (n_bytes);

    return gen_int_mode (val, mode);
  }

  // Find a 16-bit register that contains the same value like held
  // in positions I1 and I2 (if any).  Return 0 when nothing appropriate
  // for a MOVW is found.
  int reg16_with_value (int i1, int i2, const memento_t &memo) const
  {
    if (i1 == (i2 ^ 1))
      {
	const int lo8 = eq[i1 & ~1].val8 (false);
	const int hi8 = eq[i1 | 1].val8 (false);
	if (lo8 >= 0 && hi8 >= 0)
	  return memo.reg16_with_value (lo8, hi8, 0);
      }
    return 0;
  }

  // When X is a REG rtx with a known content as of MEMO, then return
  // the respective value as a constant for mode MODE.
  // If X is NULL_RTX, or not a REG, or not known, then return NULL_RTX.
  static rtx maybe_fold (rtx x, const memento_t &memo)
  {
    int n_bytes;

    if (x != NULL_RTX
	&& REG_P (x)
	&& (n_bytes = GET_MODE_SIZE (GET_MODE (x))) <= FUSE_MOVE_MAX_MODESIZE
	&& gpr_regno_p (REGNO (x), n_bytes))
      {
	rtx xval = memo.get_value_as_const_int (REGNO (x), n_bytes);
	if (xval)
	  return avr_chunk (GET_MODE (x), xval, 0);
      }

    return NULL_RTX;
  }

  // Try to conclude about the bytes that comprise X.  DEST_MODE is the
  // context mode that is used when X is CONST_INT and has VOIDmode.
  static absint_t explore (rtx x, const memento_t &memo,
			   machine_mode dest_mode = VOIDmode)
  {
    const rtx_code code = GET_CODE (x);
    bool worth_dumping = dump_file && (dump_flags & TDF_FOLDING);

    const machine_mode mode = GET_MODE (x) == VOIDmode
      ? dest_mode
      : GET_MODE (x);

    const int n_bytes = mode == VOIDmode && CONST_INT_P (x)
      ? absint_t::eq_size
      : GET_MODE_SIZE (mode);

    if (! IN_RANGE (n_bytes, 1, absint_t::eq_size))
      return absint_t (x);

    // Eat our own dog food as produced by try_plit_ldi.

    rtx xop0 = BINARY_P (x) || UNARY_P (x) ? XEXP (x, 0) : NULL_RTX;
    rtx xval0 = xop0 && CONST_INT_OR_FIXED_P (xop0)
      ? xop0
      : absint_t::maybe_fold (xop0, memo);

    if (UNARY_P (x)
	&& REG_P (xop0)
	&& GET_MODE (xop0) == mode
	&& xval0)
      {
	rtx y = simplify_unary_operation (code, mode, xval0, mode);
	if (y && CONST_INT_OR_FIXED_P (y))
	  return absint_t (x, y, n_bytes);
      }

    rtx xop1 = BINARY_P (x) ? XEXP (x, 1) : NULL_RTX;
    rtx xval1 = xop1 && CONST_INT_OR_FIXED_P (xop1)
      ? xop1
      : absint_t::maybe_fold (xop1, memo);

    if (BINARY_P (x)
	&& xval0 && xval1)
      {
	rtx y = simplify_binary_operation (code, mode, xval0, xval1);
	if (y && CONST_INT_OR_FIXED_P (y))
	  return absint_t (x, y, n_bytes);
      }

    // No fold to a constant value was found:
    // Look at the individual bytes more closely.

    absint_t ai (x);

    switch (code)
      {
      default:
	worth_dumping = false;
	break;

      case REG:
	if (END_REGNO (x) <= REG_32
	    && ! (regmask (x) & memento_t::fixed_regs_mask))
	  for (unsigned r = REGNO (x); r < END_REGNO (x); ++r)
	    {
	      ai[r - REGNO (x)].learn_regno (r);
	      if (memo.knows (r))
		ai[r - REGNO (x)].learn_val8 (memo.value (r));
	    }
	break;

      CASE_CONST_UNIQUE:
	ai = absint_t (x, x, n_bytes);
	break;

      case ASHIFT:
      case ASHIFTRT:
      case LSHIFTRT:
      case ROTATE:
      case ROTATERT:
	if ((CONST_INT_P (xop1) && INTVAL (xop1) >= 8)
	    // DImode shift offsets for transparent calls are shipped in R16.
	    || n_bytes == 8)
	  ai = explore_shift (x, memo);
	break;

      case AND:
      case IOR:
      case XOR:
	{
	  const absint_t ai0 = absint_t::explore (xop0, memo, mode);
	  const absint_t ai1 = absint_t::explore (xop1, memo, mode);
	  for (int i = 0; i < n_bytes; ++i)
	    ai[i] = absint_byte_t (code, ai0[i], ai1[i]);
	}
	break;

      case NOT:
	{
	  const absint_t ai0 = absint_t::explore (xop0, memo);
	  for (int i = 0; i < n_bytes; ++i)
	    ai[i] = absint_byte_t (NOT, ai0[i]);
	}
	break;

      case ZERO_EXTEND:
      case SIGN_EXTEND:
	{
	  const absint_t ai0 = absint_t::explore (xop0, memo);
	  const int ai0_size = GET_MODE_SIZE (GET_MODE (xop0));
	  const absint_byte_t b_signs = ai0[ai0_size - 1].get_signs (code);
	  for (int i = 0; i < n_bytes; ++i)
	    ai[i] = i < ai0_size ? ai0[i] : b_signs;
	}
	break;

      case PLUS:
      case MINUS:
	if (SCALAR_INT_MODE_P (mode)
	    || ALL_SCALAR_FIXED_POINT_MODE_P (mode))
	  {
	    const absint_t ai0 = absint_t::explore (xop0, memo, mode);
	    const absint_t ai1 = absint_t::explore (xop1, memo, mode);
	    if (code == MINUS)
	      for (int i = 0; i < n_bytes && ai1[i].val8 (false) == 0; ++i)
		ai[i] = ai0[i];

	    if (code == PLUS)
	      for (int i = 0; i < n_bytes; ++i)
		{
		  if (ai0[i].val8 (false) == 0)
		    ai[i] = ai1[i];
		  else if (ai1[i].val8 (false) == 0)
		    ai[i] = ai0[i];
		  else
		    {
		      ai[i] = absint_byte_t (code, ai0[i], ai1[i]);
		      break;
		    }
		}

	    if (code == PLUS
		&& GET_CODE (xop0) == ZERO_EXTEND
		&& CONST_INT_P (xop1))
	      {
		rtx exop = XEXP (xop0, 0);
		int exsize = GET_MODE_SIZE (GET_MODE (exop));
		rtx lo_xop1 = avr_chunk (GET_MODE (exop), xop1, 0);
		if (lo_xop1 == const0_rtx)
		  for (int i = exsize; i < n_bytes; ++i)
		    ai[i] = ai1[i];
	      }
	  }
	break; // PLUS, MINUS

      case MULT:
	if (GET_MODE (xop0) == mode
	    && SCALAR_INT_MODE_P (mode))
	  {
	    // The constant may be located in xop0's zero_extend...
	    const absint_t ai0 = absint_t::explore (xop0, memo, mode);
	    const absint_t ai1 = absint_t::explore (xop1, memo, mode);
	    const int end0 = ai0.end_knows (CONST_INT);
	    const int end1 = ai1.end_knows (CONST_INT);
	    const uint64_t mul0 = end0 > 0 ? ai0.get_value (end0) : 1;
	    const uint64_t mul1 = end1 > 0 ? ai1.get_value (end1) : 1;
	    // Shifting in off/8 zero bytes from the right.
	    const int off = mul0 * mul1 != 0 ? ctz_hwi (mul0 * mul1) : 0;
	    for (int i = 0; i < off / 8; ++i)
	      ai[i].learn_val8 (0);
	  }
	break; // MULT

      case BSWAP:
	if (GET_MODE (xop0) == mode)
	  {
	    const absint_t ai0 = absint_t::explore (xop0, memo);
	    for (int i = 0; i < n_bytes; ++i)
	      ai[i] = ai0[n_bytes - 1 - i];
	  }
	break;
      } // switch code

    if (worth_dumping)
      {
	avr_dump (";; AI.explore %C:%m ", code, mode);
	ai.dump ();
      }

    for (int i = 0; i < n_bytes; ++i)
      gcc_assert (ai[i].check ());

    return ai;
  }

  // Helper for the method above.
  static absint_t explore_shift (rtx x, const memento_t &memo)
  {
    absint_t ai (x);

    const rtx_code code = GET_CODE (x);
    const machine_mode mode = GET_MODE (x);
    const int n_bytes = GET_MODE_SIZE (mode);

    if (! BINARY_P (x))
      return ai;

    rtx xop0 = XEXP (x, 0);
    rtx xop1 = XEXP (x, 1);

    // Look at shift offsets of DImode more closely;
    // they are in R16 for __lshrdi3 etc.  Patch xop1 on success.
    if (n_bytes == 8
	&& ! CONST_INT_P (xop1)
	&& GET_MODE (xop0) == mode)
      {
	const int n_off = GET_MODE_SIZE (GET_MODE (xop1));
	const absint_t aoff = absint_t::explore (xop1, memo);
	xop1 = aoff.get_value_as_const_int (n_off);
      }

    if (! xop1
	|| GET_MODE (xop0) != mode
	|| ! IN_RANGE (n_bytes, 1, FUSE_MOVE_MAX_MODESIZE)
	|| ! CONST_INT_P (xop1)
	|| ! IN_RANGE (INTVAL (xop1), 8, 8 * n_bytes - 1))
      return ai;

    const int off = INTVAL (xop1);
    const absint_t ai0 = absint_t::explore (xop0, memo);

    switch (GET_CODE (x))
      {
      default:
	break;

      case ASHIFT:
	// Shifting in 0x00's from the right.
	for (int i = 0; i < off / 8; ++i)
	  ai[i].learn_val8 (0);
	break;

      case LSHIFTRT:
      case ASHIFTRT:
	{
	  // Shifting in 0x00's or signs from the left.
	  absint_byte_t b_signs = ai0[n_bytes - 1].get_signs (GET_CODE (x));
	  for (int i = n_bytes - off / 8; i < n_bytes; ++i)
	    ai[i] = b_signs;
	  if (off == 8 * n_bytes - 1)
	    if (code == ASHIFTRT)
	      ai[0] = b_signs;
	}
	break;
      }

    if (off % 8 != 0
	|| ai0.popcount () == 0)
      return ai;

    // For shift offsets that are a multiple of 8, record the
    // action on the constituent bytes.

    // Bytes are moving left by this offset (or zero for "none").
    const int boffL = select<int>()
      : code == ROTATE || code == ASHIFT ? off / 8
      : code == ROTATERT ? n_bytes - off / 8
      : 0;

    // Bytes are moving right by this offset (or zero for "none").
    const int boffR = select<int>()
      : code == ROTATERT || code == ASHIFTRT || code == LSHIFTRT ? off / 8
      : code == ROTATE ? n_bytes - off / 8
      : 0;

    if (dump_flags & TDF_FOLDING)
      {
	avr_dump (";; AI.explore_shift %C:%m ", code, mode);
	if (boffL)
	  avr_dump ("<< %d%s", 8 * boffL, boffL && boffR ? ", " : "");
	if (boffR)
	  avr_dump (">> %d", 8 * boffR);
	avr_dump ("\n");
      }

    if (boffL)
      for (int i = 0; i < n_bytes - boffL; ++i)
	ai[i + boffL] = ai0[i];

    if (boffR)
      for (int i = boffR; i < n_bytes; ++i)
	ai[i - boffR] = ai0[i];

    return ai;
  }

  void dump (const char *msg = nullptr, FILE *f = dump_file) const
  {
    if (f)
      dump (NULL_RTX, msg, f);
  }

  void dump (rtx dest, const char *msg = nullptr, FILE *f = dump_file) const
  {
    if (f)
      {
	int regno = dest && REG_P (dest) ? REGNO (dest) : 0;

	msg = msg && msg[0] ? msg : "AI=[%s]\n";
	const char *const xs = strstr (msg, "%s");
	gcc_assert (xs);

	fprintf (f, "%.*s", (int) (xs - msg), msg);
	for (int i = max_knows (); i >= 0; --i)
	  {
	    const int sub_regno = eq[i].regno (false /*nonstrict*/);
	    const bool nop = regno &&  sub_regno == regno + i;
	    eq[i].dump (nop ? "%s=nop" : "%s", f);
	    fprintf (f, "%s", i ? "; " : "");
	  }
	fprintf (f, "%s", xs + strlen ("%s"));
      }
  }
}; // absint_t


// Information for a REG = non-memory single_set.

struct insninfo_t
{
  // This is an insn that sets the m_size bytes of m_regno to either
  // - A compile time constant m_isrc (m_code = CONST_INT), or
  // - The contents of register number m_rsrc (m_code = REG).
  int m_size = 0;
  int m_regno;
  int m_rsrc;
  rtx_code m_code;
  uint64_t m_isrc;
  rtx_insn *m_insn = nullptr;
  rtx m_set = NULL_RTX;
  rtx m_src = NULL_RTX;
  int m_scratch = 0; // 0 or the register number of a QImode scratch.
  rtx_code m_old_code = UNKNOWN;

  // Knowledge about the bytes of the SET_SRC:  A byte may have a known
  // value, may be known to equal some register (e.g. with BSWAP),
  // or both, or may be unknown.
  absint_t m_ai;

  // May be set for binary operations.
  absint_byte_t m_new_src;

  bool init1 (insn_optimize_data_t &, int max_size, const char *purpose);

  // Upper bound for the cost (in words) of a move<mode> insn that
  // performs a REG = CONST_XXX = .m_isrc move of modesize .m_size.
  int cost () const;
  bool combine (const insninfo_t &prev, const insninfo_t &curr);
  int emit_insn () const;

  bool needs_scratch () const
  {
    gcc_assert (m_code == CONST_INT);

    for (int i = 0; i < m_size; ++i)
      if (AVRasm::ldi_needs_scratch (m_regno, m_isrc >> (8 * i)))
	return true;

    return false;
  }

  int hamming (const memento_t &memo) const
  {
    gcc_assert (m_code == CONST_INT);

    int h = 0;
    for (int i = 0; i < m_size; ++i)
      h += ! memo.have_value (m_regno + i, 1, 0xff & (m_isrc >> (8 * i)));

    return h;
  }

  // Upper bound for the number of ply_t's of a solution, given Hamming
  // distance of HAMM (-1 for unknown).
  int n_best_plys (int hamm = -1) const
  {
    gcc_assert (m_code == CONST_INT);

    if (m_size == 8)
      return (hamm >= 0 ? hamm : m_size);
    else if (hamm <= 4)
      return (hamm >= 0 ? hamm : m_size)
	// The following terms is the max number of MOVWs with a
	// Hamming difference of less than 2.
	+ (AVR_HAVE_MOVW && m_regno < REG_14) * m_size / 2
	+ (AVR_HAVE_MOVW && m_regno == REG_14) * std::max (0, m_size - 2)
	- (AVR_HAVE_MOVW && hamm == 4 && (uint32_t) m_isrc % 0x10001 == 0);
    else
      gcc_unreachable ();
  }
}; // insninfo_t


struct insn_optimize_data_t
{
  // Known values held in GPRs prior to the action of .insn / .ii,
  memento_t &regs;
  rtx_insn *insn;
  insninfo_t ii;
  bool unused;

  insn_optimize_data_t () = delete;

  insn_optimize_data_t (memento_t &memo)
    : regs(memo)
  {}
}; // insn_optimize_data_t

struct optimize_data_t
{
  insn_optimize_data_t prev;
  insn_optimize_data_t curr;

  // Number >= 0 of new insns that replace the curr insn and maybe also the
  // prev insn.  -1 when no replacement has been found.
  int n_new_insns = -1;

  // .prev will be removed provided we have (potentially zero) new insns.
  bool delete_prev_p = false;

  // Ignore these GPRs when comparing the simulation results of
  // old and new insn sequences.  Usually some scratch reg(s).
  gprmask_t ignore_mask = 0;

  optimize_data_t () = delete;

  optimize_data_t (memento_t &prev_regs, memento_t &curr_regs)
    : prev(prev_regs), curr(curr_regs)
  {}

  bool try_fuse (bbinfo_t *);
  bool try_mem0 (bbinfo_t *);
  bool try_bin_arg1 (bbinfo_t *);
  bool try_simplify (bbinfo_t *);
  bool try_split_ldi (bbinfo_t *);
  bool try_split_any (bbinfo_t *);
  bool fail (const char *reason);
  bool emit_signs (int r_sign, gprmask_t);
  void emit_move_mask (int dest, int src, int n_bytes, gprmask_t &);
  rtx_insn *emit_sequence (basic_block, rtx_insn *);
  bool get_2ary_operands (rtx_code &, const absint_byte_t &,
			  insn_optimize_data_t &, int r_dest,
			  absint_val_t &, absint_val_t &, int &ex_cost);
  rtx_insn *emit_and_apply_move (memento_t &, rtx dest, rtx src);

  // M2 is the state of GPRs as the sequence starts; M1 is the state one before.
  static void apply_sequence (const std::vector<rtx_insn *> &insns,
			      memento_t &m1, memento_t &m2)
  {
    gcc_assert (insns.size () >= 1);

    for (auto &i : insns)
      {
	m1 = m2;
	m2.apply_insn (i, false);
      }
  }
}; // optimize_data_t


// Emit INSNS before .curr.insn, replacing .curr.insn and also .prev.insn when
// .delete_prev_p is on.  Adjusts .curr.regs and .prev.regs accordingly.
rtx_insn *
optimize_data_t::emit_sequence (basic_block bb, rtx_insn *insns)
{
  gcc_assert (n_new_insns >= 0);

  // The old insns will be replaced by and simulated...
  const std::vector<rtx_insn *> old_insns = delete_prev_p
    ? std::vector<rtx_insn *> { prev.insn, curr.insn }
    : std::vector<rtx_insn *> { curr.insn };

  // ...against the new insns.
  std::vector<rtx_insn *> new_insns;
  for (rtx_insn *i = insns; i; i = NEXT_INSN (i))
    new_insns.push_back (i);

  rtx_insn *new_curr_insn;

  memento_t &m1 = prev.regs;
  memento_t &m2 = curr.regs;

  if (new_insns.empty ())
    {
      if (delete_prev_p)
	{
	  m2 = m1;
	  m1.known = 0;
	  new_curr_insn = prev_nondebug_insn_bb (bb, prev.insn);
	}
      else
	new_curr_insn = prev.insn;
    }
  else
    {
      // We are going to emit at least one new insn.  Simulate the effect of
      // the new sequence and compare it against the effect of the old one.
      // Both effects must be the same (modulo scratch regs).

      memento_t n1 = m1;
      memento_t n2 = m2;

      if (delete_prev_p)
	{
	  m2 = m1, m1.known = 0;
	  n2 = n1, n1.known = 0;
	}

      avr_dump (";; Applying new route...\n");
      optimize_data_t::apply_sequence (new_insns, n1, n2);

      avr_dump (";; Applying old route...\n");
      optimize_data_t::apply_sequence (old_insns, m1, m2);
      avr_dump ("\n");

      if (! m2.equals (n2, ignore_mask))
	{
	  // When we come here, then
	  // - We have a genuine bug, and/or
	  // - We did produce insns that are opaque to absint_t's explore().
	  avr_dump ("INCOMPLETE APPLICATION:\n");
	  m2.dump ("regs old route=%s\n\n");
	  n2.dump ("regs new route=%s\n\n");
	  avr_dump ("The new insns are:\n%L", insns);

	  fatal_insn ("incomplete application of insn", insns);
	}

      // Use N1 and N2 as the new GPR states.  Even though they are equal
      // modulo ignore_mask, N2 may know more about GPRs when it doesn't
      // clobber the scratch reg.
      m1 = n1;
      m2 = n2;

      emit_insn_before (insns, curr.insn);

      new_curr_insn = new_insns.back ();
    }

  if (delete_prev_p)
    SET_INSN_DELETED (prev.insn);

  SET_INSN_DELETED (curr.insn);

  return new_curr_insn;
}


const pass_data avr_pass_data_fuse_move =
{
  RTL_PASS,	 // type
  "",		 // name (will be patched)
  OPTGROUP_NONE, // optinfo_flags
  TV_MACH_DEP,	 // tv_id
  0,		 // properties_required
  0,		 // properties_provided
  0,		 // properties_destroyed
  0,		 // todo_flags_start
  TODO_df_finish | TODO_df_verify // todo_flags_finish
};


class avr_pass_fuse_move : public rtl_opt_pass
{
public:
  avr_pass_fuse_move (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_fuse_move, ctxt)
  {
    this->name = name;
  }

  unsigned int execute (function *func) final override
  {
    if (optimize > 0 && avropt_fuse_move > 0)
      {
	df_note_add_problem ();
	df_analyze ();

	bbinfo_t::optimize_one_function (func);
      }

    return 0;
  }
}; // avr_pass_fuse_move


// Append PLY to .plies[].  A SET or BLD ply may start a new sequence of
// SETs or BLDs and gets assigned the overhead of the sequence like for an
// initial SET or CLT instruction.  A SET ply my be added in two flavours:
// One that starts a sequence of single_sets, and one that represents the
// payload of a set_some insn.  MEMO is the GPR state prior to PLY.
void
plies_t::add (ply_t ply, const ply_t *prev, const memento_t &memo,
	      bool maybe_set_some)
{
  if (ply.code == SET)
    {
      if (prev && prev->code == SET)
	{
	  // Proceed with the SET sequence flavour.
	  ply.in_set_some = prev->in_set_some;

	  if (ply.in_set_some)
	    ply.scratch = 0;
	  else if (! ply.scratch && ply.needs_scratch ())
	    ply.cost += 2;
	}
      else
	{
	  // The 1st SET in a sequence.  May use set_some to set
	  // all bytes in one insn, or a bunch of single_sets.

	  // Route1: Bunch of single_sets.
	  const int ply_cost = ply.cost;
	  if (! ply.scratch && ply.needs_scratch ())
	    ply.cost += 2;
	  ply.in_set_some = false;

	  add (ply);

	  if (maybe_set_some)
	    {
	      // Route 2: One set_some: The 1st SET gets all the overhead.
	      ply.scratch = 0;
	      ply.cost = ply_cost + 1 + ! memo.known_dregno ();
	      ply.in_set_some = true;
	    }
	}
    } // SET
  else if (ply.is_bld ())
    {
      // The first BLD in a series of BLDs gets the extra costs
      // for the SET / CLT that precedes the BLDs.
      ply.cost += ! ply.is_same_bld (prev);
    }

  add (ply);
}


// Emit insns for .plies[] and return the number of emitted insns.
// The emitted insns represent the effect of II with MEMO, which
// is the GPR knowledge before II is executed.
int
plies_t::emit_insns (const insninfo_t &ii, const memento_t &memo) const
{
  int n_insns = 0;

  for (int i = 0; i < n_plies; ++i)
    {
      const ply_t &p = plies[i];

      // SETs and BLDs are dumped by their emit_xxxs().
      if (p.code != SET && ! p.is_bld ())
	p.dump ();

      rtx src1 = NULL_RTX;
      rtx src2 = NULL_RTX;
      rtx dest = NULL_RTX;
      rtx xscratch = NULL_RTX;
      rtx_code code = p.code;

      switch (p.code)
	{
	default:
	  avr_dump ("\n\n;; Bad ply_t:\n");
	  p.dump (i + 1);
	  gcc_unreachable ();
	  break;

	case REG: // *movhi = MOVW; movqi_insn = MOV
	  dest = gen_rtx_REG (p.size == 1 ? QImode : HImode, p.regno);
	  src1 = gen_rtx_REG (p.size == 1 ? QImode : HImode, p.arg);
	  break;

	case SET: // movqi_insn = LDI, CLR; set_some = (LDI + MOV) ** size.
	  i += emit_sets (ii, n_insns, memo, i) - 1;
	  continue;

	case MOD: // *ior<mode>3, *and<mode>3 = SET + BLD... / CLT + BLD...
	  i += emit_blds (ii, n_insns, i) - 1;
	  continue;

	case MINUS: // *subqi3 = SUB
	case PLUS:  // *addqi3 = ADD
	case AND: // *andqi3 = AND
	case IOR: // *iorqi3 = OR
	case XOR: // *xorqi3 = EOR
	  dest = gen_rtx_REG (QImode, p.regno);
	  src2 = gen_rtx_REG (QImode, p.arg);
	  break;

	case PRE_INC: // *addqi3 = INC
	case PRE_DEC: // *addqi3 = DEC
	  code = PLUS;
	  dest = gen_rtx_REG (QImode, p.regno);
	  src2 = p.code == PRE_INC ? const1_rtx : constm1_rtx;
	  break;

	case NEG: // *negqi2 = NEG
	case NOT: // *one_cmplqi2 = COM
	  dest = gen_rtx_REG (QImode, p.regno);
	  src1 = dest;
	  break;

	case ROTATE:   // *rotlqi3 = SWAP
	case ASHIFT:   // *ashlqi3 = LSL
	case ASHIFTRT: // *ashrqi3 = ASR
	case LSHIFTRT: // *lshrqi3 = LSR
	  dest = gen_rtx_REG (QImode, p.regno);
	  src2 = GEN_INT (code == ROTATE ? 4 : 1);
	  break;

	case SS_PLUS: // *addhi3 = ADIW, SBIW
	  code = PLUS;
	  dest = gen_rtx_REG (HImode, p.regno);
	  src2 = gen_int_mode (p.arg, HImode);
	  break;
	} // switch p.code

      gcc_assert (dest && (! src1) + (! src2) == 1);

      rtx src = code == REG || code == SET
	? src1
	: (src2
	   ? gen_rtx_fmt_ee (code, GET_MODE (dest), dest, src2)
	   : gen_rtx_fmt_e (code, GET_MODE (dest), src1));

      emit_valid_move_clobbercc (dest, src, xscratch);
      n_insns += 1;
    }

  return n_insns;
}


// Helper for .emit_insns().  Emit an ior<mode>3 or and<mode>3 insn
// that's equivalent to a sequence of contiguous BLDs starting at
// .plies[ISTART].  Updates N_INSNS according to the number of insns
// emitted and returns the number of consumed plys in .plies[].
int
plies_t::emit_blds (const insninfo_t &ii, int &n_insns, int istart) const
{
  const ply_t &first = plies[istart];

  gcc_assert (ii.m_size <= 4);
  gcc_assert (first.is_bld ());

  const rtx_code code = first.is_setbld () ? IOR : AND;
  const machine_mode mode = size_to_mode (ii.m_size);

  // Determine mask and number of BLDs.

  uint32_t mask = 0;
  int n_blds = 0;

  for (int i = istart; i < n_plies; ++i, ++n_blds)
    {
      const ply_t &p = plies[i];
      if (! p.is_bld () || ! p.is_same_bld (& first))
	break;

      // For AND, work on the 1-complement of the mask,
      // i.e. 1's specify which bits to clear.
      uint8_t mask8 = code == IOR ? p.arg : ~p.arg;
      mask |= mask8 << (8 * (p.regno - ii.m_regno));
    }

  mask = GET_MODE_MASK (mode) & (code == IOR ? mask : ~mask);

  if (dump_file)
    {
      fprintf (dump_file, ";; emit_blds[%d...%d] R%d[%d]%s=%0*x\n",
	       istart, istart + n_blds - 1, ii.m_regno, ii.m_size,
	       code == IOR ? "|" : "&", 2 * ii.m_size, (int) mask);
    }

  for (int i = 0; i < n_blds; ++i)
    plies[i + istart].dump ();

  rtx dest = gen_rtx_REG (mode, ii.m_regno);
  rtx src = gen_rtx_fmt_ee (code, mode, dest, gen_int_mode (mask, mode));
  rtx xscratch = mode == QImode ? NULL_RTX : gen_rtx_SCRATCH (QImode);

  emit_valid_move_clobbercc (dest, src, xscratch);
  n_insns += 1;

  return n_blds;
}


// Emit insns for a contiguous sequence of SET ply_t's starting at
// .plies[ISTART].  Advances N_INSNS by the number of emitted insns.
// MEMO ist the state of the GPRs before II is executed, where II
// represents the insn under optimization.
// The emitted insns are "movqi_insn" or "*reload_inqi"
// when .plies[ISTART].in_set_some is not set, and one "set_some" insn
// when .plies[ISTART].in_set_some is set.
int
plies_t::emit_sets (const insninfo_t &ii, int &n_insns, const memento_t &memo,
		    int istart) const
{
  gcc_assert (plies[istart].code == SET);

  const bool in_set_some = plies[istart].in_set_some;

  // Some d-regno that holds a compile-time constant, or 0.
  const int known_dregno = memo.known_dregno ();

  // Determine number of contiguous SETs,
  // and sort them in ps[] such that smaller regnos come first.

  const ply_t *ps[FUSE_MOVE_MAX_MODESIZE];
  int n_sets = 0;

  for (int i = istart; i < n_plies && plies[i].code == SET; ++i)
    ps[n_sets++] = & plies[i];

  if (dump_file)
    {
      fprintf (dump_file, ";; emit_sets[%d...%d] R%d[%d]=%0*" PRIx64,
	       istart, istart + n_sets - 1, ii.m_regno, ii.m_size,
	       2 * ii.m_size, ii.m_isrc);
      fprintf (dump_file, ", scratch=%s%d", "R" + ! ii.m_scratch, ii.m_scratch);
      fprintf (dump_file, ", known_dreg=%s%d, set_some=%d\n",
	       "R" + ! known_dregno, known_dregno, in_set_some);
    }

  for (int i = 0; i < n_sets; ++i)
    ps[i]->dump ();

  // Sort.  This is most useful on regs like (reg:SI REG_14).
  for (int i = 0; i < n_sets - 1; ++i)
    for (int j = i + 1; j < n_sets; ++j)
      if (ps[i]->regno > ps[j]->regno)
	std::swap (ps[i], ps[j]);

  // Prepare operands.
  rtx dst[FUSE_MOVE_MAX_MODESIZE];
  rtx src[FUSE_MOVE_MAX_MODESIZE];
  for (int i = 0; i < n_sets; ++i)
    {
      dst[i] = gen_rtx_REG (QImode, ps[i]->regno);
      src[i] = gen_int_mode (ps[i]->arg, QImode);
    }

  if (in_set_some)
    {
      // Emit a "set_some" insn that sets all of the collected 8-bit SETs.
      // This is a parallel with n_sets QImode SETs as payload.

      gcc_assert (! known_dregno || memo.knows (known_dregno));

      // A scratch reg...
      rtx op1 = known_dregno
	? gen_rtx_REG (QImode, known_dregno)
	: const0_rtx;
      // ...with a known content, so it can be restored without saving.
      rtx op2 = known_dregno
	? gen_int_mode (memo.values[known_dregno], QImode)
	: const0_rtx;
      // Target register envelope.
      rtx op3 = GEN_INT (ii.m_regno);
      rtx op4 = GEN_INT (ii.m_size);

      // Payload.
      for (int i = 0; i < n_sets; ++i)
	dst[i] = gen_rtx_SET (dst[i], src[i]);

      rtvec vec = gen_rtvec (5 + n_sets,
			     gen_rtx_USE (VOIDmode, op1),
			     gen_rtx_USE (VOIDmode, op2),
			     gen_rtx_USE (VOIDmode, op3),
			     gen_rtx_USE (VOIDmode, op4),
			     gen_rtx_CLOBBER (VOIDmode, cc_reg_rtx),
			     dst[0], dst[1], dst[2], dst[3]);
      rtx pattern = gen_rtx_PARALLEL (VOIDmode, vec);

      emit_valid_insn (pattern);
      n_insns += 1;
    }
  else
    {
      // Emit a bunch of movqi_insn / *reload_inqi insns.

      for (int i = 0; i < n_sets; ++i)
	if (ii.m_scratch
	    && AVRasm::constant_cost (SET, ps[i]->regno, ps[i]->arg) > 1)
	  {
	    rtx scratch = gen_rtx_REG (QImode, ii.m_scratch);
	    bool use_reload_inqi = true;
	    if (use_reload_inqi)
	      {
		emit_valid_move_clobbercc (dst[i], src[i], scratch);
		n_insns += 1;
	      }
	    else
	      {
		emit_valid_move_clobbercc (scratch, src[i]);
		emit_valid_move_clobbercc (dst[i], scratch);
		n_insns += 2;
	      }
	  }
	else
	  {
	    emit_valid_move_clobbercc (dst[i], src[i]);
	    n_insns += 1;
	  }
    }

  return n_sets;
}


// Try to find an operation such that  Y = op (X).
// Shifts and rotates are regarded as unary operaions with
// an implied 2nd operand or 1 or 4, respectively.
static rtx_code
find_arith (uint8_t y, uint8_t x)
{
#define RETIF(ex, code) y == (0xff & (ex)) ? code
  return select<rtx_code>()
    : RETIF (x + 1, PRE_INC)
    : RETIF (x - 1, PRE_DEC)
    : RETIF ((x << 4) | (x >> 4), ROTATE)
    : RETIF (-x, NEG)
    : RETIF (~x, NOT)
    : RETIF (x >> 1, LSHIFTRT)
    : RETIF (x << 1, ASHIFT)
    : RETIF ((x >> 1) | (x & 0x80), ASHIFTRT)
    : UNKNOWN;
#undef RETIF
}


// Try to find an operation such that  Z = X op X.
static rtx_code
find_arith2 (uint8_t z, uint8_t x, uint8_t y)
{
#define RETIF(ex, code) z == (0xff & (ex)) ? code
  return select<rtx_code>()
    : RETIF (x + y, PLUS)
    : RETIF (x - y, MINUS)
    : RETIF (x & y, AND)
    : RETIF (x | y, IOR)
    : RETIF (x ^ y, XOR)
    : UNKNOWN;
#undef RETIF
}


// Add plies to .plies[] that represent a MOVW, but only ones that reduce
// the Hamming distance from REGNO[SIZE] to VAL by exactly DHAMM.
void
plies_t::add_plies_movw (int regno, int size, uint64_t val,
			 int dhamm, const memento_t &memo)
{
  if (! AVR_HAVE_MOVW || size < 2)
    return;

  for (int i = 0; i < size - 1; i += 2)
    {
      // MOVW that sets less than 2 regs to the target value is
      // not needed for the upper regs.
      if (dhamm != 2 && regno + i >= REG_16)
	continue;

      const uint16_t val16 = val >> (8 * i);
      const uint8_t lo8 = val16;
      const uint8_t hi8 = val16 >> 8;

      // When one of the target bytes is already as expected, then
      // no MOVW is needed for an optimal sequence.
      if (memo.have_value (regno + i, 1, lo8)
	  || memo.have_value (regno + i + 1, 1, hi8))
	continue;

      const int h_old = memo.hamming (regno + i, 2, val16);

      // Record MOVWs that reduce the Hamming distance by DHAMM as requested.
      for (int j = FIRST_GPR; j < REG_32; j += 2)
	if (j != regno + i
	    && memo.knows (j, 2))
	  {
	    const int h_new = memo.hamming (j, 2, val16);
	    if (h_new == h_old - dhamm)
	      add (ply_t { regno + i, 2, REG, j, 1, dhamm });
	  }
    }
}


// Set PS to plys that reduce the Hamming distance from II.m_regno to
// compile-time constant II.m_isrc by 2, 1 or 0.  PREV is NULL or points
// to a previous ply_t.  MEMO is the GPR state after PREV and prior to the
// added plys.
void
bbinfo_t::get_plies (plies_t &ps, const insninfo_t &ii, const memento_t &memo,
		     const ply_t *prev)
{
  ps.reset ();

  fpd->n_get_plies += 1;

  const bool maybe_set_some = (bbinfo_t::use_set_some_p && ii.needs_scratch ());

  // Start with cheap plies, then continue to more expensive ones.
  const int regno = ii.m_regno;
  const int size = ii.m_size;
  const uint64_t val = ii.m_isrc;

  // Find MOVW with a Hamming delta of 2.
  ps.add_plies_movw (regno, size, val, 2, memo);

  // Find ADIW / SBIW
  if (AVR_HAVE_ADIW && size >= 2)
    for (int i = 0; i < size - 1; i += 2)
      if (regno + i >= REG_24
	  && memo.knows (regno + i, 2))
	{
	  const int16_t value16 = memo[regno + i] + 256 * memo[regno + i + 1];
	  const int16_t lo16 = val >> (8 * i);
	  const int16_t delta = lo16 - value16;
	  const uint8_t lo8 = val >> (8 * i);
	  const uint8_t hi8 = val >> (8 * i + 8);
	  if (IN_RANGE (delta, -63, 63)
	      && lo8 != memo[regno + i]
	      && hi8 != memo[regno + i + 1])
	    {
	      ps.add (ply_t { regno + i, 2, SS_PLUS, delta, 1, 2 });
	    }
	}

  // Find 1-reg plies.  In an optimal sequence, each 1-reg ply will decrease
  // the Hamming distance.  Thus we only have to consider plies that set
  // one of the target bytes to the target value VAL.  Start with the
  // high registers since that is the canonical order when two plies commute.

  for (int i = size - 1; i >= 0; --i)
    {
      const uint8_t val8 = val >> (8 * i);

      // Nothing to do for this byte when its value is already as desired.
      if (memo.have_value (regno + i, 1, val8))
	continue;

      // LDI or CLR.
      if (regno + i >= REG_16 || val8 == 0)
	ps.add (ply_t { regno + i, 1, SET, val8, 1 }, prev, memo,
		maybe_set_some);

      // We only may need to MOV non-zero values since there is CLR,
      // and only when there is no LDI.
      if (val8 != 0
	  && regno + i < REG_16)
	{
	  // MOV where the source register is one of the target regs.
	  for (int j = 0; j < size; ++j)
	    if (j != i)
	      if (memo.have_value (regno + j, 1, val8))
		ps.add (ply_t { regno + i, 1, REG, regno + j, 1 });

	  // MOV where the source register is not a target reg.
	  // FIXME: ticks.
	  for (int j = FIRST_GPR; j < REG_32; ++j)
	    if (! IN_RANGE (j, regno, regno + size - 1))
	      if (memo.have_value (j, 1, val8))
		ps.add (ply_t { regno + i, 1, REG, j, 1 });

	  // LDI + MOV.
	  if (regno + i < REG_16 && val8 != 0)
	    {
	      ply_t p { regno + i, 1, SET, val8, 2 };
	      p.scratch = ii.m_scratch;
	      ps.add (p, prev, memo, maybe_set_some);
	    }
	}
    }

  // Arithmetic like INC, DEC or ASHIFT.
  for (int i = size - 1; i >= 0; --i)
    if (bbinfo_t::use_arith_p
	&& regno + i < REG_16
	&& memo.knows (regno + i))
      {
	const uint8_t y = val >> (8 * i);
	const uint8_t x = memo[regno + i];
	rtx_code code;

	if (y == 0 || y == x)
	  continue;

	// INC, DEC, SWAP, LSL, NEG, ...
	if (UNKNOWN != (code = find_arith (y, x)))
	  {
	    ps.add (ply_t { regno + i, 1, code, x /* dummy */, 1 });
	    continue;
	  }

	// ADD, AND, ...
	for (int r = FIRST_GPR; r < REG_32; ++r)
	  if (r != regno + i
	      && memo.knows (r)
	      && memo[r] != 0
	      && UNKNOWN != (code = find_arith2 (y, x, memo[r])))
	    {
	      ps.add (ply_t { regno + i, 1, code, r, 1 });
	    }

	if (size < 2 || size > 4)
	  continue;

	// SET + BLD
	if ((x & y) == x && popcount_hwi (x ^ y) == 1)
	  ps.add (ply_t { regno + i, 1, MOD, x ^ y, 1 },
		  prev, memo, maybe_set_some);

	// CLT + BLD
	if ((x & y) == y && popcount_hwi (x ^ y) == 1)
	  ps.add (ply_t { regno + i, 1, MOD, x ^ y ^ 0xff, 1 },
		  prev, memo, maybe_set_some);
      }

  if (bbinfo_t::use_arith_p
      // For 8-byte values, don't use ply_t's with only a partial reduction
      // of the hamming distance.
      && size <= 4)
    {
      // Find MOVW with a Hamming delta of 1, then 0.
      ps.add_plies_movw (regno, size, val, 1, memo);
      ps.add_plies_movw (regno, size, val, 0, memo);
    }

  plies_t::max_n_plies = std::max (plies_t::max_n_plies, ps.n_plies);
}


// Try to combine two 8-bit insns PREV and CURR that (effectively)
// are REG = CONST_INT to one 16-bit such insn.  Returns true on success.
bool
insninfo_t::combine (const insninfo_t &prev, const insninfo_t &curr)
{
  if (prev.m_size == 1 && curr.m_size == 1
      && prev.m_regno == (1 ^ curr.m_regno)
      && curr.m_code == CONST_INT
      && prev.m_code == CONST_INT)
    {
      m_regno = curr.m_regno & ~1;
      m_code = CONST_INT;
      m_size = 2;
      m_scratch = std::max (curr.m_scratch, prev.m_scratch);
      m_isrc = m_regno == prev.m_regno
	? (uint8_t) prev.m_isrc + 256 * (uint8_t) curr.m_isrc
	: (uint8_t) curr.m_isrc + 256 * (uint8_t) prev.m_isrc;

      return true;
    }

  return false;
}


// Return the cost (in terms of words) of the respective mov<mode> insn.
// This can be used as an upper bound for the ply_t's cost.
int
insninfo_t::cost () const
{
  if (m_code != CONST_INT)
    return m_size;

  if (m_regno >= REG_16 || m_isrc == 0)
    return m_size
      // MOVW can save one instruction.
      - (AVR_HAVE_MOVW && m_size == 4 && (uint32_t) m_isrc % 0x10001 == 0);

  // LDI + MOV to a lower reg.
  if (m_scratch && m_size == 1)
    return 2;

  if (m_size == 8)
    {
      int len = m_size;
      for (int i = 0; i < m_size; ++i)
	len += m_regno + i < REG_16 && (0xff & (m_isrc >> (8 * i))) != 0;
      return len;
    }

  // All other cases are complicated.  Ask the output oracle.
  const machine_mode mode = size_to_mode (m_size);
  rtx xscratch = m_scratch ? all_regs_rtx[m_scratch] : NULL_RTX;
  rtx xop[] = { gen_rtx_REG (mode, m_regno), gen_int_mode (m_isrc, mode) };
  int len;
  if (m_size == 4)
    output_reload_insisf (xop, xscratch, &len);
  else
    output_reload_in_const (xop, xscratch, &len, false);

  return len;
}

// Emit the according REG = REG-or-CONST_INT insn.  Returns 1 or aborts
// when the insn is not of that form.
int
insninfo_t::emit_insn () const
{
  int n_insns = 0;

  machine_mode mode = size_to_mode (m_size);
  rtx xsrc = NULL_RTX;
  rtx xscratch = NULL_RTX;

  gcc_assert (m_size > 0);

  switch (m_code)
    {
    default:
      gcc_unreachable ();

    case CONST_INT:
      xsrc = gen_int_mode (m_isrc, mode);
      if (m_scratch && m_regno < REG_16)
	xscratch = gen_rtx_REG (QImode, m_scratch);
      break;

    case REG:
      gcc_assert (gpr_regno_p (m_rsrc, m_size));
      if (m_regno != m_rsrc)
	xsrc = gen_rtx_REG (mode, m_rsrc);
      break;
    }

  if (xsrc)
    {
      rtx dest = gen_rtx_REG (mode, m_regno);
      emit_valid_move_clobbercc (dest, xsrc, xscratch);
      n_insns += 1;
    }

  return n_insns;
}


// Entering a basic block means combining known register values from
// all incoming BBs.
void
bbinfo_t::enter ()
{
  avr_dump ("\n;; Entering [bb %d]\n", bb->index);

  gcc_assert (! done);

  edge e;
  edge_iterator ei;
  gprmask_t pred_known_mask = ~0u;
  bbinfo_t *bbi = nullptr;

  // A quick iteration over all predecessors / incoming edges to reveal
  // whether this BB is worth a closer look.
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      basic_block pred = e->src;
      bbi = & bb_info[pred->index];

      pred_known_mask &= bbi->regs.known;

      if (dump_file)
	{
	  avr_dump (";; [bb %d] <- [bb %d] ", e->dest->index, e->src->index);
	  if (bbi->done)
	    bbi->regs.dump ();
	  else
	    avr_dump (" (unknown)\n");
	}
    }

  // Only if all predecessors have already been handled, we can
  // have known values as we are entering the current BB.
  if (pred_known_mask != 0
      && bbi != nullptr)
    {
      // Initialize current BB info from BI, an arbitrary predecessor.

      regs = bbi->regs;

      // Coalesce the output values from all predecessing BBs.  At the
      // start of the current BB, a value is only known if it is known
      // in *all* predecessors and *all* these values are the same.
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  regs.coalesce (bb_info[e->src->index].regs);
	}
    }

  if (dump_file)
    {
      avr_dump (";; [bb %d] known at start: ", bb->index);
      if (regs.known)
	regs.dump ();
      else
	avr_dump (" (none)\n");
      avr_dump ("\n");
    }
}


void
bbinfo_t::leave ()
{
  done = true;

  if (dump_file)
    fprintf (dump_file, ";; Leaving [bb %d]\n\n", bb->index);
}


/* Initialize according to INSN which is a 1-byte single_set that's
   (effectively) a reg = reg or reg = const move.  INSN may be the result
   of the current pass's optimization, e.g. something like INC R2 where R2
   has a known content.  MEMO is the state prior to INSN.  Only CONST
   cases are recorded; plus cases that are non-trivial for example when
   an XOR decays to a move.  */

bool
insninfo_t::init1 (insn_optimize_data_t &iod, int max_size,
		   const char *purpose = "")
{
  m_size = 0;
  m_insn = iod.insn;
  m_old_code = UNKNOWN;
  iod.unused = false;

  if (! iod.insn
      || ! (m_set = single_set_with_scratch (iod.insn, m_scratch)))
    return false;

  rtx dest = SET_DEST (m_set);
  machine_mode mode = GET_MODE (dest);
  const int n_bytes = GET_MODE_SIZE (mode);
  max_size = std::min (max_size, FUSE_MOVE_MAX_MODESIZE);

  if (! REG_P (dest)
      || END_REGNO (dest) > REG_32
      || n_bytes > max_size)
    return false;

  // Omit insns that (explicitly) touch fixed GPRs in any way.
  using elt0_getter_HRS = elt0_getter<HARD_REG_SET, HARD_REG_ELT_TYPE>;
  HARD_REG_SET hregs;
  CLEAR_HARD_REG_SET (hregs);
  find_all_hard_regs (PATTERN (iod.insn), & hregs);
  if (memento_t::fixed_regs_mask & (gprmask_t) elt0_getter_HRS::get (hregs))
    {
      avr_dump (";; %sinit1 has fixed GPRs\n", purpose);
      return false;
    }

  if ((iod.unused = find_reg_note (iod.insn, REG_UNUSED, dest)))
    return false;

  m_src = SET_SRC (m_set);
  m_regno = REGNO (dest);
  const rtx_code src_code = GET_CODE (m_src);

  m_ai = absint_t::explore (m_src, iod.regs, mode);

  if (m_ai.popcount ())
    {
      if (m_ai.end_knows (CONST_INT) >= n_bytes)
	{
	  m_code = CONST_INT;
	  m_old_code = CONSTANT_P (m_src) ? UNKNOWN : src_code;
	  m_isrc = m_ai.get_value (n_bytes);
	  m_size = n_bytes;
	}
      else if (! REG_P (m_src)
	       && n_bytes == 1
	       && m_ai.end_knows (REG) >= n_bytes)
	{
	  m_code = REG;
	  m_old_code = src_code;
	  m_rsrc = m_ai[0].regno ();
	  m_size = n_bytes;
	}
      else if (n_bytes == 1)
	{
	  absint_byte_t &aib = m_new_src;
	  aib = m_ai[0].find_alternative_binary (iod.regs);

	  if (aib.arity () == 2
	      && aib.arg (0).regno == m_regno)
	    {
	      m_old_code = src_code;
	      m_code = aib.get_code ();
	      m_size = n_bytes;
	    }
	}
      else if (n_bytes >= 2
	       && m_ai.end_knows (VALUE) >= n_bytes)
	{
	  m_code = src_code;
	  m_size = n_bytes;
	}

      if (dump_file && m_size != 0)
	{
	  avr_dump (";; %sinit1 (%C", purpose,
		    m_old_code ? m_old_code : m_code);
	  if (m_old_code)
	    avr_dump ("-> %C", m_code);
	  avr_dump (") insn %d to R%d[%d] := %C:%m = ", INSN_UID (iod.insn),
		    m_regno, n_bytes, src_code, mode);

	  m_ai.dump (dest);

	  if (dump_flags & TDF_FOLDING)
	    avr_dump ("\n");
	}
    }

  return m_size != 0;
}


// The private worker for .apply_insn().
void
memento_t::apply_insn1 (rtx_insn *insn, bool unused)
{
  gcc_assert (NONDEBUG_INSN_P (insn));

  if (INSN_CODE (insn) == CODE_FOR_set_some)
    {
      // This insn only sets some selected bytes of register $3 of
      // modesize $4.  If non-0, then $1 is a QImode scratch d-reg with
      // a known value of $2.

      const auto &xop = recog_data.operand;
      extract_insn (insn);
      gcc_assert (recog_data.n_operands == 7);
      gcc_assert (set_some_operation (xop[0], VOIDmode));

      const rtx &xscratch = xop[1];
      const rtx &xscratch_value = xop[2];
      const int sets_start = 5;

      for (int i = sets_start; i < XVECLEN (xop[0], 0); ++i)
	{
	  rtx xset = XVECEXP (xop[0], 0, i);
	  avr_dump (";; set_some %r = %r\n", XEXP (xset, 0), XEXP (xset, 1));
	  set_values (XEXP (xset, 0), XEXP (xset, 1));
	}

      if (REG_P (xscratch))
	{
	  avr_dump (";; set_some %r = %r restore\n", xscratch, xscratch_value);
	  set_values (xscratch, xscratch_value);
	}

      return;
    } // CODE_FOR_set_some

  memento_t mold = *this;

  // When insn changes a register in whatever way, set it to "unknown".

  HARD_REG_SET rset;
  find_all_hard_reg_sets (insn, &rset, true /* implicit */);
  (*this) &= ~rset;

  rtx set = single_set (insn);
  rtx dest;

  if (! set
      || ! REG_P (dest = SET_DEST (set))
      || END_REGNO (dest) > REG_32
      || (regmask (dest) & memento_t::fixed_regs_mask))
    return;

  rtx src = SET_SRC (set);
  const rtx_code src_code = GET_CODE (src);
  const machine_mode mode = GET_MODE (dest);
  const int n_bytes = GET_MODE_SIZE (mode);

  // Insns that are too complicated or have a poor yield.
  // Just record which regs are clobberd / changed.
  if (n_bytes > FUSE_MOVE_MAX_MODESIZE
      || MEM_P (src)
      || (REG_P (src) && END_REGNO (src) > REG_32))
    {
      // Comparisons may clobber the compared reg when it is unused after.
      if (src_code == COMPARE
	  && REG_P (XEXP (src, 0))
	  && CONSTANT_P (XEXP (src, 1)))
	{
	  rtx reg = XEXP (src, 0);
	  for (unsigned r = REGNO (reg); r < END_REGNO (reg); ++r)
	    set_unknown (r);
	}
      return;
    }

  if (unused)
    return;

  // Simulate the effect of some selected insns that are likely to produce
  // or propagate known values.

  // Get an abstract representation of src.  Bytes may be unknown,
  // known to equal some 8-bit compile-time constant (CTC) value,
  // or are known to equal some 8-bit register.
  // TODO: Currently, only the ai[].val8 knowledge ist used.
  //       What's the best way to make use of ai[].regno ?

  absint_t ai = absint_t::explore (src, mold, mode);

  if (ai.popcount ())
    {
      avr_dump (";; apply_insn %d R%d[%d] := %C:%m = ", INSN_UID (insn),
		REGNO (dest), n_bytes, src_code, mode);
      ai.dump ();

      for (int i = 0; i < n_bytes; ++i)
	if (ai[i].can (CONST_INT))
	  set_value (i + REGNO (dest), ai[i].val8 ());
    }
}


void
memento_t::apply (const ply_t &p)
{
  if (p.is_movw ())
    {
      copy_value (p.regno, p.arg);
      copy_value (p.regno + 1, p.arg + 1);
    }
  else if (p.is_adiw ())
    {
      int val = p.arg + values[p.regno] + 256 * values[1 + p.regno];
      set_value (p.regno, val);
      set_value (p.regno + 1, val >> 8);
    }
  else if (p.size == 1)
    {
      int x = values[p.regno];
      int y = values[p.arg];

      switch (p.code)
	{
	default:
	  gcc_unreachable ();
	  break;

	case REG:
	  copy_value (p.regno, p.arg);
	  break;

	case SET:
	  set_value (p.regno, p.arg);
	  if (p.scratch >= REG_16)
	    set_unknown (p.scratch);
	  break;

	case MOD: // BLD
	  gcc_assert (knows (p.regno));
	  if (popcount_hwi (p.arg) == 1)
	    values[p.regno] |= p.arg;
	  else if (popcount_hwi (p.arg) == 7)
	    values[p.regno] &= p.arg;
	  else
	    gcc_unreachable ();
	  break;

#define DO_ARITH(n_args, code, expr)					\
	  case code:							\
	    gcc_assert (knows (p.regno));				\
	    if (n_args == 2)						\
	      gcc_assert (knows (p.arg));				\
	    set_value (p.regno, expr);					\
	    break

	  DO_ARITH (1, NEG, -x);
	  DO_ARITH (1, NOT, ~x);
	  DO_ARITH (1, PRE_INC, x + 1);
	  DO_ARITH (1, PRE_DEC, x - 1);
	  DO_ARITH (1, ROTATE, (x << 4) | (x >> 4));
	  DO_ARITH (1, ASHIFT, x << 1);
	  DO_ARITH (1, LSHIFTRT, x >> 1);
	  DO_ARITH (1, ASHIFTRT, (x >> 1) | (x & 0x80));

	  DO_ARITH (2, AND, x & y);
	  DO_ARITH (2, IOR, x | y);
	  DO_ARITH (2, XOR, x ^ y);
	  DO_ARITH (2, PLUS, x + y);
	  DO_ARITH (2, MINUS, x - y);
#undef DO_ARITH
	}
    } // size == 1
  else
    gcc_unreachable ();
}


// Try to find a sequence of ply_t's that represent a II.m_regno = II.m_isrc
// insn that sets a reg to a compile-time constant, and that is more
// efficient than just a move insn.  (When try_split_any_p is on, then
// solutions that perform equal to a move insn are also allowed).
// MEMO0 is the GPR state before II runs.  A solution has been found
// when .fpd->solution has at least one entry.  LEN specifies the
// depth of recursion, which works on the LEN-th ply_t.
void
bbinfo_t::find_plies (int len, const insninfo_t &ii, const memento_t &memo0)
{
  if (len > fpd->n_best_plys)
    return;

  memento_t memo = memo0;
  bool ply_applied_p = false;

  //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  const bool extra = dump_file && (dump_flags & TDF_FOLDING);

  if (extra)
    {
      fprintf (dump_file, ";; #%d (HAM=%d): get_plies R%d[%d] = ", len,
	       ii.hamming (fpd->regs0), ii.m_regno, ii.m_size);
      fprintf (dump_file, "0x%0*" PRIx64 "\n",
	       2 * ii.m_size, ii.m_isrc & size_to_mask (ii.m_size));
    }

  plies_t &ps = fpd->plies[len - 1];

  const ply_t *const prev = len >= 2 ? fpd->ply_stack[len - 2] : nullptr;
  const ply_t *const prev2 = len >= 3 ? fpd->ply_stack[len - 3] : nullptr;

  bbinfo_t::get_plies (ps, ii, memo0, prev);

#define NEXT(reason)					\
  do {							\
    if (extra)						\
      fprintf (dump_file, ";; cont=%s\n", reason);	\
    goto next;						\
  } while (0)

  for (int ip = 0; ip < ps.n_plies; ++ip)
    {
      const ply_t &p = ps.plies[ip];

      fpd->ply_stack[len - 1] = &p;

      if (0)
	next: continue;

      if (extra)
	ply_t::dump_plys (dump_file, len, 1, fpd->ply_stack + len - 1, memo0);

      // A MOVW with a Hamming distance of < 2 requires more plys.
      if (p.is_movw () && len + (2 - p.dhamming) > fpd->n_best_plys)
	NEXT ("movw.plys");

      if (len >= 2)
	{
	  // Destroying (parts of) the results of the previous ply
	  // won't yield an optimal sequence.
	  if (p.overrides (prev))
	    NEXT ("overrides");

	  // When two plys are independent of each other, then only
	  // investigate sequences that operate on the higher reg first.
	  // This canonicalization reduces the number of candidates,
	  if (p.commutes_with (prev, ii.m_scratch)
	      && p.regno > prev->regno)
	    NEXT ("noncanonic");

	  // Two subsequent BLDs touching the same register.
	  if (p.is_bld ()
	      && prev->is_bld ()
	      && p.changes_result_of (prev))
	    NEXT ("2bld");

	  // When there is a BLD, then at least 2 of the same kind
	  // shall occur in a row.
	  if (prev->is_bld ()
	      && ! p.is_bld ()
	      && (len == 2
		  || (prev->is_setbld () && ! prev2->is_setbld ())
		  || (prev->is_cltbld () && ! prev2->is_cltbld ())))
	    NEXT ("1bld");
	}

      // The hamming delta of a MOVW may be less than 2, namely 0 or 1.
      // When the latter is the case, then a reasonable sequence must
      // modify the result of the MOVW.
      if (len >= 2
	  && prev->is_movw ()
	  && prev->dhamming == 1
	  && ! p.changes_result_of (prev))
	NEXT ("movw.dh=1");

      if (len >= 3
	  && prev2->is_movw ()
	  && prev2->dhamming == 0
	  && ! p.changes_result_of (prev2))
	NEXT ("movw.dh=0");

      // When setting an n-byte destination, then at most n/2 MOVWs
      // will occur in an optimal sequence.
      int n_movw = 0;
      for (int i = 0; i < len; ++i)
	n_movw += fpd->ply_stack[i]->is_movw ();
      if (n_movw > ii.m_size / 2)
	NEXT ("movws");

      if (ply_applied_p)
	memo = memo0;

      memo.apply (p);

      ply_applied_p = true;

      // Calculate the cost of the sequence we have so far.  Scale by some
      // factor so that we can express that ADIW is more expensive than MOVW
      // because it is slower, but without defeating MOVW.
      const int SCALE = 4;

      int penal = 0;
      int cost = SCALE * 0;

      bool movw_p = 0;
      for (int i = 0; i < len; ++i)
	{
	  bool adiw_p = fpd->ply_stack[i]->is_adiw ();
	  cost += SCALE * fpd->ply_stack[i]->cost + adiw_p;
	  penal += adiw_p;
	  movw_p |= fpd->ply_stack[i]->is_movw ();
	}
      penal += movw_p;

      const int hamm = ii.hamming (memo);

      // The current Hamming distance yields a lower bound of how many
      // plys are still required.  Consider that future cost already now.
      int future_cost = AVR_HAVE_MOVW || (AVR_HAVE_ADIW && ii.m_regno >= REG_22)
	? (1 + hamm) / 2
	: hamm;

      // Similarly, when MOVW doesn't decrease the Hamming distance by 2,
      // then we know that at least 2 - dhamming plys must follow in the
      // future.  (MOVW + ADIW will not occur.)
      if (p.is_movw ())
	future_cost = std::max (future_cost, 2 - p.dhamming);

      if (extra && future_cost)
	avr_dump (";; future cost = %d, dh=%d\n", future_cost, hamm);

      cost += SCALE * future_cost;

      bool profitable = (cost < SCALE * fpd->max_ply_cost
			 || (bbinfo_t::try_split_any_p
			     && fpd->solution.n_plies == 0
			     && cost / SCALE <= fpd->max_ply_cost
			     && cost / SCALE == fpd->movmode_cost));
      if (! profitable)
	{
	  if (extra)
	    avr_dump (";; cont=cost %d+%d/%d\n", cost / SCALE, penal, SCALE);
	  continue;
	}

      if (hamm)
	{
	  // Go down that rabbit hole.
	  gcc_assert (ply_applied_p);
	  bbinfo_t::find_plies (1 + len, ii, memo);
	  continue;
	}

      // Found a solution that's better than everything so far.

      // Reduce the upper cost bound according to the found solution.
      // No future solution will be more expensive.
      fpd->max_ply_cost = cost / SCALE;

      fpd->solution = plies_t (len, fpd->ply_stack);

      if (dump_file)
	{
	  avr_dump (";; #%d FOUND COST = %d%s\n", len, cost / SCALE,
		    penal ? " with penalty" : "");
	  ply_t::dump_plys (dump_file, 0, len, fpd->ply_stack, fpd->regs0);
	  if (extra)
	    avr_dump (";; END\n");
	}
    } // for ply_t's

#undef NEXT
}


// Run .find_plies() and return true when .fpd->solution is a sequence of ply_t's
// that represents II, a REG = CONST insn.  MEMO is the GPR state prior to II.
bool
bbinfo_t::run_find_plies (const insninfo_t &ii, const memento_t &memo) const
{
  fpd->solution.reset ();
  fpd->regs0 = memo;
  fpd->n_get_plies = 0;

  const int hamm = ii.hamming (memo);

  if (hamm == 0)
    {
      avr_dump (";; Found redundant insn %d\n",
		ii.m_insn ? INSN_UID (ii.m_insn) : 0);
      return true;
    }

  // Upper bound (in words) for any solution that's better than mov<mode>.
  // Will be decreased by find plies as it finds better solutions.
  fpd->movmode_cost = ii.cost ();
  fpd->max_ply_cost = fpd->movmode_cost;

  // With a non-zero Hamming distance, this insn will require at least one
  // instruction.  When the upper bound for required instructions is that
  // small, then the current insn is good enough.
  if (fpd->max_ply_cost <= 1)
    return false;

  fpd->n_best_plys = ii.n_best_plys (hamm);
  gcc_assert (fpd->n_best_plys <= N_BEST_PLYS);

  if (dump_file)
    {
      const uint64_t mask = size_to_mask (ii.m_size);
      fprintf (dump_file, ";; find_plies R%d[%d] = 0x%0*" PRIx64,
	       ii.m_regno, ii.m_size, 2 * ii.m_size, ii.m_isrc & mask);
      if (ii.m_scratch)
	fprintf (dump_file, ", scratch=r%d", ii.m_scratch);
      memo.dump ("\n;; regs%s\n");
    }

  avr_dump (";; mov<mode> cost = %d\n", fpd->max_ply_cost);
  avr_dump (";; max plys = %d\n", fpd->n_best_plys);
  ply_t::n_ply_ts = 0;

  find_plies (1, ii, memo);

  avr_dump (";; get_plies called %d times\n", fpd->n_get_plies);
  avr_dump (";; n_ply_ts = %d\n", ply_t::n_ply_ts);
  ply_t::max_n_ply_ts = std::max (ply_t::max_n_ply_ts, ply_t::n_ply_ts);

  return fpd->solution.n_plies != 0;
}


// Try to propagate __zero_reg__ to a mem = reg insn's source.
// Returns true on success and sets .n_new_insns.
bool
optimize_data_t::try_mem0 (bbinfo_t *)
{
  rtx_insn *insn = curr.ii.m_insn;
  rtx set, mem, reg;
  machine_mode mode;

  if (insn
      && (set = single_set (insn))
      && MEM_P (mem = SET_DEST (set))
      && REG_P (reg = SET_SRC (set))
      && GET_MODE_SIZE (mode = GET_MODE (mem)) <= 4
      && END_REGNO (reg) <= REG_32
      && ! (regmask (reg) & memento_t::fixed_regs_mask)
      && curr.regs.have_value (REGNO (reg), GET_MODE_SIZE (mode), 0x0))
    {
      avr_dump (";; Found insn %d: mem:%m = 0 = r%d\n", INSN_UID (insn),
		mode, REGNO (reg));

      // Some insns like PUSHes don't clobber REG_CC.
      bool clobbers_cc = GET_CODE (PATTERN (insn)) == PARALLEL;

      if (clobbers_cc)
	emit_valid_move_clobbercc (mem, CONST0_RTX (mode));
      else
	emit_valid_insn (gen_rtx_SET (mem, CONST0_RTX (mode)));

      n_new_insns = 1;

      return true;
    }

  return false;
}


// Try to fuse two 1-byte insns .prev and .curr to one 2-byte insn (MOVW).
// Returns true on success, and sets .n_new_insns, .ignore_mask etc.
bool
optimize_data_t::try_fuse (bbinfo_t *bbi)
{
  insninfo_t comb;

  if (! prev.ii.m_size
      || ! curr.ii.m_size
      || ! comb.combine (prev.ii, curr.ii))
    return false;

  avr_dump (";; Working on fuse of insn %d + insn %d = 0x%04x\n",
	    INSN_UID (prev.insn), INSN_UID (curr.insn),
	    (unsigned) comb.m_isrc);

  bool found = bbi->run_find_plies (comb, prev.regs);
  if (found)
    {
      avr_dump (";; Found fuse of insns %d and %d\n",
		INSN_UID (prev.insn), INSN_UID (curr.insn));

      n_new_insns = bbinfo_t::fpd->solution.emit_insns (comb, prev.regs);
      delete_prev_p = true;

      if (prev.ii.m_scratch)
	ignore_mask |= regmask (prev.ii.m_scratch, 1);
      if (curr.ii.m_scratch)
	ignore_mask |= regmask (curr.ii.m_scratch, 1);
      ignore_mask &= ~regmask (comb.m_regno, comb.m_size);
    }

  return found;
}


// Try to replace an arithmetic 1-byte insn by a reg-reg move.
// Returns true on success, and sets .n_new_insns etc.
bool
optimize_data_t::try_simplify (bbinfo_t *)
{
  if (curr.ii.m_size == 1
      && curr.ii.m_old_code != REG
      && curr.ii.m_code == REG)
    {
      avr_dump (";; Found simplify of insn %d\n", INSN_UID (curr.insn));

      n_new_insns = curr.ii.emit_insn ();

      return true;
    }

  return false;
}


// Try to replace XEXP (*, 1) of a binary operation by a cheaper expression.
// Returns true on success; sets .n_new_insns, .ignore_mask, .delete_prev_p.
bool
optimize_data_t::try_bin_arg1 (bbinfo_t *)
{
  if (curr.ii.m_size != 1
      || curr.ii.m_new_src.arity () != 2
      || curr.unused)
    return false;

  avr_dump (";; Working on bin_arg1 insn %d\n", INSN_UID (curr.insn));

  gcc_assert (curr.ii.m_src && BINARY_P (curr.ii.m_src));
  rtx xarg1_old = XEXP (curr.ii.m_src, 1);

  const absint_byte_t &aib = curr.ii.m_new_src;
  const absint_val_t &arg0 = aib.arg (0);
  const absint_val_t &arg1 = aib.arg (1);
  const absint_val_t &arg1_old = curr.ii.m_ai[0].arg (1);

  rtx src = NULL_RTX;

  if (CONSTANT_P (xarg1_old))
    {
      // Sometimes, we allow expensive constants as 2nd operand like
      // in  R2 += 2  which produces two INCs.  When we have the
      // constant handy in a reg, then use that instead of the constant.
      const rtx_code code = aib.get_code ();
      gcc_assert (arg1.val8 == (INTVAL (xarg1_old) & 0xff));

      if (AVRasm::constant_cost (code, arg0.regno, arg1.val8) > 1)
	  src = aib.to_rtx ();
    }
  else if (REG_P (xarg1_old)
	   && dead_or_set_p (curr.insn, xarg1_old))
    {
      src = aib.to_rtx ();

      // The 2nd operand is a reg with a known content that dies
      // at the current insn.  Chances are high that the register
      // holds a reload value only used by the current insn.
      if (prev.ii.m_size == 1
	  && rtx_equal_p (xarg1_old, SET_DEST (prev.ii.m_set))
	  && CONSTANT_P (prev.ii.m_src))
	{
	  avr_dump (";; Found dying reload insn %d\n", INSN_UID (prev.insn));

	  delete_prev_p = true;
	  ignore_mask = regmask (arg1_old.regno, 1);
	}
    }

  if (src)
    {
      rtx dest = SET_DEST (curr.ii.m_set);

      avr_dump (";; Found bin_arg1 for insn %d: ", INSN_UID (curr.insn));
      avr_dump ("%C:%m %r", curr.ii.m_code, GET_MODE (dest), xarg1_old);
      aib.dump (" = %s\n");

      emit_valid_move_clobbercc (dest, src);
      n_new_insns = 1;
    }

  return src != NULL_RTX;
}


// Try to replace a REG = CONST insn by a cheaper sequence.
// Returns true on success, and sets .n_new_insns, .ignore_mask etc.
bool
optimize_data_t::try_split_ldi (bbinfo_t *bbi)
{
  if (! curr.ii.m_size
      || curr.unused
      || curr.ii.m_code != CONST_INT
      || (! bbinfo_t::try_split_any_p
	  // Finding plys will only ever succeed when there are
	  // regs with a known value.
	  && ! (curr.regs.known
		|| (AVR_HAVE_MOVW
		    && curr.ii.m_regno < REG_16 && curr.ii.m_size == 4))))
    return false;

  avr_dump (";; Working on split_ldi insn %d\n", INSN_UID (curr.insn));

  bool found = bbi->run_find_plies (curr.ii, curr.regs);
  if (found)
    {
      avr_dump (";; Found split for ldi insn %d\n", INSN_UID (curr.insn));

      n_new_insns = bbinfo_t::fpd->solution.emit_insns (curr.ii, curr.regs);

      if (curr.ii.m_scratch)
	ignore_mask = regmask (curr.ii.m_scratch, 1);
    }

  return found;
}


// Helper for try_split_any().
bool
optimize_data_t::fail (const char *reason)
{
  n_new_insns = -1;

  if (dump_file)
    fprintf (dump_file, ";; Giving up split_any: %s\n", reason);

  return false;
}


// Helper for try_split_any().
rtx_insn *
optimize_data_t::emit_and_apply_move (memento_t &memo, rtx dest, rtx src)
{
  rtx_insn *insn = emit_valid_move_clobbercc (dest, src);
  n_new_insns += 1;
  memo.apply_insn (insn, false);

  return insn;
}


// Set X0 and X1 so that they are operands valid for a andqi3, iorqi3, xorqi3
// or addqi3 insn with destination R_DEST.  The method loads X1 to
// a scratch reg as needed and records the GPR effect in IOD.regs.
// EXTRA_COST are extra costs in units of words of insns that cost more
// than one instruction.  This is a helper for try_split_any().
bool
optimize_data_t
    ::get_2ary_operands (rtx_code &code, const absint_byte_t &aib,
			 insn_optimize_data_t &iod, int r_dest,
			 absint_val_t &x0, absint_val_t &x1, int &extra_cost)
{
  if (code != IOR && code != AND && code != XOR && code != PLUS)
    return fail ("2ary: unknown code");

  x0 = aib.arg (0);
  x1 = aib.arg (1);

  if (! x0.knows_regno ()
      || x1.clueless ())
    return fail ("2ary: clueless");

  int val8 = x1.val8;
  int val8_cost = val8 < 0 ? 100 : AVRasm::constant_cost (code, r_dest, val8);

  if (x0.regno == r_dest
      && (x1.knows_regno ()
	  || val8_cost <= 1))
    {
      if (code == XOR
	  && val8 == 0x80
	  && x0.regno >= REG_16)
	{
	  // xorxi3 can only "r,0,r".
	  // x0 ^ 0x80  <=>  x0 - 0x80.
	  x1.regno = 0;
	  code = MINUS;
	}
      return true;
    }

  const bool and_1_bit = code == AND && popcount_hwi (val8) == 1;
  // andqi3 has a "r,r,Cb1" alternative where Cb1 has exactly 1 bit set.
  // This can accommodate bytes of higher AND Cb<N> alternatives.
  if (x0.regno != r_dest)
    {
      if (and_1_bit)
	{
	  extra_cost += 1 + (r_dest < REG_16);
	  return true;
	}
      else if (x1.regno == r_dest)
	{
	  std::swap (x0, x1);
	  return true;
	}
      return fail ("2ary is a 3-operand insn");
    }

  // Now we have:
  // 1)  r_dest = x0.regno, and
  // 2)  x1 is val8, and
  // 3)  x1 costs 2.

  const bool needs_scratch_p = select<bool>()
    : code == XOR ? true
    : code == AND ? popcount_hwi (val8) != 7
    : code == IOR ? popcount_hwi (val8) != 1
    : code == PLUS ? IN_RANGE (val8, 3, 0xff - 3)
    : bad_case<bool> ();

  const int r_val8 = iod.regs.regno_with_value (val8, 0 /* excludes: none */);
  if (r_val8)
    {
      // Found a reg that already holds the constant.
      x1.val8 = -1;
      x1.regno = r_val8;
      return true;
    }
  else if (iod.ii.m_scratch)
    {
      // Using the insn's scratch reg.
      rtx xdst = gen_rtx_REG (QImode, iod.ii.m_scratch);
      rtx xsrc = gen_int_mode (x1.val8, QImode);
      emit_and_apply_move (iod.regs, xdst, xsrc);

      x1.regno = iod.ii.m_scratch;
      x1.val8 = -1;

      return true;
    }
  else if (! needs_scratch_p)
    {
      // Some constants (1 and -1) can be loaded without a scratch.
      extra_cost += 1;
      return true;
    }
  else if (and_1_bit)
    {
      // This can always fall back to BST + CLR + BLD, but may be cheaper.
      extra_cost += 1 + (r_dest < REG_16);
      return true;
    }

  return fail ("2ary: expensive constant");
}


static inline bool
any_shift_p (rtx_code code)
{
  return code == LSHIFTRT || code == ASHIFTRT || code == ASHIFT;
}

// Try to split .curr into a sequence of 1-byte insns.
// Returns true on success.  Sets .n_new_insns and .ignore_mask.
bool
optimize_data_t::try_split_any (bbinfo_t *)
{
  if (curr.ii.m_size < 2
      // Constants are split by split_ldi.
      || CONSTANT_P (curr.ii.m_src)
      // Splitting requires knowledge about what to do with each byte.
      || curr.ii.m_ai.end_knows (VALUE) < curr.ii.m_size)
    return false;

  avr_dump (";; Working on split_any %C:%m insn %d\n", curr.ii.m_code,
	    GET_MODE (SET_DEST (curr.ii.m_set)), INSN_UID (curr.insn));

  const insninfo_t &ii = curr.ii;
  const int n_bytes = ii.m_size;
  int extra_cost = 0;
  int binop_cost = -1;

  // For plain AND, IOR, XOR get the current cost in units of words.
  if (BINARY_P (curr.ii.m_src))
    {
      const rtx_code code = curr.ii.m_code;
      if ((code == IOR || code == AND || code == XOR)
	  && REG_P (XEXP (curr.ii.m_src, 0))
	  && CONSTANT_P (XEXP (curr.ii.m_src, 1)))
	{
	  binop_cost = get_attr_length (curr.insn);
	  avr_dump (";; Competing against %C:%m cost = %d\n", code,
		    GET_MODE (curr.ii.m_src), binop_cost);
	}
    }

  // Step 1: Work out conflicts and which sign extends to perform.

  const gprmask_t regs_dest = regmask (ii.m_regno, n_bytes);
  int r_sign = 0;
  gprmask_t regs_signs = 0;
  bool has_lsl = false;
  bool has_lsr = false;

  for (int i = 0; i < n_bytes; ++i)
    {
      const absint_byte_t &aib = ii.m_ai[i];
      const int r_dest = ii.m_regno + i;
      const gprmask_t regs_src = aib.reg_mask ();

      // When only regs to the right are used, or only regs to the left
      // are used, then there's no conflict like it is arising for rotates.
      // For now, only implement conflict-free splits.
      has_lsl |= has_bits_in (regs_src & regs_dest, 0, r_dest - 1);
      has_lsr |= has_bits_in (regs_src & regs_dest, r_dest + 1, 31);
      if (has_lsl && has_lsr)
	return fail ("has both << and >>");

      if (aib.get_code () == SIGN_EXTEND)
	{
	  const absint_val_t x0 = aib.arg (0);
	  if (! r_sign)
	    r_sign = x0.regno;
	  else if (r_sign != x0.regno)
	    return fail ("too many signs");

	  // Signs are handled below after all the other bytes.
	  regs_signs |= regmask (r_dest, 1);
	}
    }

  // Step 2: Work on the individual bytes and emit according insns.

  n_new_insns = 0;
  memento_t memo = curr.regs;

  const int step = has_lsl ? -1 : 1;
  const int istart = step == 1 ? 0 : n_bytes - 1;
  const int iend = step == 1 ? n_bytes : -1;

  for (int i = istart; i != iend; i += step)
    {
      const absint_byte_t &aib = ii.m_ai[i];
      const int r_dest = ii.m_regno + i;
      rtx_code code = aib.get_code ();
      rtx xsrc = NULL_RTX;
      rtx xdest = gen_rtx_REG (QImode, r_dest);

      if (code == SET)
	{
	  const int r_src = aib.regno (false);
	  const int val8 = aib.val8 (false);
	  int r16;

	  // A no-op...
	  if (r_dest == r_src)
	    continue;
	  // ...or an existing 16-bit constant...
	  else if (AVR_HAVE_MOVW
		   && i + step != iend
		   // Next is not a no-op.
		   && ii.m_ai[i + step].regno (false) != r_dest + step
		   // Eligible for MOVW.
		   && r_dest + step == (r_dest ^ 1)
		   && r_dest % 2 == i % 2
		   && (r16 = ii.m_ai.reg16_with_value (i, i + step, memo)))
	    {
	      xdest = gen_rtx_REG (HImode, r_dest & ~1);
	      xsrc = gen_rtx_REG (HImode, r16);
	      i += step;
	    }
	  // ...or a reg-reg move from a multi-byte move...
	  else if (r_src
		   // Prefer a reg-reg move over a (potential) load
		   // of a constant, because the subsequent RTL
		   // peephole pass may combine it to a MOVW again.
		   && AVR_HAVE_MOVW
		   && REG_P (curr.ii.m_src))
	    xsrc = gen_rtx_REG (QImode, r_src);
	  // ...or a cheap constant...
	  else if (val8 >= 0
		   && AVRasm::constant_cost (SET, r_dest, val8) <= 1)
	    xsrc = gen_int_mode (val8, QImode);
	  // ...or a reg-reg move...
	  else if (r_src)
	    xsrc = gen_rtx_REG (QImode, r_src);
	  // ...or a costly constant that already exists in some reg...
	  else if (memo.regno_with_value (val8, 0 /* excludes: none */))
	    xsrc = gen_rtx_REG (QImode, memo.regno_with_value (val8, 0));
	  // ...or a costly constant loaded into curr.insn's scratch reg...
	  else if (ii.m_scratch)
	    {
	      rtx xscratch = gen_rtx_REG (QImode, ii.m_scratch);
	      rtx xval8 = gen_int_mode (val8, QImode);
	      emit_and_apply_move (memo, xscratch, xval8);
	      xsrc = xscratch;
	    }
	  // ...or a costly constant (1 or -1) that doesn't need a scratch.
	  else if (! AVRasm::ldi_needs_scratch (r_dest, val8))
	    {
	      extra_cost += 1;
	      xsrc = gen_int_mode (val8, QImode);
	    }
	  else
	    return fail ("expensive val8");
	} // SET
      else if (aib.arity () == 1)
	{
	  if (aib.get_code () == SIGN_EXTEND)
	    // Signs are handled after all the others.
	    continue;
	  else
	    {
	      const absint_val_t x0 = aib.arg (0);
	      rtx xop0 = gen_rtx_REG (QImode, x0.regno);
	      xsrc = gen_rtx_fmt_e (code, QImode, xop0);
	    }
	} // unary
      else if (aib.arity () == 2)
	{
	  absint_val_t x0;
	  absint_val_t x1;
	  insn_optimize_data_t iod (memo);
	  iod.ii = curr.ii;

	  if (! get_2ary_operands (code, aib, iod, r_dest, x0, x1, extra_cost))
	    return false;
	  rtx xop0 = gen_rtx_REG (QImode, x0.regno);
	  rtx xop1 = x1.knows_val8 ()
	    ? gen_int_mode (x1.val8, QImode)
	    : gen_rtx_REG (QImode, x1.regno);

	  xsrc = gen_rtx_fmt_ee (code, QImode, xop0, xop1);
	} // binary

      if (! xsrc)
	return fail ("no source found");

      if (r_sign
	  && (regmask (xdest) & regmask (r_sign, 1)))
	return fail ("clobbered r_sign");

      emit_and_apply_move (memo, xdest, xsrc);
    }

  // Step 3: Emit insns for sign extend.
  // No more need to track memo beyond this point.

  if (! emit_signs (r_sign, regs_signs))
    return false;

  if (binop_cost >= 0)
    {
      avr_dump (";; Expected cost: %d + %d\n", n_new_insns, extra_cost);
      if (n_new_insns + extra_cost > binop_cost)
	return fail ("too expensive");
    }

  if (ii.m_scratch)
    ignore_mask = regmask (ii.m_scratch, 1);

  return true;
}


// A helper for try_split_any() above.
// Emit sign extends from R_MSB.7 to all regs in REGS_SIGNS.
bool
optimize_data_t::emit_signs (const int r_msb, gprmask_t regs_signs)
{
  if (! regs_signs)
    return true;
  else if (! r_msb)
    return fail ("fatal: no r_msb given");

  // Pick an arbitrary reg from the sign destinations when the source
  // isn't one of the signs.
  const int r_signs = regs_signs & regmask (r_msb, 1)
    ? r_msb
    : ctz_hwi (regs_signs);

  // Set all bits in r_signs according to the sign of r_msb using the
  // r,r,C07 alternative of ashrqi3.
  rtx xsrc = gen_rtx_fmt_ee (ASHIFTRT, QImode,
			     gen_rtx_REG (QImode, r_msb), GEN_INT (7));
  emit_valid_move_clobbercc (gen_rtx_REG (QImode, r_signs), xsrc);
  regs_signs &= ~regmask (r_signs, 1);

  // Set up a 16-bit sign register if possible.
  int r16_signs = 0;
  if (regs_signs & regmask (r_signs ^ 1, 1))
    {
      emit_move_mask (r_signs ^ 1, r_signs, 1, regs_signs);
      r16_signs = r_signs & ~1;
    }

  // Handle all 16-bit signs regs provided MOVW.
  if (AVR_HAVE_MOVW)
    for (int r = FIRST_GPR; r < REG_32; r += 2)
      {
	const gprmask_t m = regmask (r, 2);
	if ((m & regs_signs) == m)
	  {
	    if (r16_signs)
	      emit_move_mask (r, r16_signs, 2, regs_signs);
	    else
	      {
		emit_move_mask (r + 0, r_signs, 1, regs_signs);
		emit_move_mask (r + 1, r_signs, 1, regs_signs);
		r16_signs = r;
	      }
	  }
      }

  // Handle all remaining signs.
  while (regs_signs)
    emit_move_mask (ctz_hwi (regs_signs), r_signs, 1, regs_signs);

  return true;
}

// Helper for the method above.  Move N_BYTES registers from R_SRC to R_DST,
// keeping track of which regs are still to be done in MASK.
void
optimize_data_t::emit_move_mask (int r_dst, int r_src, int n_bytes,
				 gprmask_t &mask)
{
  const gprmask_t mask_dst = regmask (r_dst, n_bytes);
  const gprmask_t mask_src = regmask (r_src, n_bytes);
  gcc_assert ((mask_dst & mask) == mask_dst);
  gcc_assert ((mask_src & mask) == 0);
  rtx xdst = gen_rtx_REG (size_to_mode (n_bytes), r_dst);
  rtx xsrc = gen_rtx_REG (size_to_mode (n_bytes), r_src);
  emit_valid_move_clobbercc (xdst, xsrc);
  n_new_insns += 1;
  mask &= ~mask_dst;
}


void
bbinfo_t::optimize_one_block (bool &changed)
{
  memento_t prev_regs;

  rtx_insn *insn = next_nondebug_insn_bb (bb, BB_HEAD (bb));

  for (rtx_insn *next_insn; insn; insn = next_insn)
    {
      next_insn = next_nondebug_insn_bb (bb, insn);

      avr_dump ("\n;; Working on insn %d\n%r\n\n", INSN_UID (insn), insn);

      optimize_data_t od (prev_regs, regs);

      od.prev.insn = prev_nondebug_insn_bb (bb, insn);
      od.curr.insn = insn;

      od.prev.ii.init1 (od.prev, 1, "IIprev ");
      od.curr.ii.init1 (od.curr, 8, "IIcurr ");

      start_sequence ();

      bool found = ((bbinfo_t::try_fuse_p && od.try_fuse (this))
		    || (bbinfo_t::try_bin_arg1_p && od.try_bin_arg1 (this))
		    || (bbinfo_t::try_simplify_p && od.try_simplify (this))
		    || (bbinfo_t::try_split_ldi_p && od.try_split_ldi (this))
		    || (bbinfo_t::try_split_any_p && od.try_split_any (this))
		    || (bbinfo_t::try_mem0_p && od.try_mem0 (this)));

      rtx_insn *new_insns = get_insns ();
      end_sequence ();

      gcc_assert (found == (od.n_new_insns >= 0));

      ++tick;

      // This insn will become the previous one in the next loop iteration.
      // Just used in dumps.
      rtx_insn *new_curr_insn;

      if (! found)
	{
	  // Nothing changed.
	  avr_dump (";; Keeping old route.\n");
	  gcc_assert (! od.delete_prev_p);

	  prev_regs = regs;
	  regs.apply_insn (insn, false);

	  new_curr_insn = insn;
	}
      else
	{
	  // We have new_insns.
	  changed = true;

	  if (dump_file)
	    {
	      avr_dump ("\n;; EMIT %d new insn%s replacing ",
			od.n_new_insns, "s" + (od.n_new_insns == 1));
	      if (od.delete_prev_p)
		avr_dump ("insn %d and ", INSN_UID (od.prev.insn));
	      avr_dump ("insn %d, delete_prev=%d:\n%L\n", INSN_UID (insn),
			od.delete_prev_p, new_insns);
	    }

	  new_curr_insn = od.emit_sequence (bb, new_insns);
	} // found

      if (dump_file && new_curr_insn)
	{
	  avr_dump ("\n");

	  const int d = regs.distance_to (prev_regs);
	  if (d || new_curr_insn != insn)
	    avr_dump (";; %d regs changed state:\n", d);

	  if (new_curr_insn != insn)
	    {
	      avr_dump (";; Befor insn %d", INSN_UID (new_curr_insn));
	      prev_regs.dump ();
	    }

	  avr_dump (";; After insn %d", INSN_UID (new_curr_insn));
	  regs.dump ();
	}
    } // for BB insns
}


void
bbinfo_t::optimize_one_function (function *func)
{
  bbinfo_t::fpd = XNEW (bbinfo_t::find_plies_data_t);
  bbinfo_t::bb_info = XCNEWVEC (bbinfo_t, last_basic_block_for_fn (func));
  int *post_order = XNEWVEC (int, n_basic_blocks_for_fn (func));

  plies_t::max_n_plies = 0;

  using elt0_getter_HRS = elt0_getter<HARD_REG_SET, HARD_REG_ELT_TYPE>;
  memento_t::fixed_regs_mask = (gprmask_t) elt0_getter_HRS::get (fixed_reg_set);

  // Option -mfuse-move=<0,23> provides a 3:2:2:2 mixed radix value:
  // -mfuse-move= 0 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 20 1 2 3  Digit
  // fuse           1   1   1   1   1    1   1   1   1   1    1   1      0
  // bin_arg1         1 1     1 1      1 1     1 1     1 1      1 1      1
  // split_any            1 1 1 1          1 1 1 1          1 1 1 1      2
  // split_ldi                    1 1  1 1 1 1 1 1 1 1 1 1  1 1 1 1      3
  // use arith                                     1 1 1 1  1 1 1 1      3

  // Which optimization(s) to perform.
  bbinfo_t::try_fuse_p = avropt_fuse_move & 0x1;      // Digit 0 in [0, 1].
  bbinfo_t::try_mem0_p = avropt_fuse_move & 0x1;      // Digit 0 in [0, 1].
  bbinfo_t::try_bin_arg1_p = avropt_fuse_move & 0x2;  // Digit 1 in [0, 1].
  bbinfo_t::try_split_any_p = avropt_fuse_move & 0x4; // Digit 2 in [0, 1].
  bbinfo_t::try_split_ldi_p = avropt_fuse_move >> 3;    // Digit 3 in [0, 2].
  bbinfo_t::use_arith_p = (avropt_fuse_move >> 3) >= 2; // Digit 3 in [0, 2].
  bbinfo_t::use_set_some_p = bbinfo_t::try_split_ldi_p; // Digit 3 in [0, 2].
  bbinfo_t::try_simplify_p = avropt_fuse_move != 0;

  // Topologically sort BBs from last to first.

  const int n_post_order = post_order_compute (post_order, false, false);
  bool changed = false;

  // Traverse the BBs from first to last in order to increase the chance
  // that register values from all incoming edges are known.

  for (int n = n_post_order - 1; n >= 0; --n)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (func, post_order[n]);

      bbinfo_t::bb_info[bb->index].bb = bb;
      bbinfo_t::bb_info[bb->index].enter ();
      bbinfo_t::bb_info[bb->index].optimize_one_block (changed);
      bbinfo_t::bb_info[bb->index].leave ();
    }

  if (plies_t::max_n_plies)
    avr_dump (";; max_n_plies=%d\n", (int) plies_t::max_n_plies);

  if (changed)
    {
      df_note_add_problem ();
      df_analyze ();
    }

  XDELETEVEC (post_order);
  XDELETEVEC (bbinfo_t::bb_info);
  XDELETE (bbinfo_t::fpd);
}

} // anonymous namespace


namespace
{


//////////////////////////////////////////////////////////////////////////////
// Try to replace 2 cbranch insns with 1 comparison and 2 branches.

static const pass_data avr_pass_data_ifelse =
{
  RTL_PASS,      // type
  "",            // name (will be patched)
  OPTGROUP_NONE, // optinfo_flags
  TV_DF_SCAN,    // tv_id
  0,             // properties_required
  0,             // properties_provided
  0,             // properties_destroyed
  0,             // todo_flags_start
  TODO_df_finish | TODO_df_verify // todo_flags_finish
};

class avr_pass_ifelse : public rtl_opt_pass
{
public:
  avr_pass_ifelse (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_ifelse, ctxt)
  {
    this->name = name;
  }

  bool gate (function *) final override
  {
    return optimize > 0;
  }

  unsigned int execute (function *func) final override;
}; // avr_pass_ifelse


/* Return TRUE iff comparison code CODE is explicitly signed.  */

static bool
avr_strict_signed_p (rtx_code code)
{
  return code == GT || code == GE || code == LT || code == LE;
}


/* Return TRUE iff comparison code CODE is explicitly unsigned.  */

static bool
avr_strict_unsigned_p (rtx_code code)
{
  return code == GTU || code == GEU || code == LTU || code == LEU;
}

#include "config/avr/ranges.h"

/* Suppose the inputs represent a code like

      if (x <CMP1> XVAL1)  goto way1;
      if (x <CMP2> XVAL2)  goto way2;
      way3:;

   with two integer mode comparisons where XVAL1 and XVAL2 are CONST_INT.
   When this can be rewritten in the form

      if (x <cond1> xval)  goto way1;
      if (x <cond2> xval)  goto way2;
      way3:;

  then set CMP1 = cond1, CMP2 = cond2, and return xval.  Else return NULL_RTX.
  When SWAPT is returned true, then way1 and way2 must be swapped.
  When the incomping SWAPT is false, the outgoing one will be false, too.  */

static rtx
avr_2comparisons_rhs (rtx_code &cmp1, rtx xval1,
		      rtx_code &cmp2, rtx xval2,
		      machine_mode mode, bool &swapt)
{
  const bool may_swapt = swapt;
  swapt = false;

  //////////////////////////////////////////////////////////////////
  // Step 0: Decide about signedness, map xval1/2 to the range
  //         of [un]signed machine mode.

  const bool signed1_p = avr_strict_signed_p (cmp1);
  const bool signed2_p = avr_strict_signed_p (cmp2);
  const bool unsigned1_p = avr_strict_unsigned_p (cmp1);
  const bool unsigned2_p = avr_strict_unsigned_p (cmp2);
  const bool signed_p = signed1_p || signed2_p;
  bool unsigned_p = unsigned1_p || unsigned2_p;

  using T = Ranges::scalar_type;
  T val1 = INTVAL (xval1);
  T val2 = INTVAL (xval2);

  if (signed_p + unsigned_p > 1)
    {
      // Don't go down that rabbit hole.  When the RHSs are the
      // same, we can still save one comparison.
      return val1 == val2 ? xval1 : NULL_RTX;
    }

  // Decide about signedness.  When no explicit signedness is present,
  // then cases that are close to the unsigned boundary like  EQ 0, EQ 1
  // can also be optimized.
  if (unsigned_p
      || (! signed_p && IN_RANGE (val1, -2, 2)))
    {
      unsigned_p = true;
      val1 = UINTVAL (xval1) & GET_MODE_MASK (mode);
      val2 = UINTVAL (xval2) & GET_MODE_MASK (mode);
    }

  // No way we can decompose the domain in a usable manner when the
  // RHSes are too far apart.
  if (! IN_RANGE (val1 - val2, -2, 2))
    return NULL_RTX;

  //////////////////////////////////////////////////////////////////
  // Step 1: Represent the input conditions as truth Ranges.  This
  //         establishes a decomposition / coloring of the domain.

  Ranges dom = Ranges::NBitsRanges (GET_MODE_BITSIZE (mode), unsigned_p,
				    Ranges::ALL);
  Ranges r[4] = { dom, dom.truth (cmp1, val1), dom.truth (cmp2, val2), dom };

  // r[1] shadows r[2] shadows r[3].  r[0] is just for nice indices.
  r[3].minus (r[2]);
  r[3].minus (r[1]);
  r[2].minus (r[1]);

  //////////////////////////////////////////////////////////////////
  // Step 2: Filter for cases where the domain decomposes into three
  //         intervals:  One to the left, one to the right, and one
  //         in the middle where the latter holds exactly one value.

  for (int i = 1; i <= 3; ++i)
    {
      // Keep track of which Ranges is which.
      r[i].tag = i;

      gcc_assert (r[i].check ());

      // Filter for proper intervals.  Also return for the empty set,
      // since cases where [m_min, m_max] decomposes into two intervals
      // or less have been sorted out by the generic optimizers already,
      // and hence should not be seen here.  And more than two intervals
      // at a time cannot be optimized of course.
      if (r[i].size () != 1)
	return NULL_RTX;
    }

  // Bubble-sort the three intervals such that:
  // [1] is the left interval, i.e. the one taken by LT[U].
  // [2] is the middle interval, i.e. the one taken by EQ.
  // [3] is the right interval, i.e. the one taken by GT[U].
  Ranges::sort2 (r[1], r[3]);
  Ranges::sort2 (r[2], r[3]);
  Ranges::sort2 (r[1], r[2]);

  if (dump_file)
    fprintf (dump_file,
	     ";; Decomposed: .%d=[%ld, %ld] .%d=[%ld, %ld] .%d=[%ld, %ld]\n",
	     r[1].tag, (long) r[1].ranges[0].lo, (long) r[1].ranges[0].hi,
	     r[2].tag, (long) r[2].ranges[0].lo, (long) r[2].ranges[0].hi,
	     r[3].tag, (long) r[3].ranges[0].lo, (long) r[3].ranges[0].hi);

  // EQ / NE can handle only one value.
  if (r[2].cardinality (0) != 1)
    return NULL_RTX;

  // Success! This is the sought for xval.
  const T val = r[2].ranges[0].lo;

  //////////////////////////////////////////////////////////////////
  // Step 3: Work out which label gets which condition, trying to
  //         avoid the expensive codes GT[U] and LE[U] if possible.
  //         Avoiding expensive codes is always possible when labels
  //         way1 and way2 may be swapped.

  // The xx1 ways have an expensive GT for cmp1 which can be avoided
  // by swapping way1 with way2.
  swapt = may_swapt && r[3].tag == 1;
  if (swapt)
    std::swap (r[3], r[2].tag == 2 ? r[2] : r[1]);

  // 6 = 3! ways to assign LT, EQ, GT to the three labels.
  const int way = 100 * r[1].tag + 10 * r[2].tag + r[3].tag;

  if (dump_file)
    fprintf (dump_file, ";; Success: unsigned=%d, swapt=%d, way=%d, rhs=%ld\n",
	     unsigned_p, swapt, way, (long) val);

#define WAY(w, c1, c2)					\
  case w:						\
    cmp1 = unsigned_p ? unsigned_condition (c1) : c1;	\
    cmp2 = unsigned_p ? unsigned_condition (c2) : c2;	\
    break;

  switch (way)
    {
    default:
      gcc_unreachable ();

      // cmp1 gets the LT, avoid difficult branches for cmp2.
      WAY (123, LT, EQ);
      WAY (132, LT, NE);

      // cmp1 gets the EQ, avoid difficult branches for cmp2.
      WAY (213, EQ, LT);
      WAY (312, EQ, GE);

      // cmp1 gets the difficult GT, unavoidable as we may not swap way1/2.
      WAY (231, GT, NE);
      WAY (321, GT, EQ);
    }

#undef WAY

  return gen_int_mode (val, mode);
}


/* A helper for the next method.  Suppose we have two conditional branches
   with REG and CONST_INT operands

      if (reg <cond1> xval1) goto label1;
      if (reg <cond2> xval2) goto label2;

   If the second comparison is redundant and there are codes <cmp1>
   and <cmp2> such that the sequence can be performed as

      REG_CC = compare (reg, xval);
      if (REG_CC <cmp1> 0) goto label1;
      if (REG_CC <cmp2> 0) goto label2;

   then set COND1 to cmp1, COND2 to cmp2, SWAPT to true when the branch
   targets have to be swapped, and return XVAL.  Otherwise, return NULL_RTX.
   This function may clobber COND1 and COND2 even when it returns NULL_RTX.

   REVERSE_COND1 can be set to reverse condition COND1.  This is useful
   when the second comparison does not follow the first one, but is
   located after label1 like in:

      if (reg <cond1> xval1) goto label1;
      ...
      label1:
      if (reg <cond2> xval2) goto label2;

   In such a case we cannot swap the labels, and we may end up with a
   difficult branch -- though one comparison can still be optimized out.
   Getting rid of such difficult branches would require to reorder blocks. */

static rtx
avr_redundant_compare (rtx xreg1, rtx_code &cond1, rtx xval1,
		       rtx xreg2, rtx_code &cond2, rtx xval2,
		       bool reverse_cond1, bool &swapt)
{
  // Make sure we have two REG <cond> CONST_INT comparisons with the same reg.
  if (! rtx_equal_p (xreg1, xreg2)
      || ! CONST_INT_P (xval1)
      || ! CONST_INT_P (xval2))
    return NULL_RTX;

  if (reverse_cond1)
    cond1 = reverse_condition (cond1);

  // Allow swapping label1 <-> label2 only when ! reverse_cond1.
  swapt = ! reverse_cond1;
  rtx_code c1 = cond1;
  rtx_code c2 = cond2;
  rtx xval = avr_2comparisons_rhs (c1, xval1,
				   c2, xval2, GET_MODE (xreg1), swapt);
  if (! xval)
    return NULL_RTX;

  if (dump_file)
    {
      rtx_code a1 = reverse_cond1 ? reverse_condition (cond1) : cond1;
      rtx_code b1 = reverse_cond1 ? reverse_condition (c1) : c1;
      const char *s_rev1 = reverse_cond1 ? " reverse_cond1" : "";
      avr_dump (";; cond1: %C %r%s\n", a1, xval1, s_rev1);
      avr_dump (";; cond2: %C %r\n", cond2, xval2);
      avr_dump (";; => %C %d\n", b1, (int) INTVAL (xval));
      avr_dump (";; => %C %d\n", c2, (int) INTVAL (xval));
    }

  cond1 = c1;
  cond2 = c2;

  return xval;
}


/* Similar to the function above, but assume that

      if (xreg1 <cond1> xval1) goto label1;
      if (xreg2 <cond2> xval2) goto label2;

   are two subsequent REG-REG comparisons.  When this can be represented as

      REG_CC = compare (reg, xval);
      if (REG_CC <cmp1> 0) goto label1;
      if (REG_CC <cmp2> 0) goto label2;

   then set XREG1 to reg, COND1 and COND2 accordingly, and return xval.
   Otherwise, return NULL_RTX.  This optmization can be performed
   when { xreg1, xval1 } and { xreg2, xval2 } are equal as sets.
   It can be done in such a way that no difficult branches occur.  */

static rtx
avr_redundant_compare_regs (rtx &xreg1, rtx_code &cond1, rtx &xval1,
			    rtx &xreg2, rtx_code &cond2, rtx &xval2,
			    bool reverse_cond1)
{
  bool swapped;

  if (! REG_P (xval1))
    return NULL_RTX;
  else if (rtx_equal_p (xreg1, xreg2)
	   && rtx_equal_p (xval1, xval2))
    swapped = false;
  else if (rtx_equal_p (xreg1, xval2)
	   && rtx_equal_p (xreg2, xval1))
    swapped = true;
  else
    return NULL_RTX;

  // Found a redundant REG-REG comparison.  Assume that the incoming
  // representation has been canonicalized by CANONICALIZE_COMPARISON.
  // We can always represent this using only one comparison and in such
  // a way that no difficult branches are required.

  if (dump_file)
    {
      const char *s_rev1 = reverse_cond1 ? " reverse_cond1" : "";
      avr_dump (";; %r %C %r%s\n", xreg1, cond1, xval1, s_rev1);
      avr_dump (";; %r %C %r\n", xreg2, cond2, xval2);
    }

  if (reverse_cond1)
    cond1 = reverse_condition (cond1);

  if (swapped)
    {
      if (cond1 == EQ || cond1 == NE)
	{
	  avr_dump (";; case #21\n");
	  std::swap (xreg1, xval1);
	}
      else
	{
	  std::swap (xreg2, xval2);
	  cond2 = swap_condition (cond2);

	  // The swap may have introduced a difficult comparison.
	  // In order to get of it, only a few cases need extra care.
	  if ((cond1 == LT && cond2 == GT)
	      || (cond1 == LTU && cond2 == GTU))
	    {
	      avr_dump (";; case #22\n");
	      cond2 = NE;
	    }
	  else
	    avr_dump (";; case #23\n");
	}
    }
  else
    avr_dump (";; case #20\n");

  return xval1;
}


/* INSN1 and INSN2 are two cbranch insns for the same integer mode.
   When FOLLOW_LABEL1 is false, then INSN2 is located in the fallthrough
   path of INSN1.  When FOLLOW_LABEL1 is true, then INSN2 is located at
   the true edge of INSN1, INSN2 is preceded by a barrier, and no other
   edge leads to the basic block of INSN2.

   Try to replace INSN1 and INSN2 by a compare insn and two branch insns.
   When such a replacement has been performed, then return the insn where the
   caller should continue scanning the insn stream.  Else, return nullptr.  */

static rtx_insn *
avr_optimize_2ifelse (rtx_jump_insn *insn1,
		      rtx_jump_insn *insn2, bool follow_label1)
{
  avr_dump (";; Investigating jump_insn %d and jump_insn %d.\n",
	    INSN_UID (insn1), INSN_UID (insn2));

  // Extract the operands of the insns:
  // $0 = comparison operator ($1, $2)
  // $1 = reg
  // $2 = reg or const_int
  // $3 = code_label
  // $4 = optional SCRATCH for HI, PSI, SI cases.

  const auto &op = recog_data.operand;

  extract_insn (insn1);
  rtx xop1[5] = { op[0], op[1], op[2], op[3], op[4] };
  int n_operands = recog_data.n_operands;

  extract_insn (insn2);
  rtx xop2[5] = { op[0], op[1], op[2], op[3], op[4] };

  rtx_code code1 = GET_CODE (xop1[0]);
  rtx_code code2 = GET_CODE (xop2[0]);
  bool swap_targets = false;

  // Search redundant REG-REG comparison.
  rtx xval = avr_redundant_compare_regs (xop1[1], code1, xop1[2],
					 xop2[1], code2, xop2[2],
					 follow_label1);

  // Search redundant REG-CONST_INT comparison.
  if (! xval)
    xval = avr_redundant_compare (xop1[1], code1, xop1[2],
				  xop2[1], code2, xop2[2],
				  follow_label1, swap_targets);
  if (! xval)
    {
      avr_dump (";; Nothing found for jump_insn %d and jump_insn %d.\n",
		INSN_UID (insn1), INSN_UID (insn2));
      return nullptr;
    }

  if (follow_label1)
    code1 = reverse_condition (code1);

  //////////////////////////////////////////////////////
  // Found a replacement.

  if (dump_file)
    {
      avr_dump (";; => %C %r\n", code1, xval);
      avr_dump (";; => %C %r\n", code2, xval);

      fprintf (dump_file, "\n;; Found chain of jump_insn %d and"
	       " jump_insn %d, follow_label1=%d:\n",
	       INSN_UID (insn1), INSN_UID (insn2), follow_label1);
      print_rtl_single (dump_file, PATTERN (insn1));
      print_rtl_single (dump_file, PATTERN (insn2));
    }

  rtx_insn *next_insn
    = next_nonnote_nondebug_insn (follow_label1 ? insn1 : insn2);

  // Pop the new branch conditions and the new comparison.
  // Prematurely split into compare + branch so that we can drop
  // the 2nd comparison.  The following pass, split2, splits all
  // insns for REG_CC, and it should still work as usual even when
  // there are already some REG_CC insns around.

  rtx xcond1 = gen_rtx_fmt_ee (code1, VOIDmode, cc_reg_rtx, const0_rtx);
  rtx xcond2 = gen_rtx_fmt_ee (code2, VOIDmode, cc_reg_rtx, const0_rtx);
  rtx xpat1 = gen_branch (xop1[3], xcond1);
  rtx xpat2 = gen_branch (xop2[3], xcond2);
  rtx xcompare = NULL_RTX;
  machine_mode mode = GET_MODE (xop1[1]);

  if (mode == QImode)
    {
      gcc_assert (n_operands == 4);
      xcompare = gen_cmpqi3 (xop1[1], xval);
    }
  else
    {
      gcc_assert (n_operands == 5);
      rtx scratch = GET_CODE (xop1[4]) == SCRATCH ? xop2[4] : xop1[4];
      rtx (*gen_cmp)(rtx,rtx,rtx)
	= mode == HImode  ? gen_gen_comparehi
	: mode == PSImode ? gen_gen_comparepsi
	: gen_gen_comparesi; // SImode
      xcompare = gen_cmp (xop1[1], xval, scratch);
    }

  // Emit that stuff.

  rtx_insn *cmp = emit_insn_before (xcompare, insn1);
  rtx_jump_insn *branch1 = emit_jump_insn_after (xpat1, insn1);
  rtx_jump_insn *branch2 = emit_jump_insn_after (xpat2, insn2);

  JUMP_LABEL (branch1) = xop1[3];
  JUMP_LABEL (branch2) = xop2[3];
  // delete_insn() decrements LABEL_NUSES when deleting a JUMP_INSN,
  // but when we pop a new JUMP_INSN, do it by hand.
  ++LABEL_NUSES (xop1[3]);
  ++LABEL_NUSES (xop2[3]);

  delete_insn (insn1);
  delete_insn (insn2);

  if (swap_targets)
    {
      gcc_assert (! follow_label1);

      basic_block to1 = BLOCK_FOR_INSN (xop1[3]);
      basic_block to2 = BLOCK_FOR_INSN (xop2[3]);
      edge e1 = find_edge (BLOCK_FOR_INSN (branch1), to1);
      edge e2 = find_edge (BLOCK_FOR_INSN (branch2), to2);
      gcc_assert (e1);
      gcc_assert (e2);
      redirect_edge_and_branch (e1, to2);
      redirect_edge_and_branch (e2, to1);
    }

  // As a side effect, also recog the new insns.
  gcc_assert (valid_insn_p (cmp));
  gcc_assert (valid_insn_p (branch1));
  gcc_assert (valid_insn_p (branch2));

  return next_insn;
}


/* Sequences like

      SREG = compare (reg, 1 + val);
	  if (SREG >= 0)  goto label1;
      SREG = compare (reg, val);
	  if (SREG == 0)  goto label2;

   can be optimized to

      SREG = compare (reg, val);
	  if (SREG == 0)  goto label2;
	  if (SREG >= 0)  goto label1;

   Almost all cases where one of the comparisons is redundant can
   be transformed in such a way that only one comparison is required
   and no difficult branches are needed.  */

unsigned int
avr_pass_ifelse::execute (function *)
{
  rtx_insn *next_insn;

  for (rtx_insn *insn = get_insns (); insn; insn = next_insn)
    {
      next_insn = next_nonnote_nondebug_insn (insn);

      if (! next_insn)
	break;

      // Search for two cbranch insns.  The first one is a cbranch.
      // Filter for "cbranch<mode>4_insn" with mode in QI, HI, PSI, SI.

      if (! JUMP_P (insn))
	continue;

      int icode1 = recog_memoized (insn);

      if (icode1 != CODE_FOR_cbranchqi4_insn
	  && icode1 != CODE_FOR_cbranchhi4_insn
	  && icode1 != CODE_FOR_cbranchpsi4_insn
	  && icode1 != CODE_FOR_cbranchsi4_insn)
	continue;

      rtx_jump_insn *insn1 = as_a<rtx_jump_insn *> (insn);

      // jmp[0]: We can optimize cbranches that follow cbranch insn1.
      rtx_insn *jmp[2] = { next_insn, nullptr };

      // jmp[1]: A cbranch following the label of cbranch insn1.
      if (LABEL_NUSES (JUMP_LABEL (insn1)) == 1)
	{
	  rtx_insn *code_label1 = JUMP_LABEL_AS_INSN (insn1);
	  rtx_insn *barrier = prev_nonnote_nondebug_insn (code_label1);

	  // When the target label of insn1 is used exactly once and is
	  // not a fallthrough, i.e. is preceded by a barrier, then
	  // consider the insn following that label.
	  if (barrier && BARRIER_P (barrier))
	    jmp[1] = next_nonnote_nondebug_insn (code_label1);
      }

      // With almost certainty, only one of the two possible jumps can
      // be optimized with insn1, but it's hard to tell which one a priori.
      // Just try both.  In the unlikely case where both could be optimized,
      // prefer jmp[0] because eliminating difficult branches is impeded
      // by following label1.

      for (int j = 0; j < 2; ++j)
	if (jmp[j] && JUMP_P (jmp[j])
	    && recog_memoized (jmp[j]) == icode1)
	  {
	    rtx_insn *next
	      = avr_optimize_2ifelse (insn1, as_a<rtx_jump_insn *> (jmp[j]),
				      j == 1 /* follow_label1 */);
	    if (next)
	      {
		next_insn = next;
		break;
	      }
	  }

    } // loop insns

  return 0;
}



//////////////////////////////////////////////////////////////////////////////
// Optimize results of the casesi expander for modes < SImode.

static const pass_data avr_pass_data_casesi =
{
  RTL_PASS,      // type
  "",            // name (will be patched)
  OPTGROUP_NONE, // optinfo_flags
  TV_DF_SCAN,    // tv_id
  0,             // properties_required
  0,             // properties_provided
  0,             // properties_destroyed
  0,             // todo_flags_start
  0              // todo_flags_finish
};

class avr_pass_casesi : public rtl_opt_pass
{
public:
  avr_pass_casesi (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_casesi, ctxt)
  {
    this->name = name;
  }

  bool gate (function *) final override
  {
    return optimize > 0;
  }

  unsigned int execute (function *) final override;
}; // avr_pass_casesi


/* Make one parallel insn with all the patterns from insns i[0]..i[5].  */

static rtx_insn *
avr_parallel_insn_from_insns (rtx_insn *i[5])
{
  rtvec vec = gen_rtvec (5, PATTERN (i[0]), PATTERN (i[1]), PATTERN (i[2]),
			 PATTERN (i[3]), PATTERN (i[4]));
  start_sequence ();
  emit (gen_rtx_PARALLEL (VOIDmode, vec));
  rtx_insn *insn = get_insns ();
  end_sequence ();

  return insn;
}


/* Return true if we see an insn stream generated by casesi expander together
   with an extension to SImode of the switch value.

   If this is the case, fill in the insns from casesi to INSNS[1..5] and
   the SImode extension to INSNS[0].  Moreover, extract the operands of
   pattern casesi_<mode>_sequence forged from the sequence to recog_data.  */

static bool
avr_is_casesi_sequence (basic_block bb, rtx_insn *insn, rtx_insn *insns[5])
{
  rtx set_4, set_0;

  /* A first and quick test for a casesi sequences.  As a side effect of
     the test, harvest respective insns to INSNS[0..4].  */

  if (!(JUMP_P (insns[4] = insn)
	// casesi is the only insn that comes up with UNSPEC_INDEX_JMP,
	// hence the following test ensures that we are actually dealing
	// with code from casesi.
	&& (set_4 = single_set (insns[4]))
	&& UNSPEC == GET_CODE (SET_SRC (set_4))
	&& UNSPEC_INDEX_JMP == XINT (SET_SRC (set_4), 1)

	&& (insns[3] = prev_real_insn (insns[4]))
	&& (insns[2] = prev_real_insn (insns[3]))
	&& (insns[1] = prev_real_insn (insns[2]))

	// Insn prior to casesi.
	&& (insns[0] = prev_real_insn (insns[1]))
	&& (set_0 = single_set (insns[0]))
	&& extend_operator (SET_SRC (set_0), SImode)))
    {
      return false;
    }

  if (dump_file)
    {
      fprintf (dump_file, ";; Sequence from casesi in "
	       "[bb %d]:\n\n", bb->index);
      for (int i = 0; i < 5; i++)
	print_rtl_single (dump_file, insns[i]);
    }

  /* We have to deal with quite some operands.  Extracting them by hand
     would be tedious, therefore wrap the insn patterns into a parallel,
     run recog against it and then use insn extract to get the operands. */

  rtx_insn *xinsn = avr_parallel_insn_from_insns (insns);

  INSN_CODE (xinsn) = recog (PATTERN (xinsn), xinsn, NULL /* num_clobbers */);

  /* Failing to recognize means that someone changed the casesi expander or
     that some passes prior to this one performed some unexpected changes.
     Gracefully drop such situations instead of aborting.  */

  if (INSN_CODE (xinsn) < 0)
    {
      if (dump_file)
	fprintf (dump_file, ";; Sequence not recognized, giving up.\n\n");

      return false;
    }

  gcc_assert (CODE_FOR_casesi_qi_sequence == INSN_CODE (xinsn)
	      || CODE_FOR_casesi_hi_sequence == INSN_CODE (xinsn));

  extract_insn (xinsn);

  // Assert on the anatomy of xinsn's operands we are going to work with.

  gcc_assert (recog_data.n_operands == 12);
  gcc_assert (recog_data.n_dups == 3);

  if (dump_file)
    {
      fprintf (dump_file, ";; Operands extracted:\n");
      for (int i = 0; i < recog_data.n_operands; i++)
	avr_fdump (dump_file, ";; $%d = %r\n", i, recog_data.operand[i]);
      fprintf (dump_file, "\n");
    }

  return true;
}


/* INSNS[1..4] is a sequence as generated by casesi and INSNS[0] is an
   extension of an 8-bit or 16-bit integer to SImode.  XOP contains the
   operands of INSNS as extracted by insn_extract from pattern
   casesi_<mode>_sequence:

      $0: SImode reg switch value as result of $10.
      $1: Negative of smallest index in switch.
      $2: Number of entries in switch.
      $3: Label to table.
      $4: Label if out-of-bounds.
      $5: $0 + $1.
      $6: 3-byte PC: subreg:HI ($5) + label_ref ($3)
	  2-byte PC: subreg:HI ($5)
      $7: HI reg index into table (Z or pseudo)
      $8: Z or scratch:HI (to be clobbered)
      $9: R24 or const0_rtx (to be clobbered)
      $10: Extension to SImode of an 8-bit or 16-bit integer register $11.
      $11: QImode or HImode register input of $10.

   Try to optimize this sequence, i.e. use the original HImode / QImode
   switch value instead of SImode.  */

static void
avr_optimize_casesi (rtx_insn *insns[5], rtx *xop)
{
  // Original mode of the switch value; this is QImode or HImode.
  machine_mode mode = GET_MODE (xop[11]);

  // How the original switch value was extended to SImode; this is
  // SIGN_EXTEND or ZERO_EXTEND.
  rtx_code code = GET_CODE (xop[10]);

  // Lower index, upper index (plus one) and range of case calues.
  HOST_WIDE_INT low_idx = -INTVAL (xop[1]);
  HOST_WIDE_INT num_idx = INTVAL (xop[2]);
  HOST_WIDE_INT hig_idx = low_idx + num_idx;

  // Maximum ranges of (un)signed QImode resp. HImode.
  unsigned umax = QImode == mode ? 0xff : 0xffff;
  int imax = QImode == mode ? 0x7f : 0x7fff;
  int imin = -imax - 1;

  // Testing the case range and whether it fits into the range of the
  // (un)signed mode.  This test should actually always pass because it
  // makes no sense to have case values outside the mode range.  Notice
  // that case labels which are unreachable because they are outside the
  // mode of the switch value (e.g. "case -1" for uint8_t) have already
  // been thrown away by the middle-end.

  if (SIGN_EXTEND == code
      && low_idx >= imin
      && hig_idx <= imax)
    {
      // ok
    }
  else if (ZERO_EXTEND == code
	   && low_idx >= 0
	   && (unsigned) hig_idx <= umax)
    {
      // ok
    }
  else
    {
      if (dump_file)
	fprintf (dump_file, ";; Case ranges too big, giving up.\n\n");
      return;
    }

  // Do normalization of switch value $10 and out-of-bound check in its
  // original mode instead of in SImode.  Use a newly created pseudo.
  // This will replace insns[1..2].

  start_sequence ();

  rtx reg = copy_to_mode_reg (mode, xop[11]);

  rtx (*gen_add)(rtx,rtx,rtx) = QImode == mode ? gen_addqi3 : gen_addhi3;
  rtx (*gen_cbranch)(rtx,rtx,rtx,rtx)
    = QImode == mode ? gen_cbranchqi4 : gen_cbranchhi4;

  emit_insn (gen_add (reg, reg, gen_int_mode (-low_idx, mode)));
  rtx op0 = reg; rtx op1 = gen_int_mode (num_idx, mode);
  rtx labelref = copy_rtx (xop[4]);
  rtx xbranch = gen_cbranch (gen_rtx_fmt_ee (GTU, VOIDmode, op0, op1),
			     op0, op1, labelref);
  rtx_insn *cbranch = emit_jump_insn (xbranch);
  JUMP_LABEL (cbranch) = xop[4];
  ++LABEL_NUSES (xop[4]);

  rtx_insn *seq1 = get_insns ();
  rtx_insn *last1 = get_last_insn ();
  end_sequence ();

  emit_insn_after (seq1, insns[2]);

  // After the out-of-bounds test and corresponding branch, use a
  // 16-bit index.  If QImode is used, extend it to HImode first.
  // This will replace insns[4].

  start_sequence ();

  if (QImode == mode)
    reg = force_reg (HImode, gen_rtx_fmt_e (code, HImode, reg));

  rtx pat_4 = AVR_3_BYTE_PC
    ? gen_movhi (xop[7], reg)
    : gen_addhi3 (xop[7], reg, gen_rtx_LABEL_REF (VOIDmode, xop[3]));

  emit_insn (pat_4);

  rtx_insn *seq2 = get_insns ();
  rtx_insn *last2 = get_last_insn ();
  end_sequence ();

  emit_insn_after (seq2, insns[3]);

  if (dump_file)
    {
      fprintf (dump_file, ";; New insns: ");

      for (rtx_insn *insn = seq1; ; insn = NEXT_INSN (insn))
	{
	  fprintf (dump_file, "%d, ", INSN_UID (insn));
	  if (insn == last1)
	    break;
	}
      for (rtx_insn *insn = seq2; ; insn = NEXT_INSN (insn))
	{
	  fprintf (dump_file, "%d%s", INSN_UID (insn),
		   insn == last2 ? ".\n\n" : ", ");
	  if (insn == last2)
	    break;
	}

      fprintf (dump_file, ";; Deleting insns: %d, %d, %d.\n\n",
	       INSN_UID (insns[1]), INSN_UID (insns[2]), INSN_UID (insns[3]));
    }

  // Pseudodelete the SImode and subreg of SImode insns.  We don't care
  // about the extension insns[0]: Its result is now unused and other
  // passes will clean it up.

  SET_INSN_DELETED (insns[1]);
  SET_INSN_DELETED (insns[2]);
  SET_INSN_DELETED (insns[3]);
}


unsigned int
avr_pass_casesi::execute (function *func)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, func)
    {
      rtx_insn *insn, *insns[5];

      FOR_BB_INSNS (bb, insn)
	{
	  if (avr_is_casesi_sequence (bb, insn, insns))
	    {
	      avr_optimize_casesi (insns, recog_data.operand);
	    }
	}
    }

  return 0;
}

} // anonymous namespace

/* Perform some extra checks on operands of casesi_<mode>_sequence.
   Not all operand dependencies can be described by means of predicates.
   This function performs left over checks and should always return true.
   Returning false means that someone changed the casesi expander but did
   not adjust casesi_<mode>_sequence.  */

bool
avr_casei_sequence_check_operands (rtx *xop)
{
  rtx sub_5 = NULL_RTX;

  if (AVR_HAVE_EIJMP_EICALL
      // The last clobber op of the tablejump.
      && xop[9] == all_regs_rtx[REG_24])
    {
      // $6 is: (subreg:SI ($5) 0)
      sub_5 = xop[6];
    }

  if (!AVR_HAVE_EIJMP_EICALL
      // $6 is: (plus:HI (subreg:SI ($5) 0)
      //		 (label_ref ($3)))
      && PLUS == GET_CODE (xop[6])
      && LABEL_REF == GET_CODE (XEXP (xop[6], 1))
      && rtx_equal_p (xop[3], XEXP (XEXP (xop[6], 1), 0))
      // The last clobber op of the tablejump.
      && xop[9] == const0_rtx)
    {
      sub_5 = XEXP (xop[6], 0);
    }

  if (sub_5
      && SUBREG_P (sub_5)
      && SUBREG_BYTE (sub_5) == 0
      && rtx_equal_p (xop[5], SUBREG_REG (sub_5)))
    return true;

  if (dump_file)
    fprintf (dump_file, "\n;; Failed condition for casesi_<mode>_sequence\n\n");

  return false;
}

namespace
{


//////////////////////////////////////////////////////////////////////////////
// Find more POST_INC and PRE_DEC cases.

static const pass_data avr_pass_data_fuse_add =
{
  RTL_PASS,	    // type
  "",		    // name (will be patched)
  OPTGROUP_NONE,    // optinfo_flags
  TV_MACH_DEP,	    // tv_id
  0,		    // properties_required
  0,		    // properties_provided
  0,		    // properties_destroyed
  0,		    // todo_flags_start
  TODO_df_finish    // todo_flags_finish
};

class avr_pass_fuse_add : public rtl_opt_pass
{
public:
  avr_pass_fuse_add (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_fuse_add, ctxt)
  {
    this->name = name;
  }

  // Cloning is required because we are running one instance of the pass
  // before peephole2. and a second one after cprop_hardreg.
  opt_pass * clone () final override
  {
    return make_avr_pass_fuse_add (m_ctxt);
  }

  unsigned int execute (function *func) final override
  {
    func->machine->n_avr_fuse_add_executed += 1;
    n_avr_fuse_add_executed = func->machine->n_avr_fuse_add_executed;

    if (optimize && avropt_fuse_add > 0)
      return execute1 (func);
    return 0;
  }

  unsigned int execute1 (function *);

  struct Some_Insn
  {
    rtx_insn *insn = nullptr;
    rtx dest, src;
    bool valid () const { return insn != nullptr; }
    void set_deleted ()
    {
      gcc_assert (insn);
      SET_INSN_DELETED (insn);
      insn = nullptr;
    }
  };

  // If .insn is not NULL, then this is a  reg:HI += const_int
  // of an address register.
  struct Add_Insn : Some_Insn
  {
    rtx addend;
    int regno;
    Add_Insn () {}
    Add_Insn (rtx_insn *insn);
  };

  // If .insn is not NULL, then this sets an address register
  // to a constant value.
  struct Ldi_Insn : Some_Insn
  {
    int regno;
    Ldi_Insn () {}
    Ldi_Insn (rtx_insn *insn);
  };

  // If .insn is not NULL, then this is a load or store insn where the
  // address is REG or POST_INC with an address register.
  struct Mem_Insn : Some_Insn
  {
    rtx reg_or_0, mem, addr, addr_reg;
    int addr_regno;
    rtx_code addr_code;
    machine_mode mode;
    addr_space_t addr_space;
    bool store_p, volatile_p;
    Mem_Insn () {}
    Mem_Insn (rtx_insn *insn);
  };

  rtx_insn *fuse_ldi_add (Ldi_Insn &prev_ldi, Add_Insn &add);
  rtx_insn *fuse_add_add (Add_Insn &prev_add, Add_Insn &add);
  rtx_insn *fuse_add_mem (Add_Insn &prev_add, Mem_Insn &mem);
  rtx_insn *fuse_mem_add (Mem_Insn &prev_mem, Add_Insn &add);
}; // avr_pass_fuse_add


/* Describe properties of AVR's indirect load and store instructions
   LD, LDD, ST, STD, LPM, ELPM depending on register number, volatility etc.
   Rules for "volatile" accesses are:

	 | Xmega	   |  non-Xmega
   ------+-----------------+----------------
   load  | read LSB first  | read LSB first
   store | write LSB first | write MSB first
*/

struct AVR_LdSt_Props
{
  bool has_postinc, has_predec, has_ldd;
  // The insn printers will use POST_INC or PRE_DEC addressing, no matter
  // what adressing modes we are feeding into them.
  bool want_postinc, want_predec;

  AVR_LdSt_Props (int regno, bool store_p, bool volatile_p, addr_space_t as)
  {
    bool generic_p = ADDR_SPACE_GENERIC_P (as);
    bool flashx_p = (! generic_p
		     && as != ADDR_SPACE_MEMX && as != ADDR_SPACE_FLASHX);
    has_postinc = generic_p || (flashx_p && regno == REG_Z);
    has_predec = generic_p;
    has_ldd = ! AVR_TINY && generic_p && (regno == REG_Y || regno == REG_Z);
    want_predec  = volatile_p && generic_p && ! AVR_XMEGA && store_p;
    want_postinc = volatile_p && generic_p && (AVR_XMEGA || ! store_p);
    want_postinc |= flashx_p && regno == REG_Z;
  }

  AVR_LdSt_Props (const avr_pass_fuse_add::Mem_Insn &m)
    : AVR_LdSt_Props (m.addr_regno, m.store_p, m.volatile_p, m.addr_space)
  {
    gcc_assert (m.valid ());
  }
};


/* Emit a single_set that clobbers REG_CC.  */

static rtx_insn *
emit_move_ccc (rtx dest, rtx src)
{
  return emit_insn (gen_gen_move_clobbercc (dest, src));
}


/* Emit a single_set that clobbers REG_CC after insn AFTER.  */

static rtx_insn *
emit_move_ccc_after (rtx dest, rtx src, rtx_insn *after)
{
  return emit_insn_after (gen_gen_move_clobbercc (dest, src), after);
}

static bool
reg_seen_between_p (const_rtx reg, const rtx_insn *from, const rtx_insn *to)
{
  return (reg_used_between_p (reg, from, to)
	  || reg_set_between_p (reg, from, to));
}


static void
avr_maybe_adjust_cfa (rtx_insn *insn, rtx reg, int addend)
{
  if (addend
      && frame_pointer_needed
      && REGNO (reg) == FRAME_POINTER_REGNUM
      && avropt_fuse_add == 3)
    {
      rtx plus = plus_constant (Pmode, reg, addend);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_ADJUST_CFA, gen_rtx_SET (reg, plus));
    }
}


// If successful, this represents a SET of a pointer register to a constant.
avr_pass_fuse_add::Ldi_Insn::Ldi_Insn (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return;

  src = SET_SRC (set);
  dest = SET_DEST (set);

  if (REG_P (dest)
      && GET_MODE (dest) == Pmode
      && IN_RANGE (regno = REGNO (dest), REG_X, REG_Z)
      && CONSTANT_P (src))
    {
      this->insn = insn;
    }
}

// If successful, this represents a PLUS with CONST_INT of a pointer
// register X, Y or Z.  Otherwise, the object is not valid().
avr_pass_fuse_add::Add_Insn::Add_Insn (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return;

  src = SET_SRC (set);
  dest = SET_DEST (set);
  if (REG_P (dest)
      // We are only interested in PLUSes that change address regs.
      && GET_MODE (dest) == Pmode
      && IN_RANGE (regno = REGNO (dest), REG_X, REG_Z)
      && PLUS == GET_CODE (src)
      && rtx_equal_p (XEXP (src, 0), dest)
      && CONST_INT_P (XEXP (src, 1)))
    {
      // This is reg:HI += const_int.
      addend = XEXP (src, 1);
      this->insn = insn;
    }
}

// If successful, this represents a load or store insn where the addressing
// mode uses pointer register X, Y or Z.  Otherwise, the object is not valid().
avr_pass_fuse_add::avr_pass_fuse_add::Mem_Insn::Mem_Insn (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return;

  src = SET_SRC (set);
  dest = SET_DEST (set);
  mode = GET_MODE (dest);

  if (MEM_P (dest)
      && (REG_P (src) || src == CONST0_RTX (mode)))
    {
      reg_or_0 = src;
      mem = dest;
    }
  else if (REG_P (dest) && MEM_P (src))
    {
      reg_or_0 = dest;
      mem = src;
    }
  else
    return;

  if (avr_mem_memx_p (mem)
      || avr_load_libgcc_p (mem))
    return;

  addr = XEXP (mem, 0);
  addr_code = GET_CODE (addr);

  if (addr_code == REG)
    addr_reg = addr;
  else if (addr_code == POST_INC || addr_code == PRE_DEC)
    addr_reg = XEXP (addr, 0);
  else
    return;

  addr_regno = REGNO (addr_reg);

  if (avropt_fuse_add == 2
      && frame_pointer_needed
      && addr_regno == FRAME_POINTER_REGNUM)
    MEM_VOLATILE_P (mem) = 0;

  if (reg_overlap_mentioned_p (reg_or_0, addr) // Can handle CONSTANT_P.
      || addr_regno > REG_Z
      || avr_mem_memx_p (mem)
      // The following optimizations only handle REG and POST_INC,
      // so that's all what we allow here.
      || (addr_code != REG && addr_code != POST_INC))
    return;

  addr_space = MEM_ADDR_SPACE (mem);
  volatile_p = MEM_VOLATILE_P (mem);
  store_p = MEM_P (dest);

  // Turn this "valid".
  this->insn = insn;
}

/* Try to combine a Ldi insn with a PLUS CONST_INT addend to one Ldi insn.
   If LDI is valid, then it precedes ADD in the same block.
   When a replacement is found, a new insn is emitted and the old insns
   are pseudo-deleted.  The returned insn is the point where the calling
   scanner should continue.  When no replacement is found, nullptr is
   returned and nothing changed.  */

rtx_insn *
avr_pass_fuse_add::fuse_ldi_add (Ldi_Insn &ldi, Add_Insn &add)
{
  if (! ldi.valid ()
      || reg_seen_between_p (ldi.dest, ldi.insn, add.insn))
    {
      // If something is between the Ldi and the current insn, we can
      // set the Ldi invalid to speed future scans.
      return ldi.insn = nullptr;
    }

  // Found a Ldi with const and a PLUS insns in the same BB,
  // and with no interfering insns between them.

  // Emit new Ldi with the sum of the original offsets after the old Ldi.
  rtx xval = plus_constant (Pmode, ldi.src, INTVAL (add.addend));

  rtx_insn *insn = emit_move_ccc_after (ldi.dest, xval, ldi.insn);
  avr_dump (";; new Ldi[%d] insn %d after %d: R%d = %r\n\n", ldi.regno,
	    INSN_UID (insn), INSN_UID (ldi.insn), ldi.regno, xval);

  rtx_insn *next = NEXT_INSN (add.insn);
  ldi.set_deleted ();
  add.set_deleted ();

  return next;
}

/* Try to combine two PLUS insns with CONST_INT addend to one such insn.
   If PREV_ADD is valid, then it precedes ADD in the same basic block.
   When a replacement is found, a new insn is emitted and the old insns
   are pseudo-deleted.  The returned insn is the point where the calling
   scanner should continue.  When no replacement is found, nullptr is
   returned and nothing changed.  */

rtx_insn *
avr_pass_fuse_add::fuse_add_add (Add_Insn &prev_add, Add_Insn &add)
{
  if (! prev_add.valid ()
      || reg_seen_between_p (add.dest, prev_add.insn, add.insn))
    {
      // If something is between the previous Add and the current insn,
      // we can set the previous Add invalid to speed future scans.
      return prev_add.insn = nullptr;
    }

  // Found two PLUS insns in the same BB, and with no interfering
  // insns between them.
  rtx plus = plus_constant (Pmode, add.src, INTVAL (prev_add.addend));

  rtx_insn *next;
  if (REG_P (plus))
    {
      avr_dump (";; Add[%d] from %d annihilates %d\n\n", add.regno,
		INSN_UID (prev_add.insn), INSN_UID (add.insn));
      next = NEXT_INSN (add.insn);
    }
  else
    {
      // Emit after the current insn, so that it will be picked
      // up as next valid Add insn.
      next = emit_move_ccc_after (add.dest, plus, add.insn);
      avr_dump (";; #1 new Add[%d] insn %d after %d: R%d += %d\n\n",
		add.regno, INSN_UID (next), INSN_UID (add.insn),
		add.regno, (int) INTVAL (XEXP (plus, 1)));
      gcc_assert (GET_CODE (plus) == PLUS);
    }

  add.set_deleted ();
  prev_add.set_deleted ();

  return next;
}

/* Try to combine a PLUS of the address register with a load or store insn.
   If ADD is valid, then it precedes MEM in the same basic block.
   When a replacement is found, a new insn is emitted and the old insns
   are pseudo-deleted.  The returned insn is the point where the calling
   scanner should continue.  When no replacement is found, nullptr is
   returned and nothing changed.  */

rtx_insn *
avr_pass_fuse_add::fuse_add_mem (Add_Insn &add, Mem_Insn &mem)
{
  if (! add.valid ()
      || reg_seen_between_p (add.dest, add.insn, mem.insn))
    {
      // If something is between the Add and the current insn, we can
      // set the Add invalid to speed future scans.
      return add.insn = nullptr;
    }

  AVR_LdSt_Props ap { mem };

  int msize = GET_MODE_SIZE (mem.mode);

  // The mem insn really wants PRE_DEC.
  bool case1 = ((mem.addr_code == REG || mem.addr_code == POST_INC)
		&& msize > 1 && ap.want_predec && ! ap.has_ldd);

  // The offset can be consumed by a PRE_DEC.
  bool case2 = (- INTVAL (add.addend) == msize
		&& (mem.addr_code == REG || mem.addr_code == POST_INC)
		&& ap.has_predec && ! ap.want_postinc);

  if (! case1 && ! case2)
    return nullptr;

  // Change from REG or POST_INC to PRE_DEC.
  rtx xmem = change_address (mem.mem, mem.mode,
			     gen_rtx_PRE_DEC (Pmode, mem.addr_reg));
  rtx dest = mem.store_p ? xmem : mem.reg_or_0;
  rtx src  = mem.store_p ? mem.reg_or_0 : xmem;

  rtx_insn *next = emit_move_ccc_after (dest, src, mem.insn);
  add_reg_note (next, REG_INC, mem.addr_reg);
  avr_dump (";; new Mem[%d] insn %d after %d: %r = %r\n\n", mem.addr_regno,
	    INSN_UID (next), INSN_UID (mem.insn), dest, src);

  // Changing REG or POST_INC -> PRE_DEC means that the addend before
  // the memory access must be increased by the size of the access,
  rtx plus = plus_constant (Pmode, add.src, msize);
  if (! REG_P (plus))
    {
      rtx_insn *insn = emit_move_ccc_after (add.dest, plus, add.insn);
      avr_dump (";; #2 new Add[%d] insn %d after %d: R%d += %d\n\n",
		add.regno, INSN_UID (insn), INSN_UID (add.insn),
		add.regno, (int) INTVAL (XEXP (plus, 1)));
      gcc_assert (GET_CODE (plus) == PLUS);
    }
  else
    avr_dump (";; Add[%d] insn %d consumed into %d\n\n",
	      add.regno, INSN_UID (add.insn), INSN_UID (next));

  // Changing POST_INC -> PRE_DEC means that the addend after the mem has to be
  // the size of the access.  The hope is that this new add insn may be unused.
  if (mem.addr_code == POST_INC)
    {
      plus = plus_constant (Pmode, add.dest, msize);
      rtx_insn *next2 = emit_move_ccc_after (add.dest, plus, next);
      avr_dump (";; #3 new Add[%d] insn %d after %d: R%d += %d\n\n", add.regno,
		INSN_UID (next2), INSN_UID (next), add.regno, msize);
      next = next2;
    }

  add.set_deleted ();
  mem.set_deleted ();

  return next;
}

/* Try to combine a load or store insn with a PLUS of the address register.
   If MEM is valid, then it precedes ADD in the same basic block.
   When a replacement is found, a new insn is emitted and the old insns
   are pseudo-deleted.  The returned insn is the point where the calling
   scanner should continue.  When no replacement is found, nullptr is
   returned and nothing changed.  */

rtx_insn *
avr_pass_fuse_add::fuse_mem_add (Mem_Insn &mem, Add_Insn &add)
{
  if (! mem.valid ()
      || reg_seen_between_p (add.dest, mem.insn, add.insn))
    {
      // If something is between the Mem and the current insn, we can
      // set the Mem invalid to speed future scans.
      return mem.insn = nullptr;
    }

  AVR_LdSt_Props ap { mem };

  int msize = GET_MODE_SIZE (mem.mode);

  // The add insn can be consumed by a POST_INC.
  bool case1 = (mem.addr_code == REG
		&& INTVAL (add.addend) == msize
		&& ap.has_postinc && ! ap.want_predec);

  // There are cases where even a partial consumption of the offset is better.
  // This are the cases where no LD+offset addressing is available, because
  // the address register is obviously used after the mem insn, and a mem insn
  // with REG addressing mode will have to restore the address.
  bool case2 = (mem.addr_code == REG
		&& msize > 1 && ap.want_postinc && ! ap.has_ldd);

  if (! case1 && ! case2)
    return nullptr;

  // Change addressing mode from REG to POST_INC.
  rtx xmem = change_address (mem.mem, mem.mode,
			     gen_rtx_POST_INC (Pmode, mem.addr_reg));
  rtx dest = mem.store_p ? xmem : mem.reg_or_0;
  rtx src  = mem.store_p ? mem.reg_or_0 : xmem;

  rtx_insn *insn = emit_move_ccc_after (dest, src, mem.insn);
  add_reg_note (insn, REG_INC, mem.addr_reg);
  avr_dump (";; new Mem[%d] insn %d after %d: %r = %r\n\n", add.regno,
	    INSN_UID (insn), INSN_UID (mem.insn), dest, src);

  rtx_insn *next = NEXT_INSN (add.insn);

  // Changing REG -> POST_INC means that the post addend must be
  // decreased by the size of the access.
  rtx plus = plus_constant (Pmode, add.src, -msize);
  if (! REG_P (plus))
    {
      next = emit_move_ccc_after (mem.addr_reg, plus, add.insn);
      avr_dump (";; #4 new Add[%d] insn %d after %d: R%d += %d\n\n",
		add.regno, INSN_UID (next), INSN_UID (add.insn),
		add.regno, (int) INTVAL (XEXP (plus, 1)));
      gcc_assert (GET_CODE (plus) == PLUS);
    }
  else
    avr_dump (";; Add[%d] insn %d consumed into %d\n\n",
	      add.regno, INSN_UID (add.insn), INSN_UID (insn));

  add.set_deleted ();
  mem.set_deleted ();

  return next;
}

/* Try to post-reload combine PLUS with CONST_INt of pointer registers with:
   - Sets to a constant address.
   - PLUS insn of that kind.
   - Indirect loads and stores.
   In almost all cases, combine opportunities arise from the preparation
   done by `avr_split_fake_addressing_move', but in some rare cases combinations
   are found for the ordinary cores, too.
      As we consider at most one Mem insn per try, there may still be missed
   optimizations like  POST_INC + PLUS + POST_INC  might be performed
   as  PRE_DEC + PRE_DEC  for two adjacent locations.  */

unsigned int
avr_pass_fuse_add::execute1 (function *func)
{
  df_note_add_problem ();
  df_analyze ();

  int n_add = 0, n_mem = 0, n_ldi = 0;
  basic_block bb;

  FOR_EACH_BB_FN (bb, func)
    {
      Ldi_Insn prev_ldi_insns[REG_32];
      Add_Insn prev_add_insns[REG_32];
      Mem_Insn prev_mem_insns[REG_32];
      rtx_insn *insn, *curr;

      avr_dump ("\n;; basic block %d\n\n", bb->index);

      FOR_BB_INSNS_SAFE (bb, insn, curr)
	{
	  rtx_insn *next = nullptr;
	  Ldi_Insn ldi_insn { insn };
	  Add_Insn add_insn { insn };
	  Mem_Insn mem_insn { insn };

	  if (add_insn.valid ())
	    {
	      // Found reg:HI += const_int
	      avr_dump (";; insn %d: Add[%d]: R%d += %d\n\n",
			INSN_UID (add_insn.insn), add_insn.regno,
			add_insn.regno, (int) INTVAL (add_insn.addend));
	      Ldi_Insn &prev_ldi_insn = prev_ldi_insns[add_insn.regno];
	      Add_Insn &prev_add_insn = prev_add_insns[add_insn.regno];
	      Mem_Insn &prev_mem_insn = prev_mem_insns[add_insn.regno];
	      if ((next = fuse_ldi_add (prev_ldi_insn, add_insn)))
		curr = next, n_ldi += 1;
	      else if ((next = fuse_add_add (prev_add_insn, add_insn)))
		curr = next, n_add += 1;
	      else if ((next = fuse_mem_add (prev_mem_insn, add_insn)))
		curr = next, n_mem += 1;
	      else
		prev_add_insn = add_insn;
	    }
	  else if (mem_insn.valid ())
	    {
	      int addr_regno = REGNO (mem_insn.addr_reg);
	      avr_dump (";; insn %d: Mem[%d]: %r = %r\n\n",
			INSN_UID (mem_insn.insn), addr_regno,
			mem_insn.dest, mem_insn.src);
	      Add_Insn &prev_add_insn = prev_add_insns[addr_regno];
	      if ((next = fuse_add_mem (prev_add_insn, mem_insn)))
		curr = next, n_mem += 1;
	      else
		prev_mem_insns[addr_regno] = mem_insn;
	    }
	  else if (ldi_insn.valid ())
	    {
	      if (! CONST_INT_P (ldi_insn.src))
		avr_dump (";; insn %d: Ldi[%d]: R%d = %r\n\n",
			  INSN_UID (ldi_insn.insn), ldi_insn.regno,
			  ldi_insn.regno, ldi_insn.src);
	      prev_ldi_insns[ldi_insn.regno] = ldi_insn;
	    }
	} // for insns
    } // for BBs

  avr_dump (";; Function %f: Found %d changes: %d ldi, %d add, %d mem.\n",
	    n_ldi + n_add + n_mem, n_ldi, n_add, n_mem);

  return 0;
}



//////////////////////////////////////////////////////////////////////////////
// Split shift insns after peephole2 / befor avr-fuse-move.

static const pass_data avr_pass_data_split_after_peephole2 =
{
  RTL_PASS,	    // type
  "",		    // name (will be patched)
  OPTGROUP_NONE,    // optinfo_flags
  TV_DF_SCAN,	    // tv_id
  0,		    // properties_required
  0,		    // properties_provided
  0,		    // properties_destroyed
  0,		    // todo_flags_start
  0		    // todo_flags_finish
};

class avr_pass_split_after_peephole2 : public rtl_opt_pass
{
public:
  avr_pass_split_after_peephole2 (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_split_after_peephole2, ctxt)
  {
    this->name = name;
  }

  unsigned int execute (function *) final override
  {
    if (avr_shift_is_3op ())
      split_all_insns ();
    return 0;
  }

}; // avr_pass_split_after_peephole2

} // anonymous namespace


/* Whether some shift insn alternatives are a `3op' 3-operand insn.
   This 3op alternatives allow the source and the destination register
   of the shift to be different right from the start, because the splitter
   will split the 3op shift into a 3-operand byte shift and a 2-operand
   residual bit shift.  (When the residual shift has an offset of one
   less than the bitsize, then the residual shift is also a 3op insn.)  */

bool
avr_shift_is_3op ()
{
  // Don't split for OPTIMIZE_SIZE_MAX (-Oz).
  // For OPTIMIZE_SIZE_BALANCED (-Os), we still split because
  // the size overhead (if at all) is marginal.

  return (avropt_split_bit_shift
	  && optimize > 0
	  && avr_optimize_size_level () < OPTIMIZE_SIZE_MAX);
}


/* Implement constraints `C2a', `C2l', `C2r' ... `C4a', `C4l', `C4r'.
   Whether we split an N_BYTES shift of code CODE in { ASHIFTRT,
   LSHIFTRT, ASHIFT } into a byte shift and a residual bit shift.  */

bool
avr_split_shift_p (int n_bytes, int offset, rtx_code code)
{
  gcc_assert (n_bytes == 4 || n_bytes == 3 || n_bytes == 2);

  if (! avr_shift_is_3op ()
      || offset % 8 == 0)
    return false;

  if (n_bytes == 4)
    return select<bool>()
      : code == ASHIFT ? IN_RANGE (offset, 9, 31) && offset != 15
      : code == ASHIFTRT ? IN_RANGE (offset, 9, 29) && offset != 15
      : code == LSHIFTRT ? IN_RANGE (offset, 9, 31) && offset != 15
      : bad_case<bool> ();

  if (n_bytes == 3)
    return select<bool>()
      : code == ASHIFT ? IN_RANGE (offset, 9, 23) && offset != 15
      : code == ASHIFTRT ? IN_RANGE (offset, 9, 21) && offset != 15
      : code == LSHIFTRT ? IN_RANGE (offset, 9, 23) && offset != 15
      : bad_case<bool> ();

  if (n_bytes == 2)
    return select<bool>()
      : code == ASHIFT ? IN_RANGE (offset, 9, 15)
      : code == ASHIFTRT ? IN_RANGE (offset, 9, 13)
      : code == LSHIFTRT ? IN_RANGE (offset, 9, 15)
      : bad_case<bool> ();

  return false;
}


/* Emit a DEST = SRC <code> OFF shift of QImode, HImode or PSImode.
   SCRATCH is a QImode d-register, scratch:QI, or NULL_RTX.
   This function is used to emit shifts that have been split into
   a byte shift and a residual bit shift that operates on a mode
   strictly smaller than the original shift.  */

static void
avr_emit_shift (rtx_code code, rtx dest, rtx src, int off, rtx scratch)
{
  const machine_mode mode = GET_MODE (dest);
  const int n_bits = GET_MODE_BITSIZE (mode);
  rtx xoff = GEN_INT (off);

  // Work out which alternatives can handle 3 operands independent
  // of options.

  const bool b8_is_3op = off == 6;

  const bool b16_is_3op = select<bool>()
    : code == ASHIFT ? (satisfies_constraint_C7c (xoff) // 7...12
			// The "C05 C06" alternative of *ashlhi3_const.
			|| (AVR_HAVE_MUL && scratch && (off == 5 || off == 6)))
    : code == LSHIFTRT ? satisfies_constraint_C7c (xoff)
    : code == ASHIFTRT ? off == 7
    : bad_case<bool> ();

  const bool b24_is_3op = select<bool>()
    : code == ASHIFT ? off == 15
    : code == LSHIFTRT ? off == 15
    : code == ASHIFTRT ? false
    : bad_case<bool> ();

  const bool is_3op = (off % 8 == 0
		       || off == n_bits - 1
		       || (code == ASHIFTRT && off == n_bits - 2)
		       || (n_bits == 8 && b8_is_3op)
		       || (n_bits == 16 && b16_is_3op)
		       || (n_bits == 24 && b24_is_3op));
  rtx shift;

  if (is_3op)
    {
      shift = gen_rtx_fmt_ee (code, mode, src, xoff);
    }
  else
    {
      if (REGNO (dest) != REGNO (src))
	emit_valid_move_clobbercc (dest, src);
      shift = gen_rtx_fmt_ee (code, mode, dest, xoff);
    }

  if (n_bits == 8)
    // 8-bit shifts don't have a scratch operand.
    scratch = NULL_RTX;
  else if (! scratch && n_bits == 24)
    // 24-bit shifts always have a scratch operand.
    scratch = gen_rtx_SCRATCH (QImode);

  emit_valid_move_clobbercc (dest, shift, scratch);
}


/* Handle the 4-byte case of avr_split_shift below:
   Split 4-byte shift  DEST = SRC <code> IOFF  into a 3-operand
   byte shift and a residual shift in a smaller mode if possible.
   SCRATCH is a QImode upper scratch register or NULL_RTX.  */

static bool
avr_split_shift4 (rtx dest, rtx src, int ioff, rtx scratch, rtx_code code)
{
  gcc_assert (GET_MODE_SIZE (GET_MODE (dest)) == 4);

  if (code == ASHIFT)
    {
      if (IN_RANGE (ioff, 25, 31))
	{
	  rtx dst8 = avr_byte (dest, 3);
	  rtx src8 = avr_byte (src, 0);
	  avr_emit_shift (code, dst8, src8, ioff - 24, NULL_RTX);
	  emit_valid_move_clobbercc (avr_byte (dest, 2), const0_rtx);
	  emit_valid_move_clobbercc (avr_word (dest, 0), const0_rtx);
	  return true;
	}
      else if (IN_RANGE (ioff, 17, 23))
	{
	  rtx dst16 = avr_word (dest, 2);
	  rtx src16 = avr_word (src, 0);
	  avr_emit_shift (code, dst16, src16, ioff - 16, scratch);
	  emit_valid_move_clobbercc (avr_word (dest, 0), const0_rtx);
	  return true;
	}
      // ...the 9...14 cases are only handled by define_split because
      // for now, we don't exploit that the low byte is zero.
    }
  else if (code == ASHIFTRT
	   || code == LSHIFTRT)
    {
      if (IN_RANGE (ioff, 25, 30 + (code == LSHIFTRT)))
	{
	  rtx dst8 = avr_byte (dest, 0);
	  rtx src8 = avr_byte (src, 3);
	  avr_emit_shift (code, dst8, src8, ioff - 24, NULL_RTX);
	  if (code == ASHIFTRT)
	    {
	      rtx signs = avr_byte (dest, 1);
	      avr_emit_shift (code, signs, src8, 7, NULL_RTX);
	      emit_valid_move_clobbercc (avr_byte (dest, 2), signs);
	      emit_valid_move_clobbercc (avr_byte (dest, 3), signs);
	    }
	  else
	    {
	      emit_valid_move_clobbercc (avr_byte (dest, 1), const0_rtx);
	      emit_valid_move_clobbercc (avr_word (dest, 2), const0_rtx);
	    }
	  return true;
	}
      else if (IN_RANGE (ioff, 17, 23))
	{
	  rtx dst16 = avr_word (dest, 0);
	  rtx src16 = avr_word (src, 2);
	  avr_emit_shift (code, dst16, src16, ioff - 16, scratch);
	  if (code == ASHIFTRT)
	    {
	      rtx msb = avr_byte (src, 3);
	      rtx signs = avr_byte (dest, 2);
	      avr_emit_shift (code, signs, msb, 7, NULL_RTX);
	      emit_valid_move_clobbercc (avr_byte (dest, 3), signs);
	    }
	  else
	    emit_valid_move_clobbercc (avr_word (dest, 2), const0_rtx);

	  return true;
	}
      else if (IN_RANGE (ioff, 9, 15))
	{
	  avr_emit_shift (code, dest, src, 8, scratch);
	  rtx dst24 = avr_chunk (PSImode, dest, 0);
	  rtx src24 = avr_chunk (PSImode, dest, 0);
	  avr_emit_shift (code, dst24, src24, ioff - 8, scratch);
	  return true;
	}
    }
  else
    gcc_unreachable ();

  return false;
}


/* Handle the 3-byte case of avr_split_shift below:
   Split 3-byte shift  DEST = SRC <code> IOFF  into a 3-operand
   byte shift and a residual shift in a smaller mode if possible.
   SCRATCH is a QImode upper scratch register or NULL_RTX.  */

static bool
avr_split_shift3 (rtx dest, rtx src, int ioff, rtx scratch, rtx_code code)
{
  gcc_assert (GET_MODE_SIZE (GET_MODE (dest)) == 3);

  if (code == ASHIFT)
    {
      if (IN_RANGE (ioff, 17, 23))
	{
	  rtx dst8 = avr_byte (dest, 2);
	  rtx src8 = avr_byte (src, 0);
	  avr_emit_shift (code, dst8, src8, ioff - 16, NULL_RTX);
	  emit_valid_move_clobbercc (avr_word (dest, 0), const0_rtx);
	  return true;
	}
      // ...the 9...14 cases are only handled by define_split because
      // for now, we don't exploit that the low byte is zero.
    }
  else if (code == ASHIFTRT
	   || code == LSHIFTRT)
    {
      if (IN_RANGE (ioff, 17, 22 + (code == LSHIFTRT)))
	{
	  rtx dst8 = avr_byte (dest, 0);
	  rtx src8 = avr_byte (src, 2);
	  avr_emit_shift (code, dst8, src8, ioff - 16, NULL_RTX);
	  if (code == ASHIFTRT)
	    {
	      rtx signs = avr_byte (dest, 1);
	      avr_emit_shift (code, signs, src8, 7, NULL_RTX);
	      emit_valid_move_clobbercc (avr_byte (dest, 2), signs);
	    }
	  else
	    {
	      emit_valid_move_clobbercc (avr_byte (dest, 1), const0_rtx);
	      emit_valid_move_clobbercc (avr_byte (dest, 2), const0_rtx);
	    }
	  return true;
	}
      else if (IN_RANGE (ioff, 9, 15))
	{
	  avr_emit_shift (code, dest, src, 8, scratch);
	  rtx dst16 = avr_chunk (HImode, dest, 0);
	  rtx src16 = avr_chunk (HImode, dest, 0);
	  avr_emit_shift (code, dst16, src16, ioff - 8, scratch);
	  return true;
	}
    }
  else
    gcc_unreachable ();

  return false;
}


/* Handle the 2-byte case of avr_split_shift below:
   Split 2-byte shift  DEST = SRC <code> IOFF  into a 3-operand
   byte shift and a residual shift in a smaller mode if possible.
   SCRATCH is a QImode upper scratch register or NULL_RTX.  */

static bool
avr_split_shift2 (rtx dest, rtx src, int ioff, rtx /*scratch*/, rtx_code code)
{
  gcc_assert (GET_MODE_SIZE (GET_MODE (dest)) == 2);

  if (code == ASHIFT)
    {
      if (IN_RANGE (ioff, 9, 15))
	{
	  rtx dst8 = avr_byte (dest, 1);
	  rtx src8 = avr_byte (src, 0);
	  avr_emit_shift (code, dst8, src8, ioff - 8, NULL_RTX);
	  emit_valid_move_clobbercc (avr_byte (dest, 0), const0_rtx);
	  return true;
	}
    }
  else if (code == ASHIFTRT
	   || code == LSHIFTRT)
    {
      if (IN_RANGE (ioff, 9, 14 + (code == LSHIFTRT)))
	{
	  rtx dst8 = avr_byte (dest, 0);
	  rtx src8 = avr_byte (src, 1);
	  rtx signs = const0_rtx;
	  avr_emit_shift (code, dst8, src8, ioff - 8, NULL_RTX);
	  if (code == ASHIFTRT)
	    {
	      signs = avr_byte (dest, 1);
	      avr_emit_shift (code, signs, src8, 7, NULL_RTX);
	    }
	  emit_valid_move_clobbercc (avr_byte (dest, 1), signs);
	  return true;
	}
    }
  else
    gcc_unreachable ();

  return false;
}


/* Worker for a define_split that runs when -msplit-bit-shift is on.
   Split a shift of code CODE into a 3op byte shift and a residual bit shift.
   Return 'true' when a split has been performed and insns have been emitted.
   Otherwise, return 'false'.  */

bool
avr_split_shift (rtx xop[], rtx scratch, rtx_code code)
{
  scratch = scratch && REG_P (scratch) ? scratch : NULL_RTX;
  rtx dest = xop[0];
  rtx src = xop[1];
  int ioff = INTVAL (xop[2]);
  int n_bytes = GET_MODE_SIZE (GET_MODE (dest));

  return select<bool>()
    : n_bytes == 2 ? avr_split_shift2 (dest, src, ioff, scratch, code)
    : n_bytes == 3 ? avr_split_shift3 (dest, src, ioff, scratch, code)
    : n_bytes == 4 ? avr_split_shift4 (dest, src, ioff, scratch, code)
    : bad_case<bool> ();
}


namespace
{


//////////////////////////////////////////////////////////////////////////////
// Determine whether an ISR may use the __gcc_isr pseudo-instruction.

static const pass_data avr_pass_data_pre_proep =
{
  RTL_PASS,	    // type
  "",		    // name (will be patched)
  OPTGROUP_NONE,    // optinfo_flags
  TV_DF_SCAN,	    // tv_id
  0,		    // properties_required
  0,		    // properties_provided
  0,		    // properties_destroyed
  0,		    // todo_flags_start
  0		    // todo_flags_finish
};

class avr_pass_pre_proep : public rtl_opt_pass
{
public:
  avr_pass_pre_proep (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_pre_proep, ctxt)
  {
    this->name = name;
  }

  void compute_maybe_gasisr (function *);

  unsigned int execute (function *fun) final override
  {
    if (avropt_gasisr_prologues
	// Whether this function is an ISR worth scanning at all.
	&& !fun->machine->is_no_gccisr
	&& (fun->machine->is_interrupt
	    || fun->machine->is_signal)
	&& !cfun->machine->is_naked
	// Paranoia: Non-local gotos and labels that might escape.
	&& !cfun->calls_setjmp
	&& !cfun->has_nonlocal_label
	&& !cfun->has_forced_label_in_static)
      {
	compute_maybe_gasisr (fun);
      }

    return 0;
  }

}; // avr_pass_pre_proep


/* Set fun->machine->gasisr.maybe provided we don't find anything that
   prohibits GAS generating parts of ISR prologues / epilogues for us.  */

void
avr_pass_pre_proep::compute_maybe_gasisr (function *fun)
{
  // Don't use BB iterators so that we see JUMP_TABLE_DATA.

  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      // Transparent calls always use [R]CALL and are filtered out by GAS.
      // ISRs don't use -mcall-prologues, hence what remains to be filtered
      // out are open coded (tail) calls.

      if (CALL_P (insn))
	return;

      // __tablejump2__ clobbers something and is targeted by JMP so
      // that GAS won't see its usage.

      if (AVR_HAVE_JMP_CALL
	  && JUMP_TABLE_DATA_P (insn))
	return;

      // Non-local gotos not seen in *FUN.

      if (JUMP_P (insn)
	  && find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
	return;
    }

  fun->machine->gasisr.maybe = 1;
}



//////////////////////////////////////////////////////////////////////////////
// Late recomputation of notes so we can use `reg_unused_after()' and friends.

static const pass_data avr_pass_data_recompute_notes =
{
  RTL_PASS,      // type
  "",            // name (will be patched)
  OPTGROUP_NONE, // optinfo_flags
  TV_DF_SCAN,    // tv_id
  0,             // properties_required
  0,             // properties_provided
  0,             // properties_destroyed
  0,             // todo_flags_start
  TODO_df_finish | TODO_df_verify // todo_flags_finish
};

class avr_pass_recompute_notes : public rtl_opt_pass
{
public:
  avr_pass_recompute_notes (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_recompute_notes, ctxt)
  {
    this->name = name;
  }

  unsigned int execute (function *) final override
  {
    df_note_add_problem ();
    df_analyze ();

    return 0;
  }
}; // avr_pass_recompute_notes

} // anonymous namespace



//////////////////////////////////////////////////////////////////////////////
// Function visible and used outside this module.

/* During reload, we allow much more addresses than Reduced Tiny actually
   supports.  Split them after reload in order to get closer to the
   core's capabilities.  This sets the stage for pass .avr-fuse-add.  */

bool
avr_split_fake_addressing_move (rtx_insn * /*insn*/, rtx *xop)
{
  bool store_p = false;
  rtx mem, reg_or_0;

  if (REG_P (xop[0]) && MEM_P (xop[1]))
    {
      reg_or_0 = xop[0];
      mem = xop[1];
    }
  else if (MEM_P (xop[0])
	   && (REG_P (xop[1])
	       || xop[1] == CONST0_RTX (GET_MODE (xop[0]))))
    {
      mem = xop[0];
      reg_or_0 = xop[1];
      store_p = true;
    }
  else
    return false;

  machine_mode mode = GET_MODE (mem);
  rtx base, addr = XEXP (mem, 0);
  rtx_code addr_code = GET_CODE (addr);

  if (REG_P (reg_or_0)
      && reg_overlap_mentioned_p (reg_or_0, addr))
    return false;
  else if (addr_code == PLUS || addr_code == PRE_DEC || addr_code == POST_INC)
    base = XEXP (addr, 0);
  else if (addr_code == REG)
    base = addr;
  else
    return false;

  if (REGNO (base) > REG_Z)
    return false;

  if (! AVR_TINY
      // Only keep base registers that can't do PLUS addressing.
      && ((REGNO (base) != REG_X
	   && ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (mem)))
	  || avr_load_libgcc_p (mem)
	  || avr_mem_memx_p (mem)))
    return false;

  bool volatile_p = MEM_VOLATILE_P (mem);
  bool mem_volatile_p = false;
  if (frame_pointer_needed
      && REGNO (base) == FRAME_POINTER_REGNUM)
    {
      if (avropt_fuse_add < 2
	  // Be a projection (we always split PLUS).
	  || (avropt_fuse_add == 2 && volatile_p && addr_code != PLUS))
	return false;

      // Changing the frame pointer locally may confuse later passes
      // like .dse2 which don't track changes of FP, not even when
      // respective CFA notes are present.  An example is pr22141-1.c.
      if (avropt_fuse_add == 2)
	mem_volatile_p = true;
    }

  rtx_code new_code = UNKNOWN;
  HOST_WIDE_INT add = 0, sub = 0;
  int msize = GET_MODE_SIZE (mode);

  AVR_LdSt_Props ap { (int) REGNO (base), store_p, volatile_p,
		      ADDR_SPACE_GENERIC };

  switch (addr_code)
    {
    default:
      return false;

    case PLUS:
      add = INTVAL (XEXP (addr, 1));
      if (msize == 1)
	{
	  new_code = REG;
	  sub = -add;
	}
      else if (ap.want_predec)
	{
	  // volatile stores prefer PRE_DEC (MSB first)
	  sub = -add;
	  add += msize;
	  new_code = PRE_DEC;
	}
      else
	{
	  new_code = POST_INC;
	  sub = -add - msize;
	}
      break;

    case POST_INC:
      // volatile stores prefer PRE_DEC (MSB first)
      if (msize > 1 && ap.want_predec)
	{
	  add = msize;
	  new_code = PRE_DEC;
	  sub = msize;
	  break;
	}
      return false;

    case PRE_DEC:
      // volatile loads prefer POST_INC (LSB first)
      if (msize > 1 && ap.want_postinc)
	{
	  add = -msize;
	  new_code = POST_INC;
	  sub = -msize;
	  break;
	}
      return false;

    case REG:
      if (msize == 1)
	return false;

      if (ap.want_predec)
	{
	  add = msize;
	  new_code = PRE_DEC;
	  sub = 0;
	}
      else
	{
	  add = 0;
	  new_code = POST_INC;
	  sub = -msize;
	}
      break;
    } // switch addr_code

  rtx_insn *insn;

  if (add)
    {
      insn = emit_move_ccc (base, plus_constant (Pmode, base, add));
      avr_maybe_adjust_cfa (insn, base, add);
    }

  rtx new_addr = new_code == REG
    ? base
    : gen_rtx_fmt_e (new_code, Pmode, base);

  rtx new_mem = change_address (mem, mode, new_addr);
  if (mem_volatile_p)
    MEM_VOLATILE_P (new_mem) = 1;

  insn = emit_move_ccc (store_p ? new_mem : reg_or_0,
			store_p ? reg_or_0 : new_mem);
  if (auto_inc_p (new_addr))
    {
      add_reg_note (insn, REG_INC, base);
      int off = new_code == POST_INC ? msize : -msize;
      avr_maybe_adjust_cfa (insn, base, off);
    }

  if (sub)
    {
      insn = emit_move_ccc (base, plus_constant (Pmode, base, sub));
      avr_maybe_adjust_cfa (insn, base, sub);
    }

  return true;
}


/* Given memory reference mem(ADDR), return true when it can be split into
   single-byte moves, and all resulting addresses are natively supported.
   ADDR is in addr-space generic.  */

static bool
splittable_address_p (rtx addr, int n_bytes)
{
  if (CONSTANT_ADDRESS_P (addr)
      || GET_CODE (addr) == PRE_DEC
      || GET_CODE (addr) == POST_INC)
    return true;

  if (! AVR_TINY)
    {
      rtx base = select<rtx>()
	: REG_P (addr) ? addr
	: GET_CODE (addr) == PLUS ? XEXP (addr, 0)
	: NULL_RTX;

      int off = select<int>()
	: REG_P (addr) ? 0
	: GET_CODE (addr) == PLUS ? (int) INTVAL (XEXP (addr, 1))
	: -1;

      return (base && REG_P (base)
	      && (REGNO (base) == REG_Y || REGNO (base) == REG_Z)
	      && IN_RANGE (off, 0, 64 - n_bytes));
    }

  return false;
}


/* Like avr_byte(), but also knows how to split POST_INC and PRE_DEC
   memory references.  */

static rtx
avr_byte_maybe_mem (rtx x, int n)
{
  rtx addr, b;
  if (MEM_P (x)
      && (GET_CODE (addr = XEXP (x, 0)) == POST_INC
	  || GET_CODE (addr) == PRE_DEC))
    b = gen_rtx_MEM (QImode, copy_rtx (addr));
  else
    b = avr_byte (x, n);

  if (MEM_P (x))
    gcc_assert (MEM_P (b));

  return b;
}


/* Split multi-byte load / stores into 1-byte such insns
   provided non-volatile, addr-space = generic, no reg-overlap
   and the resulting addressings are all natively supported.
   Returns true when the  XOP[0] = XOP[1]  insn has been split and
   false, otherwise.  */

bool
avr_split_ldst (rtx *xop)
{
  rtx dest = xop[0];
  rtx src = xop[1];
  machine_mode mode = GET_MODE (dest);
  int n_bytes = GET_MODE_SIZE (mode);
  rtx mem, reg_or_0;

  if (MEM_P (dest) && reg_or_0_operand (src, mode))
    {
      mem = dest;
      reg_or_0 = src;
    }
  else if (register_operand (dest, mode) && MEM_P (src))
    {
      reg_or_0 = dest;
      mem = src;
    }
  else
    return false;

  rtx addr = XEXP (mem, 0);

  if (MEM_VOLATILE_P (mem)
      || ! ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (mem))
      || ! IN_RANGE (n_bytes, 2, 4)
      || ! splittable_address_p (addr, n_bytes)
      || reg_overlap_mentioned_p (reg_or_0, addr))
    return false;

  const int step = GET_CODE (addr) == PRE_DEC ? -1 : 1;
  const int istart = step > 0 ? 0 : n_bytes - 1;
  const int iend = istart + step * n_bytes;

  for (int i = istart; i != iend; i += step)
    {
      rtx di = avr_byte_maybe_mem (dest, i);
      rtx si = avr_byte_maybe_mem (src, i);
      emit_move_ccc (di, si);
    }

  return true;
}



// Functions  make_<pass-name> (gcc::context*)  where <pass-name> is
// according to the pass declaration in avr-passes.def.  GCC's pass
// manager uses these function to create the respective pass object.

// Optimize results of the casesi expander for modes < SImode.

rtl_opt_pass *
make_avr_pass_casesi (gcc::context *ctxt)
{
  return new avr_pass_casesi (ctxt, "avr-casesi");
}

// Try to replace 2 cbranch insns with 1 comparison and 2 branches.

rtl_opt_pass *
make_avr_pass_ifelse (gcc::context *ctxt)
{
  return new avr_pass_ifelse (ctxt, "avr-ifelse");
}

// Determine whether an ISR may use the __gcc_isr pseudo-instruction.

rtl_opt_pass *
make_avr_pass_pre_proep (gcc::context *ctxt)
{
  return new avr_pass_pre_proep (ctxt, "avr-pre-proep");
}

// Find more POST_INC and PRE_DEC cases.

rtl_opt_pass *
make_avr_pass_fuse_add (gcc::context *ctxt)
{
  return new avr_pass_fuse_add (ctxt, "avr-fuse-add");
}

// Late recomputation of notes so we can use `reg_unused_after()' and friends.

rtl_opt_pass *
make_avr_pass_recompute_notes (gcc::context *ctxt)
{
  return new avr_pass_recompute_notes (ctxt, "avr-notes-free-cfg");
}

// Optimize moves after reload.

rtl_opt_pass *
make_avr_pass_fuse_move (gcc::context *ctxt)
{
  return new avr_pass_fuse_move (ctxt, "avr-fuse-move");
}

// Split insns after peephole2 / befor avr-fuse-move.

rtl_opt_pass *
make_avr_pass_split_after_peephole2 (gcc::context *ctxt)
{
  return new avr_pass_split_after_peephole2 (ctxt, "avr-split-after-peephole2");
}
