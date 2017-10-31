/* Subroutines for insn-output.c for ATMEL AVR micro controllers
   Copyright (C) 1998-2017 Free Software Foundation, Inc.
   Contributed by Denis Chertykov (chertykov@gmail.com)

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

#include "config.h"
#include "system.h"
#include "intl.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "cgraph.h"
#include "c-family/c-common.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "conditions.h"
#include "insn-attr.h"
#include "reload.h"
#include "varasm.h"
#include "calls.h"
#include "stor-layout.h"
#include "output.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "cfgrtl.h"
#include "params.h"
#include "builtins.h"
#include "context.h"
#include "tree-pass.h"
#include "print-rtl.h"
#include "rtl-iter.h"

/* This file should be included last.  */
#include "target-def.h"

/* Maximal allowed offset for an address in the LD command */
#define MAX_LD_OFFSET(MODE) (64 - (signed)GET_MODE_SIZE (MODE))

/* Return true if STR starts with PREFIX and false, otherwise.  */
#define STR_PREFIX_P(STR,PREFIX) (0 == strncmp (STR, PREFIX, strlen (PREFIX)))

/* The 4 bits starting at SECTION_MACH_DEP are reserved to store the
   address space where data is to be located.
   As the only non-generic address spaces are all located in flash,
   this can be used to test if data shall go into some .progmem* section.
   This must be the rightmost field of machine dependent section flags.  */
#define AVR_SECTION_PROGMEM (0xf * SECTION_MACH_DEP)

/* Similar 4-bit region for SYMBOL_REF_FLAGS.  */
#define AVR_SYMBOL_FLAG_PROGMEM (0xf * SYMBOL_FLAG_MACH_DEP)

/* Similar 4-bit region in SYMBOL_REF_FLAGS:
   Set address-space AS in SYMBOL_REF_FLAGS of SYM  */
#define AVR_SYMBOL_SET_ADDR_SPACE(SYM,AS)                       \
  do {                                                          \
    SYMBOL_REF_FLAGS (sym) &= ~AVR_SYMBOL_FLAG_PROGMEM;         \
    SYMBOL_REF_FLAGS (sym) |= (AS) * SYMBOL_FLAG_MACH_DEP;      \
  } while (0)

/* Read address-space from SYMBOL_REF_FLAGS of SYM  */
#define AVR_SYMBOL_GET_ADDR_SPACE(SYM)                          \
  ((SYMBOL_REF_FLAGS (sym) & AVR_SYMBOL_FLAG_PROGMEM)           \
   / SYMBOL_FLAG_MACH_DEP)

/* (AVR_TINY only): Symbol has attribute progmem */
#define AVR_SYMBOL_FLAG_TINY_PM \
  (SYMBOL_FLAG_MACH_DEP << 7)

/* (AVR_TINY only): Symbol has attribute absdata */
#define AVR_SYMBOL_FLAG_TINY_ABSDATA \
  (SYMBOL_FLAG_MACH_DEP << 8)

#define TINY_ADIW(REG1, REG2, I)                                \
    "subi " #REG1 ",lo8(-(" #I "))" CR_TAB                      \
    "sbci " #REG2 ",hi8(-(" #I "))"

#define TINY_SBIW(REG1, REG2, I)                                \
    "subi " #REG1 ",lo8((" #I "))" CR_TAB                       \
    "sbci " #REG2 ",hi8((" #I "))"

#define AVR_TMP_REGNO (AVR_TINY ? TMP_REGNO_TINY : TMP_REGNO)
#define AVR_ZERO_REGNO (AVR_TINY ? ZERO_REGNO_TINY : ZERO_REGNO)

/* Known address spaces.  The order must be the same as in the respective
   enum from avr.h (or designated initialized must be used).  */
const avr_addrspace_t avr_addrspace[ADDR_SPACE_COUNT] =
{
  { ADDR_SPACE_RAM,  0, 2, "", 0, NULL },
  { ADDR_SPACE_FLASH,  1, 2, "__flash",   0, ".progmem.data" },
  { ADDR_SPACE_FLASH1, 1, 2, "__flash1",  1, ".progmem1.data" },
  { ADDR_SPACE_FLASH2, 1, 2, "__flash2",  2, ".progmem2.data" },
  { ADDR_SPACE_FLASH3, 1, 2, "__flash3",  3, ".progmem3.data" },
  { ADDR_SPACE_FLASH4, 1, 2, "__flash4",  4, ".progmem4.data" },
  { ADDR_SPACE_FLASH5, 1, 2, "__flash5",  5, ".progmem5.data" },
  { ADDR_SPACE_MEMX, 1, 3, "__memx",  0, ".progmemx.data" },
};


/* Holding RAM addresses of some SFRs used by the compiler and that
   are unique over all devices in an architecture like 'avr4'.  */

typedef struct
{
  /* SREG: The processor status */
  int sreg;

  /* RAMPX, RAMPY, RAMPD and CCP of XMEGA */
  int ccp;
  int rampd;
  int rampx;
  int rampy;

  /* RAMPZ: The high byte of 24-bit address used with ELPM */
  int rampz;

  /* SP: The stack pointer and its low and high byte */
  int sp_l;
  int sp_h;
} avr_addr_t;

static avr_addr_t avr_addr;


/* Prototypes for local helper functions.  */

static const char* out_movqi_r_mr (rtx_insn *, rtx[], int*);
static const char* out_movhi_r_mr (rtx_insn *, rtx[], int*);
static const char* out_movsi_r_mr (rtx_insn *, rtx[], int*);
static const char* out_movqi_mr_r (rtx_insn *, rtx[], int*);
static const char* out_movhi_mr_r (rtx_insn *, rtx[], int*);
static const char* out_movsi_mr_r (rtx_insn *, rtx[], int*);

static int get_sequence_length (rtx_insn *insns);
static int sequent_regs_live (void);
static const char *ptrreg_to_str (int);
static const char *cond_string (enum rtx_code);
static int avr_num_arg_regs (machine_mode, const_tree);
static int avr_operand_rtx_cost (rtx, machine_mode, enum rtx_code,
                                 int, bool);
static void output_reload_in_const (rtx*, rtx, int*, bool);
static struct machine_function * avr_init_machine_status (void);


/* Prototypes for hook implementors if needed before their implementation.  */

static bool avr_rtx_costs (rtx, machine_mode, int, int, int*, bool);


/* Allocate registers from r25 to r8 for parameters for function calls.  */
#define FIRST_CUM_REG 26

/* Last call saved register */
#define LAST_CALLEE_SAVED_REG (AVR_TINY ? 19 : 17)

/* Implicit target register of LPM instruction (R0) */
extern GTY(()) rtx lpm_reg_rtx;
rtx lpm_reg_rtx;

/* (Implicit) address register of LPM instruction (R31:R30 = Z) */
extern GTY(()) rtx lpm_addr_reg_rtx;
rtx lpm_addr_reg_rtx;

/* Temporary register RTX (reg:QI TMP_REGNO) */
extern GTY(()) rtx tmp_reg_rtx;
rtx tmp_reg_rtx;

/* Zeroed register RTX (reg:QI ZERO_REGNO) */
extern GTY(()) rtx zero_reg_rtx;
rtx zero_reg_rtx;

/* RTXs for all general purpose registers as QImode */
extern GTY(()) rtx all_regs_rtx[32];
rtx all_regs_rtx[32];

/* SREG, the processor status */
extern GTY(()) rtx sreg_rtx;
rtx sreg_rtx;

/* RAMP* special function registers */
extern GTY(()) rtx rampd_rtx;
extern GTY(()) rtx rampx_rtx;
extern GTY(()) rtx rampy_rtx;
extern GTY(()) rtx rampz_rtx;
rtx rampd_rtx;
rtx rampx_rtx;
rtx rampy_rtx;
rtx rampz_rtx;

/* RTX containing the strings "" and "e", respectively */
static GTY(()) rtx xstring_empty;
static GTY(()) rtx xstring_e;

/* Current architecture.  */
const avr_arch_t *avr_arch;

/* Unnamed sections associated to __attribute__((progmem)) aka. PROGMEM
   or to address space __flash* or __memx.  Only used as singletons inside
   avr_asm_select_section, but it must not be local there because of GTY.  */
static GTY(()) section *progmem_section[ADDR_SPACE_COUNT];

/* Condition for insns/expanders from avr-dimode.md.  */
bool avr_have_dimode = true;

/* To track if code will use .bss and/or .data.  */
bool avr_need_clear_bss_p = false;
bool avr_need_copy_data_p = false;


/* Transform UP into lowercase and write the result to LO.
   You must provide enough space for LO.  Return LO.  */

static char*
avr_tolower (char *lo, const char *up)
{
  char *lo0 = lo;

  for (; *up; up++, lo++)
    *lo = TOLOWER (*up);

  *lo = '\0';

  return lo0;
}


/* Constraint helper function.  XVAL is a CONST_INT or a CONST_DOUBLE.
   Return true if the least significant N_BYTES bytes of XVAL all have a
   popcount in POP_MASK and false, otherwise.  POP_MASK represents a subset
   of integers which contains an integer N iff bit N of POP_MASK is set.  */

bool
avr_popcount_each_byte (rtx xval, int n_bytes, int pop_mask)
{
  machine_mode mode = GET_MODE (xval);

  if (VOIDmode == mode)
    mode = SImode;

  for (int i = 0; i < n_bytes; i++)
    {
      rtx xval8 = simplify_gen_subreg (QImode, xval, mode, i);
      unsigned int val8 = UINTVAL (xval8) & GET_MODE_MASK (QImode);

      if (0 == (pop_mask & (1 << popcount_hwi (val8))))
        return false;
    }

  return true;
}


/* Access some RTX as INT_MODE.  If X is a CONST_FIXED we can get
   the bit representation of X by "casting" it to CONST_INT.  */

rtx
avr_to_int_mode (rtx x)
{
  machine_mode mode = GET_MODE (x);

  return VOIDmode == mode
    ? x
    : simplify_gen_subreg (int_mode_for_mode (mode).require (), x, mode, 0);
}

namespace {

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

  virtual unsigned int execute (function*)
  {
    df_note_add_problem ();
    df_analyze ();

    return 0;
  }
}; // avr_pass_recompute_notes

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

  void avr_rest_of_handle_casesi (function*);

  virtual bool gate (function*) { return optimize > 0; }

  virtual unsigned int execute (function *func)
  {
    avr_rest_of_handle_casesi (func);

    return 0;
  }
}; // avr_pass_casesi

} // anon namespace

rtl_opt_pass*
make_avr_pass_recompute_notes (gcc::context *ctxt)
{
  return new avr_pass_recompute_notes (ctxt, "avr-notes-free-cfg");
}

rtl_opt_pass*
make_avr_pass_casesi (gcc::context *ctxt)
{
  return new avr_pass_casesi (ctxt, "avr-casesi");
}


/* Make one parallel insn with all the patterns from insns i[0]..i[5].  */

static rtx_insn*
avr_parallel_insn_from_insns (rtx_insn *i[6])
{
  rtvec vec = gen_rtvec (6, PATTERN (i[0]), PATTERN (i[1]), PATTERN (i[2]),
                         PATTERN (i[3]), PATTERN (i[4]), PATTERN (i[5]));
  start_sequence();
  emit (gen_rtx_PARALLEL (VOIDmode, vec));
  rtx_insn *insn = get_insns();
  end_sequence();

  return insn;
}


/* Return true if we see an insn stream generated by casesi expander together
   with an extension to SImode of the switch value.

   If this is the case, fill in the insns from casesi to INSNS[1..5] and
   the SImode extension to INSNS[0].  Moreover, extract the operands of
   pattern casesi_<mode>_sequence forged from the sequence to recog_data.  */

static bool
avr_is_casesi_sequence (basic_block bb, rtx_insn *insn, rtx_insn *insns[6])
{
  rtx set_5, set_0;

  /* A first and quick test for a casesi sequences.  As a side effect of
     the test, harvest respective insns to INSNS[0..5].  */

  if (!(JUMP_P (insns[5] = insn)
        // casesi is the only insn that comes up with UNSPEC_INDEX_JMP,
        // hence the following test ensures that we are actually dealing
        // with code from casesi.
        && (set_5 = single_set (insns[5]))
        && UNSPEC == GET_CODE (SET_SRC (set_5))
        && UNSPEC_INDEX_JMP == XINT (SET_SRC (set_5), 1)

        && (insns[4] = prev_real_insn (insns[5]))
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
      for (int i = 0; i < 6; i++)
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

  gcc_assert (11 == recog_data.n_operands);
  gcc_assert (4 == recog_data.n_dups);

  if (dump_file)
    {
      fprintf (dump_file, ";; Operands extracted:\n");
      for (int i = 0; i < recog_data.n_operands; i++)
        avr_fdump (dump_file, ";; $%d = %r\n", i, recog_data.operand[i]);
      fprintf (dump_file, "\n");
    }

  return true;
}


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
      && xop[8] == all_regs_rtx[24])
    {
      // $6 is: (subreg:SI ($5) 0)
      sub_5 = xop[6];
    }

  if (!AVR_HAVE_EIJMP_EICALL
      // $6 is: (plus:HI (subreg:SI ($5) 0)
      //                 (label_ref ($3)))
      && PLUS == GET_CODE (xop[6])
      && LABEL_REF == GET_CODE (XEXP (xop[6], 1))
      && rtx_equal_p (xop[3], XEXP (XEXP (xop[6], 1), 0))
      // The last clobber op of the tablejump.
      && xop[8] == const0_rtx)
    {
      sub_5 = XEXP (xop[6], 0);
    }

  if (sub_5
      && SUBREG_P (sub_5)
      && 0 == SUBREG_BYTE (sub_5)
      && rtx_equal_p (xop[5], SUBREG_REG (sub_5)))
    return true;

  if (dump_file)
    fprintf (dump_file, "\n;; Failed condition for casesi_<mode>_sequence\n\n");

  return false;
}


/* INSNS[1..5] is a sequence as generated by casesi and INSNS[0] is an
   extension of an 8-bit or 16-bit integer to SImode.  XOP contains the
   operands of INSNS as extracted by insn_extract from pattern
   casesi_<mode>_sequence:

      $0: SImode reg switch value as result of $9.
      $1: Negative of smallest index in switch.
      $2: Number of entries in switch.
      $3: Label to table.
      $4: Label if out-of-bounds.
      $5: $0 + $1.
      $6: 3-byte PC: subreg:HI ($5) + label_ref ($3)
          2-byte PC: subreg:HI ($5)
      $7: HI reg index into table (Z or pseudo)
      $8: R24 or const0_rtx (to be clobbered)
      $9: Extension to SImode of an 8-bit or 16-bit integer register $10.
      $10: QImode or HImode register input of $9.

   Try to optimize this sequence, i.e. use the original HImode / QImode
   switch value instead of SImode.  */

static void
avr_optimize_casesi (rtx_insn *insns[6], rtx *xop)
{
  // Original mode of the switch value; this is QImode or HImode.
  machine_mode mode = GET_MODE (xop[10]);

  // How the original switch value was extended to SImode; this is
  // SIGN_EXTEND or ZERO_EXTEND.
  enum rtx_code code = GET_CODE (xop[9]);

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

  start_sequence();

  rtx_insn *seq1, *seq2, *last1, *last2;

  rtx reg = copy_to_mode_reg (mode, xop[10]);

  rtx (*gen_add)(rtx,rtx,rtx) = QImode == mode ? gen_addqi3 : gen_addhi3;
  rtx (*gen_cmp)(rtx,rtx) = QImode == mode ? gen_cmpqi3 : gen_cmphi3;

  emit_insn (gen_add (reg, reg, gen_int_mode (-low_idx, mode)));
  emit_insn (gen_cmp (reg, gen_int_mode (num_idx, mode)));

  seq1 = get_insns();
  last1 = get_last_insn();
  end_sequence();

  emit_insn_before (seq1, insns[1]);

  // After the out-of-bounds test and corresponding branch, use a
  // 16-bit index.  If QImode is used, extend it to HImode first.
  // This will replace insns[4].

  start_sequence();

  if (QImode == mode)
    reg = force_reg (HImode, gen_rtx_fmt_e (code, HImode, reg));

  rtx pat_4 = AVR_3_BYTE_PC
    ? gen_movhi (xop[7], reg)
    : gen_addhi3 (xop[7], reg, gen_rtx_LABEL_REF (VOIDmode, xop[3]));

  emit_insn (pat_4);

  seq2 = get_insns();
  last2 = get_last_insn();
  end_sequence();

  emit_insn_after (seq2, insns[4]);

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
               INSN_UID (insns[1]), INSN_UID (insns[2]), INSN_UID (insns[4]));
    }

  // Pseudodelete the SImode and subreg of SImode insns.  We don't care
  // about the extension insns[0]: Its result is now unused and other
  // passes will clean it up.

  SET_INSN_DELETED (insns[1]);
  SET_INSN_DELETED (insns[2]);
  SET_INSN_DELETED (insns[4]);
}


void
avr_pass_casesi::avr_rest_of_handle_casesi (function *func)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, func)
    {
      rtx_insn *insn, *insns[6];

      FOR_BB_INSNS (bb, insn)
        {
          if (avr_is_casesi_sequence (bb, insn, insns))
            {
              avr_optimize_casesi (insns, recog_data.operand);
            }
        }
    }
}


/* Set `avr_arch' as specified by `-mmcu='.
   Return true on success.  */

static bool
avr_set_core_architecture (void)
{
  /* Search for mcu core architecture.  */

  if (!avr_mmcu)
    avr_mmcu = AVR_MMCU_DEFAULT;

  avr_arch = &avr_arch_types[0];

  for (const avr_mcu_t *mcu = avr_mcu_types; ; mcu++)
    {
      if (NULL == mcu->name)
        {
          /* Reached the end of `avr_mcu_types'.  This should actually never
             happen as options are provided by device-specs.  It could be a
             typo in a device-specs or calling the compiler proper directly
             with -mmcu=<device>. */

          error ("unknown core architecture %qs specified with %qs",
                 avr_mmcu, "-mmcu=");
          avr_inform_core_architectures ();
          break;
        }
      else if (0 == strcmp (mcu->name, avr_mmcu)
               // Is this a proper architecture ?
               && NULL == mcu->macro)
        {
          avr_arch = &avr_arch_types[mcu->arch_id];
          if (avr_n_flash < 0)
            avr_n_flash = 1 + (mcu->flash_size - 1) / 0x10000;

          return true;
        }
    }

  return false;
}


/* Implement `TARGET_OPTION_OVERRIDE'.  */

static void
avr_option_override (void)
{
  /* Disable -fdelete-null-pointer-checks option for AVR target.
     This option compiler assumes that dereferencing of a null pointer
     would halt the program.  For AVR this assumption is not true and
     programs can safely dereference null pointers.  Changes made by this
     option may not work properly for AVR.  So disable this option. */

  flag_delete_null_pointer_checks = 0;

  /* caller-save.c looks for call-clobbered hard registers that are assigned
     to pseudos that cross calls and tries so save-restore them around calls
     in order to reduce the number of stack slots needed.

     This might lead to situations where reload is no more able to cope
     with the challenge of AVR's very few address registers and fails to
     perform the requested spills.  */

  if (avr_strict_X)
    flag_caller_saves = 0;

  /* Allow optimizer to introduce store data races. This used to be the
     default - it was changed because bigger targets did not see any
     performance decrease. For the AVR though, disallowing data races
     introduces additional code in LIM and increases reg pressure.  */

  maybe_set_param_value (PARAM_ALLOW_STORE_DATA_RACES, 1,
                         global_options.x_param_values,
                         global_options_set.x_param_values);

  /* Unwind tables currently require a frame pointer for correctness,
     see toplev.c:process_options().  */

  if ((flag_unwind_tables
       || flag_non_call_exceptions
       || flag_asynchronous_unwind_tables)
      && !ACCUMULATE_OUTGOING_ARGS)
    {
      flag_omit_frame_pointer = 0;
    }

  if (flag_pic == 1)
    warning (OPT_fpic, "-fpic is not supported");
  if (flag_pic == 2)
    warning (OPT_fPIC, "-fPIC is not supported");
  if (flag_pie == 1)
    warning (OPT_fpie, "-fpie is not supported");
  if (flag_pie == 2)
    warning (OPT_fPIE, "-fPIE is not supported");

#if !defined (HAVE_AS_AVR_MGCCISR_OPTION)
  avr_gasisr_prologues = 0;
#endif

  if (!avr_set_core_architecture())
    return;

  /* RAM addresses of some SFRs common to all devices in respective arch. */

  /* SREG: Status Register containing flags like I (global IRQ) */
  avr_addr.sreg = 0x3F + avr_arch->sfr_offset;

  /* RAMPZ: Address' high part when loading via ELPM */
  avr_addr.rampz = 0x3B + avr_arch->sfr_offset;

  avr_addr.rampy = 0x3A + avr_arch->sfr_offset;
  avr_addr.rampx = 0x39 + avr_arch->sfr_offset;
  avr_addr.rampd = 0x38 + avr_arch->sfr_offset;
  avr_addr.ccp = (AVR_TINY ? 0x3C : 0x34) + avr_arch->sfr_offset;

  /* SP: Stack Pointer (SP_H:SP_L) */
  avr_addr.sp_l = 0x3D + avr_arch->sfr_offset;
  avr_addr.sp_h = avr_addr.sp_l + 1;

  init_machine_status = avr_init_machine_status;

  avr_log_set_avr_log();
}

/* Function to set up the backend function structure.  */

static struct machine_function *
avr_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}


/* Implement `INIT_EXPANDERS'.  */
/* The function works like a singleton.  */

void
avr_init_expanders (void)
{
  for (int regno = 0; regno < 32; regno ++)
    all_regs_rtx[regno] = gen_rtx_REG (QImode, regno);

  lpm_reg_rtx  = all_regs_rtx[LPM_REGNO];
  tmp_reg_rtx  = all_regs_rtx[AVR_TMP_REGNO];
  zero_reg_rtx = all_regs_rtx[AVR_ZERO_REGNO];

  lpm_addr_reg_rtx = gen_rtx_REG (HImode, REG_Z);

  sreg_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.sreg));
  rampd_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.rampd));
  rampx_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.rampx));
  rampy_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.rampy));
  rampz_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.rampz));

  xstring_empty = gen_rtx_CONST_STRING (VOIDmode, "");
  xstring_e = gen_rtx_CONST_STRING (VOIDmode, "e");

  /* TINY core does not have regs r10-r16, but avr-dimode.md expects them
     to be present */
  if (AVR_TINY)
    avr_have_dimode = false;
}


/* Implement `REGNO_REG_CLASS'.  */
/* Return register class for register R.  */

enum reg_class
avr_regno_reg_class (int r)
{
  static const enum reg_class reg_class_tab[] =
    {
      R0_REG,
      /* r1 - r15 */
      NO_LD_REGS, NO_LD_REGS, NO_LD_REGS,
      NO_LD_REGS, NO_LD_REGS, NO_LD_REGS, NO_LD_REGS,
      NO_LD_REGS, NO_LD_REGS, NO_LD_REGS, NO_LD_REGS,
      NO_LD_REGS, NO_LD_REGS, NO_LD_REGS, NO_LD_REGS,
      /* r16 - r23 */
      SIMPLE_LD_REGS, SIMPLE_LD_REGS, SIMPLE_LD_REGS, SIMPLE_LD_REGS,
      SIMPLE_LD_REGS, SIMPLE_LD_REGS, SIMPLE_LD_REGS, SIMPLE_LD_REGS,
      /* r24, r25 */
      ADDW_REGS, ADDW_REGS,
      /* X: r26, 27 */
      POINTER_X_REGS, POINTER_X_REGS,
      /* Y: r28, r29 */
      POINTER_Y_REGS, POINTER_Y_REGS,
      /* Z: r30, r31 */
      POINTER_Z_REGS, POINTER_Z_REGS,
      /* SP: SPL, SPH */
      STACK_REG, STACK_REG
    };

  if (r <= 33)
    return reg_class_tab[r];

  return ALL_REGS;
}


/* Implement `TARGET_SCALAR_MODE_SUPPORTED_P'.  */

static bool
avr_scalar_mode_supported_p (scalar_mode mode)
{
  if (ALL_FIXED_POINT_MODE_P (mode))
    return true;

  if (PSImode == mode)
    return true;

  return default_scalar_mode_supported_p (mode);
}


/* Return TRUE if DECL is a VAR_DECL located in flash and FALSE, otherwise.  */

static bool
avr_decl_flash_p (tree decl)
{
  if (TREE_CODE (decl) != VAR_DECL
      || TREE_TYPE (decl) == error_mark_node)
    {
      return false;
    }

  return !ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (TREE_TYPE (decl)));
}


/* Return TRUE if DECL is a VAR_DECL located in the 24-bit flash
   address space and FALSE, otherwise.  */

static bool
avr_decl_memx_p (tree decl)
{
  if (TREE_CODE (decl) != VAR_DECL
      || TREE_TYPE (decl) == error_mark_node)
    {
      return false;
    }

  return (ADDR_SPACE_MEMX == TYPE_ADDR_SPACE (TREE_TYPE (decl)));
}


/* Return TRUE if X is a MEM rtx located in flash and FALSE, otherwise.  */

bool
avr_mem_flash_p (rtx x)
{
  return (MEM_P (x)
          && !ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (x)));
}


/* Return TRUE if X is a MEM rtx located in the 24-bit flash
   address space and FALSE, otherwise.  */

bool
avr_mem_memx_p (rtx x)
{
  return (MEM_P (x)
          && ADDR_SPACE_MEMX == MEM_ADDR_SPACE (x));
}


/* A helper for the subsequent function attribute used to dig for
   attribute 'name' in a FUNCTION_DECL or FUNCTION_TYPE */

static inline int
avr_lookup_function_attribute1 (const_tree func, const char *name)
{
  if (FUNCTION_DECL == TREE_CODE (func))
    {
      if (NULL_TREE != lookup_attribute (name, DECL_ATTRIBUTES (func)))
        {
          return true;
        }

      func = TREE_TYPE (func);
    }

  gcc_assert (TREE_CODE (func) == FUNCTION_TYPE
              || TREE_CODE (func) == METHOD_TYPE);

  return NULL_TREE != lookup_attribute (name, TYPE_ATTRIBUTES (func));
}

/* Return nonzero if FUNC is a naked function.  */

static int
avr_naked_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "naked");
}

/* Return nonzero if FUNC is an interrupt function as specified
   by the "interrupt" attribute.  */

static int
avr_interrupt_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "interrupt");
}

/* Return nonzero if FUNC is a signal function as specified
   by the "signal" attribute.  */

static int
avr_signal_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "signal");
}

/* Return nonzero if FUNC is an OS_task function.  */

static int
avr_OS_task_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "OS_task");
}

/* Return nonzero if FUNC is an OS_main function.  */

static int
avr_OS_main_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "OS_main");
}


/* Return nonzero if FUNC is a no_gccisr function as specified
   by the "no_gccisr" attribute.  */

static int
avr_no_gccisr_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "no_gccisr");
}

/* Implement `TARGET_SET_CURRENT_FUNCTION'.  */
/* Sanity cheching for above function attributes.  */

static void
avr_set_current_function (tree decl)
{
  location_t loc;
  const char *isr;

  if (decl == NULL_TREE
      || current_function_decl == NULL_TREE
      || current_function_decl == error_mark_node
      || ! cfun->machine
      || cfun->machine->attributes_checked_p)
    return;

  loc = DECL_SOURCE_LOCATION (decl);

  cfun->machine->is_naked = avr_naked_function_p (decl);
  cfun->machine->is_signal = avr_signal_function_p (decl);
  cfun->machine->is_interrupt = avr_interrupt_function_p (decl);
  cfun->machine->is_OS_task = avr_OS_task_function_p (decl);
  cfun->machine->is_OS_main = avr_OS_main_function_p (decl);
  cfun->machine->is_no_gccisr = avr_no_gccisr_function_p (decl);

  isr = cfun->machine->is_interrupt ? "interrupt" : "signal";

  /* Too much attributes make no sense as they request conflicting features. */

  if (cfun->machine->is_OS_task + cfun->machine->is_OS_main
      + (cfun->machine->is_signal || cfun->machine->is_interrupt) > 1)
    error_at (loc, "function attributes %qs, %qs and %qs are mutually"
              " exclusive", "OS_task", "OS_main", isr);

  /* 'naked' will hide effects of 'OS_task' and 'OS_main'.  */

  if (cfun->machine->is_naked
      && (cfun->machine->is_OS_task || cfun->machine->is_OS_main))
    warning_at (loc, OPT_Wattributes, "function attributes %qs and %qs have"
                " no effect on %qs function", "OS_task", "OS_main", "naked");

  if (cfun->machine->is_interrupt || cfun->machine->is_signal)
    {
      tree args = TYPE_ARG_TYPES (TREE_TYPE (decl));
      tree ret = TREE_TYPE (TREE_TYPE (decl));
      const char *name;

      name = DECL_ASSEMBLER_NAME_SET_P (decl)
        ? IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl))
        : IDENTIFIER_POINTER (DECL_NAME (decl));

      /* Skip a leading '*' that might still prefix the assembler name,
         e.g. in non-LTO runs.  */

      name = default_strip_name_encoding (name);

      /* Interrupt handlers must be  void __vector (void)  functions.  */

      if (args && TREE_CODE (TREE_VALUE (args)) != VOID_TYPE)
        error_at (loc, "%qs function cannot have arguments", isr);

      if (TREE_CODE (ret) != VOID_TYPE)
        error_at (loc, "%qs function cannot return a value", isr);

#if defined WITH_AVRLIBC
      /* Silently ignore 'signal' if 'interrupt' is present.  AVR-LibC startet
         using this when it switched from SIGNAL and INTERRUPT to ISR.  */

      if (cfun->machine->is_interrupt)
        cfun->machine->is_signal = 0;

      /* If the function has the 'signal' or 'interrupt' attribute, ensure
         that the name of the function is "__vector_NN" so as to catch
         when the user misspells the vector name.  */

      if (!STR_PREFIX_P (name, "__vector"))
        warning_at (loc, OPT_Wmisspelled_isr, "%qs appears to be a misspelled "
                    "%qs handler, missing %<__vector%> prefix", name, isr);
#endif // AVR-LibC naming conventions
    }

#if defined WITH_AVRLIBC
  // Common problem is using "ISR" without first including avr/interrupt.h.
  const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
  name = default_strip_name_encoding (name);
  if (0 == strcmp ("ISR", name)
      || 0 == strcmp ("INTERRUPT", name)
      || 0 == strcmp ("SIGNAL", name))
    {
      warning_at (loc, OPT_Wmisspelled_isr, "%qs is a reserved identifier"
                  " in AVR-LibC.  Consider %<#include <avr/interrupt.h>%>"
                  " before using the %qs macro", name, name);
    }
#endif // AVR-LibC naming conventions

  /* Don't print the above diagnostics more than once.  */

  cfun->machine->attributes_checked_p = 1;
}


/* Implement `ACCUMULATE_OUTGOING_ARGS'.  */

int
avr_accumulate_outgoing_args (void)
{
  if (!cfun)
    return TARGET_ACCUMULATE_OUTGOING_ARGS;

  /* FIXME: For setjmp and in avr_builtin_setjmp_frame_value we don't know
        what offset is correct.  In some cases it is relative to
        virtual_outgoing_args_rtx and in others it is relative to
        virtual_stack_vars_rtx.  For example code see
            gcc.c-torture/execute/built-in-setjmp.c
            gcc.c-torture/execute/builtins/sprintf-chk.c   */

  return (TARGET_ACCUMULATE_OUTGOING_ARGS
          && !(cfun->calls_setjmp
               || cfun->has_nonlocal_label));
}


/* Report contribution of accumulated outgoing arguments to stack size.  */

static inline int
avr_outgoing_args_size (void)
{
  return ACCUMULATE_OUTGOING_ARGS ? crtl->outgoing_args_size : 0;
}


/* Implement TARGET_STARTING_FRAME_OFFSET.  */
/* This is the offset from the frame pointer register to the first stack slot
   that contains a variable living in the frame.  */

static HOST_WIDE_INT
avr_starting_frame_offset (void)
{
  return 1 + avr_outgoing_args_size ();
}


/* Return the number of hard registers to push/pop in the prologue/epilogue
   of the current function, and optionally store these registers in SET.  */

static int
avr_regs_to_save (HARD_REG_SET *set)
{
  int count;
  int int_or_sig_p = cfun->machine->is_interrupt || cfun->machine->is_signal;

  if (set)
    CLEAR_HARD_REG_SET (*set);
  count = 0;

  /* No need to save any registers if the function never returns or
     has the "OS_task" or "OS_main" attribute.  */

  if (TREE_THIS_VOLATILE (current_function_decl)
      || cfun->machine->is_OS_task
      || cfun->machine->is_OS_main)
    return 0;

  for (int reg = 0; reg < 32; reg++)
    {
      /* Do not push/pop __tmp_reg__, __zero_reg__, as well as
         any global register variables.  */

      if (fixed_regs[reg])
        continue;

      if ((int_or_sig_p && !crtl->is_leaf && call_used_regs[reg])
          || (df_regs_ever_live_p (reg)
              && (int_or_sig_p || !call_used_regs[reg])
              /* Don't record frame pointer registers here.  They are treated
                 indivitually in prologue.  */
              && !(frame_pointer_needed
                   && (reg == REG_Y || reg == REG_Y + 1))))
        {
          if (set)
            SET_HARD_REG_BIT (*set, reg);
          count++;
        }
    }
  return count;
}


/* Implement `TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS' */

static bool
avr_allocate_stack_slots_for_args (void)
{
  return !cfun->machine->is_naked;
}


/* Return true if register FROM can be eliminated via register TO.  */

static bool
avr_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return ((frame_pointer_needed && to == FRAME_POINTER_REGNUM)
          || !frame_pointer_needed);
}


/* Implement `TARGET_WARN_FUNC_RETURN'.  */

static bool
avr_warn_func_return (tree decl)
{
  /* Naked functions are implemented entirely in assembly, including the
     return sequence, so suppress warnings about this.  */

  return !avr_naked_function_p (decl);
}

/* Compute offset between arg_pointer and frame_pointer.  */

int
avr_initial_elimination_offset (int from, int to)
{
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return 0;
  else
    {
      int offset = frame_pointer_needed ? 2 : 0;
      int avr_pc_size = AVR_HAVE_EIJMP_EICALL ? 3 : 2;

      // If FROM is ARG_POINTER_REGNUM, we are not in an ISR as ISRs
      // might not have arguments.  Hence the following is not affected
      // by gasisr prologues.
      offset += avr_regs_to_save (NULL);
      return (get_frame_size () + avr_outgoing_args_size()
              + avr_pc_size + 1 + offset);
    }
}


/* Helper for the function below.  */

static void
avr_adjust_type_node (tree *node, machine_mode mode, int sat_p)
{
  *node = make_node (FIXED_POINT_TYPE);
  TYPE_SATURATING (*node) = sat_p;
  TYPE_UNSIGNED (*node) = UNSIGNED_FIXED_POINT_MODE_P (mode);
  TYPE_IBIT (*node) = GET_MODE_IBIT (mode);
  TYPE_FBIT (*node) = GET_MODE_FBIT (mode);
  TYPE_PRECISION (*node) = GET_MODE_BITSIZE (mode);
  SET_TYPE_ALIGN (*node, 8);
  SET_TYPE_MODE (*node, mode);

  layout_type (*node);
}


/* Implement `TARGET_BUILD_BUILTIN_VA_LIST'.  */

static tree
avr_build_builtin_va_list (void)
{
  /* avr-modes.def adjusts [U]TA to be 64-bit modes with 48 fractional bits.
     This is more appropriate for the 8-bit machine AVR than 128-bit modes.
     The ADJUST_IBIT/FBIT are handled in toplev:init_adjust_machine_modes()
     which is auto-generated by genmodes, but the compiler assigns [U]DAmode
     to the long long accum modes instead of the desired [U]TAmode.

     Fix this now, right after node setup in tree.c:build_common_tree_nodes().
     This must run before c-cppbuiltin.c:builtin_define_fixed_point_constants()
     which built-in defines macros like __ULLACCUM_FBIT__ that are used by
     libgcc to detect IBIT and FBIT.  */

  avr_adjust_type_node (&ta_type_node, TAmode, 0);
  avr_adjust_type_node (&uta_type_node, UTAmode, 0);
  avr_adjust_type_node (&sat_ta_type_node, TAmode, 1);
  avr_adjust_type_node (&sat_uta_type_node, UTAmode, 1);

  unsigned_long_long_accum_type_node = uta_type_node;
  long_long_accum_type_node = ta_type_node;
  sat_unsigned_long_long_accum_type_node = sat_uta_type_node;
  sat_long_long_accum_type_node = sat_ta_type_node;

  /* Dispatch to the default handler.  */

  return std_build_builtin_va_list ();
}


/* Implement `TARGET_BUILTIN_SETJMP_FRAME_VALUE'.  */
/* Actual start of frame is virtual_stack_vars_rtx this is offset from
   frame pointer by +TARGET_STARTING_FRAME_OFFSET.
   Using saved frame = virtual_stack_vars_rtx - TARGET_STARTING_FRAME_OFFSET
   avoids creating add/sub of offset in nonlocal goto and setjmp.  */

static rtx
avr_builtin_setjmp_frame_value (void)
{
  rtx xval = gen_reg_rtx (Pmode);
  emit_insn (gen_subhi3 (xval, virtual_stack_vars_rtx,
                         gen_int_mode (avr_starting_frame_offset (), Pmode)));
  return xval;
}


/* Return contents of MEM at frame pointer + stack size + 1 (+2 if 3-byte PC).
   This is return address of function.  */

rtx
avr_return_addr_rtx (int count, rtx tem)
{
  rtx r;

  /* Can only return this function's return address. Others not supported.  */
  if (count)
    return NULL;

  if (AVR_3_BYTE_PC)
    {
      r = gen_rtx_SYMBOL_REF (Pmode, ".L__stack_usage+2");
      warning (0, "%<builtin_return_address%> contains only 2 bytes"
               " of address");
    }
  else
    r = gen_rtx_SYMBOL_REF (Pmode, ".L__stack_usage+1");

  cfun->machine->use_L__stack_usage = 1;

  r = gen_rtx_PLUS (Pmode, tem, r);
  r = gen_frame_mem (Pmode, memory_address (Pmode, r));
  r = gen_rtx_ROTATE (HImode, r, GEN_INT (8));
  return r;
}

/* Return 1 if the function epilogue is just a single "ret".  */

int
avr_simple_epilogue (void)
{
  return (! frame_pointer_needed
          && get_frame_size () == 0
          && avr_outgoing_args_size() == 0
          && avr_regs_to_save (NULL) == 0
          && ! cfun->machine->is_interrupt
          && ! cfun->machine->is_signal
          && ! cfun->machine->is_naked
          && ! TREE_THIS_VOLATILE (current_function_decl));
}

/* This function checks sequence of live registers.  */

static int
sequent_regs_live (void)
{
  int live_seq = 0;
  int cur_seq = 0;

  for (int reg = 0; reg <= LAST_CALLEE_SAVED_REG; ++reg)
    {
      if (fixed_regs[reg])
        {
          /* Don't recognize sequences that contain global register
             variables.  */

          if (live_seq != 0)
            return 0;
          else
            continue;
        }

      if (!call_used_regs[reg])
        {
          if (df_regs_ever_live_p (reg))
            {
              ++live_seq;
              ++cur_seq;
            }
          else
            cur_seq = 0;
        }
    }

  if (!frame_pointer_needed)
    {
      if (df_regs_ever_live_p (REG_Y))
        {
          ++live_seq;
          ++cur_seq;
        }
      else
        cur_seq = 0;

      if (df_regs_ever_live_p (REG_Y + 1))
        {
          ++live_seq;
          ++cur_seq;
        }
      else
        cur_seq = 0;
    }
  else
    {
      cur_seq += 2;
      live_seq += 2;
    }
  return (cur_seq == live_seq) ? live_seq : 0;
}

namespace {
static const pass_data avr_pass_data_pre_proep =
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


class avr_pass_pre_proep : public rtl_opt_pass
{
public:
  avr_pass_pre_proep (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_pre_proep, ctxt)
  {
    this->name = name;
  }

  void compute_maybe_gasisr (function*);

  virtual unsigned int execute (function *fun)
  {
    if (avr_gasisr_prologues
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

} // anon namespace

rtl_opt_pass*
make_avr_pass_pre_proep (gcc::context *ctxt)
{
  return new avr_pass_pre_proep (ctxt, "avr-pre-proep");
}


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


/* Obtain the length sequence of insns.  */

int
get_sequence_length (rtx_insn *insns)
{
  int length = 0;

  for (rtx_insn *insn = insns; insn; insn = NEXT_INSN (insn))
    length += get_attr_length (insn);

  return length;
}


/*  Implement `INCOMING_RETURN_ADDR_RTX'.  */

rtx
avr_incoming_return_addr_rtx (void)
{
  /* The return address is at the top of the stack.  Note that the push
     was via post-decrement, which means the actual address is off by one.  */
  return gen_frame_mem (HImode, plus_constant (Pmode, stack_pointer_rtx, 1));
}


/* Unset a bit in *SET.  If successful, return the respective bit number.
   Otherwise, return -1 and *SET is unaltered.  */

static int
avr_hregs_split_reg (HARD_REG_SET *set)
{
  for (int regno = 0; regno < 32; regno++)
    if (TEST_HARD_REG_BIT (*set, regno))
      {
        // Don't remove a register from *SET which might indicate that
        // some RAMP* register might need ISR prologue / epilogue treatment.

        if (AVR_HAVE_RAMPX
            && (REG_X == regno || REG_X + 1 == regno)
            && TEST_HARD_REG_BIT (*set, REG_X)
            && TEST_HARD_REG_BIT (*set, REG_X + 1))
          continue;

        if (AVR_HAVE_RAMPY
            && !frame_pointer_needed
            && (REG_Y == regno || REG_Y + 1 == regno)
            && TEST_HARD_REG_BIT (*set, REG_Y)
            && TEST_HARD_REG_BIT (*set, REG_Y + 1))
          continue;

        if (AVR_HAVE_RAMPZ
            && (REG_Z == regno || REG_Z + 1 == regno)
            && TEST_HARD_REG_BIT (*set, REG_Z)
            && TEST_HARD_REG_BIT (*set, REG_Z + 1))
          continue;
            
        CLEAR_HARD_REG_BIT (*set, regno);
        return regno;
      }

  return -1;
}


/*  Helper for expand_prologue.  Emit a push of a byte register.  */

static void
emit_push_byte (unsigned regno, bool frame_related_p)
{
  rtx mem, reg;
  rtx_insn *insn;

  mem = gen_rtx_POST_DEC (HImode, stack_pointer_rtx);
  mem = gen_frame_mem (QImode, mem);
  reg = gen_rtx_REG (QImode, regno);

  insn = emit_insn (gen_rtx_SET (mem, reg));
  if (frame_related_p)
    RTX_FRAME_RELATED_P (insn) = 1;

  cfun->machine->stack_usage++;
}


/*  Helper for expand_prologue.  Emit a push of a SFR via register TREG.
    SFR is a MEM representing the memory location of the SFR.
    If CLR_P then clear the SFR after the push using zero_reg.  */

static void
emit_push_sfr (rtx sfr, bool frame_related_p, bool clr_p, int treg)
{
  rtx_insn *insn;

  gcc_assert (MEM_P (sfr));

  /* IN treg, IO(SFR) */
  insn = emit_move_insn (all_regs_rtx[treg], sfr);
  if (frame_related_p)
    RTX_FRAME_RELATED_P (insn) = 1;

  /* PUSH treg */
  emit_push_byte (treg, frame_related_p);

  if (clr_p)
    {
      /* OUT IO(SFR), __zero_reg__ */
      insn = emit_move_insn (sfr, const0_rtx);
      if (frame_related_p)
        RTX_FRAME_RELATED_P (insn) = 1;
    }
}

static void
avr_prologue_setup_frame (HOST_WIDE_INT size, HARD_REG_SET set)
{
  rtx_insn *insn;
  bool isr_p = cfun->machine->is_interrupt || cfun->machine->is_signal;
  int live_seq = sequent_regs_live ();

  HOST_WIDE_INT size_max
    = (HOST_WIDE_INT) GET_MODE_MASK (AVR_HAVE_8BIT_SP ? QImode : Pmode);

  bool minimize = (TARGET_CALL_PROLOGUES
                   && size < size_max
                   && live_seq
                   && !isr_p
                   && !cfun->machine->is_OS_task
                   && !cfun->machine->is_OS_main
                   && !AVR_TINY);

  if (minimize
      && (frame_pointer_needed
          || avr_outgoing_args_size() > 8
          || (AVR_2_BYTE_PC && live_seq > 6)
          || live_seq > 7))
    {
      rtx pattern;
      int first_reg, reg, offset;

      emit_move_insn (gen_rtx_REG (HImode, REG_X),
                      gen_int_mode (size, HImode));

      pattern = gen_call_prologue_saves (gen_int_mode (live_seq, HImode),
                                         gen_int_mode (live_seq+size, HImode));
      insn = emit_insn (pattern);
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Describe the effect of the unspec_volatile call to prologue_saves.
         Note that this formulation assumes that add_reg_note pushes the
         notes to the front.  Thus we build them in the reverse order of
         how we want dwarf2out to process them.  */

      /* The function does always set frame_pointer_rtx, but whether that
         is going to be permanent in the function is frame_pointer_needed.  */

      add_reg_note (insn, REG_CFA_ADJUST_CFA,
                    gen_rtx_SET ((frame_pointer_needed
				  ? frame_pointer_rtx
				  : stack_pointer_rtx),
                                 plus_constant (Pmode, stack_pointer_rtx,
                                                -(size + live_seq))));

      /* Note that live_seq always contains r28+r29, but the other
         registers to be saved are all below 18.  */

      first_reg = (LAST_CALLEE_SAVED_REG + 1) - (live_seq - 2);

      for (reg = 29, offset = -live_seq + 1;
           reg >= first_reg;
           reg = (reg == 28 ? LAST_CALLEE_SAVED_REG : reg - 1), ++offset)
        {
          rtx m, r;

          m = gen_rtx_MEM (QImode, plus_constant (Pmode, stack_pointer_rtx,
                                                  offset));
          r = gen_rtx_REG (QImode, reg);
          add_reg_note (insn, REG_CFA_OFFSET, gen_rtx_SET (m, r));
        }

      cfun->machine->stack_usage += size + live_seq;
    }
  else /* !minimize */
    {
      for (int reg = 0; reg < 32; ++reg)
        if (TEST_HARD_REG_BIT (set, reg))
          emit_push_byte (reg, true);

      if (frame_pointer_needed
          && (!(cfun->machine->is_OS_task || cfun->machine->is_OS_main)))
        {
          /* Push frame pointer.  Always be consistent about the
             ordering of pushes -- epilogue_restores expects the
             register pair to be pushed low byte first.  */

          emit_push_byte (REG_Y, true);
          emit_push_byte (REG_Y + 1, true);
        }

      if (frame_pointer_needed
          && size == 0)
        {
          insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
          RTX_FRAME_RELATED_P (insn) = 1;
        }

      if (size != 0)
        {
          /*  Creating a frame can be done by direct manipulation of the
              stack or via the frame pointer. These two methods are:
                  fp =  sp
                  fp -= size
                  sp =  fp
              or
                  sp -= size
                  fp =  sp    (*)
              the optimum method depends on function type, stack and
              frame size.  To avoid a complex logic, both methods are
              tested and shortest is selected.

              There is also the case where SIZE != 0 and no frame pointer is
              needed; this can occur if ACCUMULATE_OUTGOING_ARGS is on.
              In that case, insn (*) is not needed in that case.
              We use the X register as scratch. This is save because in X
              is call-clobbered.
                 In an interrupt routine, the case of SIZE != 0 together with
              !frame_pointer_needed can only occur if the function is not a
              leaf function and thus X has already been saved.  */

          int irq_state = -1;
          HOST_WIDE_INT size_cfa = size, neg_size;
          rtx_insn *fp_plus_insns;
          rtx fp, my_fp;

          gcc_assert (frame_pointer_needed
                      || !isr_p
                      || !crtl->is_leaf);

          fp = my_fp = (frame_pointer_needed
                        ? frame_pointer_rtx
                        : gen_rtx_REG (Pmode, REG_X));

          if (AVR_HAVE_8BIT_SP)
            {
              /* The high byte (r29) does not change:
                 Prefer SUBI (1 cycle) over SBIW (2 cycles, same size).  */

              my_fp = all_regs_rtx[FRAME_POINTER_REGNUM];
            }

          /* Cut down size and avoid size = 0 so that we don't run
             into ICE like PR52488 in the remainder.  */

          if (size > size_max)
            {
              /* Don't error so that insane code from newlib still compiles
                 and does not break building newlib.  As PR51345 is implemented
                 now, there are multilib variants with -msp8.

                 If user wants sanity checks he can use -Wstack-usage=
                 or similar options.

                 For CFA we emit the original, non-saturated size so that
                 the generic machinery is aware of the real stack usage and
                 will print the above diagnostic as expected.  */

              size = size_max;
            }

          size = trunc_int_for_mode (size, GET_MODE (my_fp));
          neg_size = trunc_int_for_mode (-size, GET_MODE (my_fp));

          /************  Method 1: Adjust frame pointer  ************/

          start_sequence ();

          /* Normally, the dwarf2out frame-related-expr interpreter does
             not expect to have the CFA change once the frame pointer is
             set up.  Thus, we avoid marking the move insn below and
             instead indicate that the entire operation is complete after
             the frame pointer subtraction is done.  */

          insn = emit_move_insn (fp, stack_pointer_rtx);
          if (frame_pointer_needed)
            {
              RTX_FRAME_RELATED_P (insn) = 1;
              add_reg_note (insn, REG_CFA_ADJUST_CFA,
                            gen_rtx_SET (fp, stack_pointer_rtx));
            }

          insn = emit_move_insn (my_fp, plus_constant (GET_MODE (my_fp),
                                                       my_fp, neg_size));

          if (frame_pointer_needed)
            {
              RTX_FRAME_RELATED_P (insn) = 1;
              add_reg_note (insn, REG_CFA_ADJUST_CFA,
                            gen_rtx_SET (fp, plus_constant (Pmode, fp,
							    -size_cfa)));
            }

          /* Copy to stack pointer.  Note that since we've already
             changed the CFA to the frame pointer this operation
             need not be annotated if frame pointer is needed.
             Always move through unspec, see PR50063.
             For meaning of irq_state see movhi_sp_r insn.  */

          if (cfun->machine->is_interrupt)
            irq_state = 1;

          if (TARGET_NO_INTERRUPTS
              || cfun->machine->is_signal
              || cfun->machine->is_OS_main)
            irq_state = 0;

          if (AVR_HAVE_8BIT_SP)
            irq_state = 2;

          insn = emit_insn (gen_movhi_sp_r (stack_pointer_rtx,
                                            fp, GEN_INT (irq_state)));
          if (!frame_pointer_needed)
            {
              RTX_FRAME_RELATED_P (insn) = 1;
              add_reg_note (insn, REG_CFA_ADJUST_CFA,
                            gen_rtx_SET (stack_pointer_rtx,
                                         plus_constant (Pmode,
                                                        stack_pointer_rtx,
                                                        -size_cfa)));
            }

          fp_plus_insns = get_insns ();
          end_sequence ();

          /************  Method 2: Adjust Stack pointer  ************/

          /* Stack adjustment by means of RCALL . and/or PUSH __TMP_REG__
             can only handle specific offsets.  */

          int n_rcall = size / (AVR_3_BYTE_PC ? 3 : 2);

          if (avr_sp_immediate_operand (gen_int_mode (-size, HImode), HImode)
              // Don't use more than 3 RCALLs.
              && n_rcall <= 3)
            {
              rtx_insn *sp_plus_insns;

              start_sequence ();

              insn = emit_move_insn (stack_pointer_rtx,
                                     plus_constant (Pmode, stack_pointer_rtx,
                                                    -size));
              RTX_FRAME_RELATED_P (insn) = 1;
              add_reg_note (insn, REG_CFA_ADJUST_CFA,
                            gen_rtx_SET (stack_pointer_rtx,
                                         plus_constant (Pmode,
                                                        stack_pointer_rtx,
                                                        -size_cfa)));
              if (frame_pointer_needed)
                {
                  insn = emit_move_insn (fp, stack_pointer_rtx);
                  RTX_FRAME_RELATED_P (insn) = 1;
                }

              sp_plus_insns = get_insns ();
              end_sequence ();

              /************ Use shortest method  ************/

              emit_insn (get_sequence_length (sp_plus_insns)
                         < get_sequence_length (fp_plus_insns)
                         ? sp_plus_insns
                         : fp_plus_insns);
            }
          else
            {
              emit_insn (fp_plus_insns);
            }

          cfun->machine->stack_usage += size_cfa;
        } /* !minimize && size != 0 */
    } /* !minimize */
}


/*  Output function prologue.  */

void
avr_expand_prologue (void)
{
  HARD_REG_SET set;
  HOST_WIDE_INT size;

  size = get_frame_size() + avr_outgoing_args_size();

  cfun->machine->stack_usage = 0;

  /* Prologue: naked.  */
  if (cfun->machine->is_naked)
    {
      return;
    }

  avr_regs_to_save (&set);

  if (cfun->machine->is_interrupt || cfun->machine->is_signal)
    {
      int treg = AVR_TMP_REGNO;
      /* Enable interrupts.  */
      if (cfun->machine->is_interrupt)
        emit_insn (gen_enable_interrupt ());

      if (cfun->machine->gasisr.maybe)
        {
          /* Let GAS PR21472 emit prologue preamble for us which handles SREG,
             ZERO_REG and TMP_REG and one additional, optional register for
             us in an optimal way.  This even scans through inline asm.  */

          cfun->machine->gasisr.yes = 1;

          // The optional reg or TMP_REG if we don't need one.  If we need one,
          // remove that reg from SET so that it's not puhed / popped twice.
          // We also use it below instead of TMP_REG in some places.

          treg = avr_hregs_split_reg (&set);
          if (treg < 0)
            treg = AVR_TMP_REGNO;
          cfun->machine->gasisr.regno = treg;

          // The worst case of pushes.  The exact number can be inferred
          // at assembly time by magic expression __gcc_isr.n_pushed.
          cfun->machine->stack_usage += 3 + (treg != AVR_TMP_REGNO);

          // Emit a Prologue chunk.  Epilogue chunk(s) might follow.
          // The final Done chunk is emit by final postscan.
          emit_insn (gen_gasisr (GEN_INT (GASISR_Prologue), GEN_INT (treg)));
        }
      else // !TARGET_GASISR_PROLOGUES: Classic, dumb prologue preamble.
        {
          /* Push zero reg.  */
          emit_push_byte (AVR_ZERO_REGNO, true);

          /* Push tmp reg.  */
          emit_push_byte (AVR_TMP_REGNO, true);

          /* Push SREG.  */
          /* ??? There's no dwarf2 column reserved for SREG.  */
          emit_push_sfr (sreg_rtx, false, false /* clr */, AVR_TMP_REGNO);

          /* Clear zero reg.  */
          emit_move_insn (zero_reg_rtx, const0_rtx);

          /* Prevent any attempt to delete the setting of ZERO_REG!  */
          emit_use (zero_reg_rtx);
        }

      /* Push and clear RAMPD/X/Y/Z if present and low-part register is used.
         ??? There are no dwarf2 columns reserved for RAMPD/X/Y/Z.  */

      if (AVR_HAVE_RAMPD)
        emit_push_sfr (rampd_rtx, false /* frame */, true /* clr */, treg);

      if (AVR_HAVE_RAMPX
          && TEST_HARD_REG_BIT (set, REG_X)
          && TEST_HARD_REG_BIT (set, REG_X + 1))
        {
          emit_push_sfr (rampx_rtx, false /* frame */, true /* clr */, treg);
        }

      if (AVR_HAVE_RAMPY
          && (frame_pointer_needed
              || (TEST_HARD_REG_BIT (set, REG_Y)
                  && TEST_HARD_REG_BIT (set, REG_Y + 1))))
        {
          emit_push_sfr (rampy_rtx, false /* frame */, true /* clr */, treg);
        }

      if (AVR_HAVE_RAMPZ
          && TEST_HARD_REG_BIT (set, REG_Z)
          && TEST_HARD_REG_BIT (set, REG_Z + 1))
        {
          emit_push_sfr (rampz_rtx, false /* frame */, AVR_HAVE_RAMPD, treg);
        }
    }  /* is_interrupt is_signal */

  avr_prologue_setup_frame (size, set);

  if (flag_stack_usage_info)
    current_function_static_stack_size
      = cfun->machine->stack_usage + INCOMING_FRAME_SP_OFFSET;
}


/* Implement `TARGET_ASM_FUNCTION_END_PROLOGUE'.  */
/* Output summary at end of function prologue.  */

static void
avr_asm_function_end_prologue (FILE *file)
{
  if (cfun->machine->is_naked)
    {
      fputs ("/* prologue: naked */\n", file);
    }
  else
    {
      if (cfun->machine->is_interrupt)
        {
          fputs ("/* prologue: Interrupt */\n", file);
        }
      else if (cfun->machine->is_signal)
        {
          fputs ("/* prologue: Signal */\n", file);
        }
      else
        fputs ("/* prologue: function */\n", file);
    }

  if (ACCUMULATE_OUTGOING_ARGS)
    fprintf (file, "/* outgoing args size = %d */\n",
             avr_outgoing_args_size());

  fprintf (file, "/* frame size = " HOST_WIDE_INT_PRINT_DEC " */\n",
           get_frame_size());

  if (!cfun->machine->gasisr.yes)
    {
      fprintf (file, "/* stack size = %d */\n", cfun->machine->stack_usage);
      // Create symbol stack offset so all functions have it. Add 1 to stack
      // usage for offset so that SP + .L__stack_offset = return address.
      fprintf (file, ".L__stack_usage = %d\n", cfun->machine->stack_usage);
    }
  else
    {
      int used_by_gasisr = 3 + (cfun->machine->gasisr.regno != AVR_TMP_REGNO);
      int to = cfun->machine->stack_usage;
      int from = to - used_by_gasisr;
      // Number of pushed regs is only known at assembly-time.
      fprintf (file, "/* stack size = %d...%d */\n", from , to);
      fprintf (file, ".L__stack_usage = %d + __gcc_isr.n_pushed\n", from);
    }
}


/* Implement `EPILOGUE_USES'.  */

int
avr_epilogue_uses (int regno ATTRIBUTE_UNUSED)
{
  if (reload_completed
      && cfun->machine
      && (cfun->machine->is_interrupt || cfun->machine->is_signal))
    return 1;
  return 0;
}

/*  Helper for avr_expand_epilogue.  Emit a pop of a byte register.  */

static void
emit_pop_byte (unsigned regno)
{
  rtx mem, reg;

  mem = gen_rtx_PRE_INC (HImode, stack_pointer_rtx);
  mem = gen_frame_mem (QImode, mem);
  reg = gen_rtx_REG (QImode, regno);

  emit_insn (gen_rtx_SET (reg, mem));
}

/*  Output RTL epilogue.  */

void
avr_expand_epilogue (bool sibcall_p)
{
  int live_seq;
  HARD_REG_SET set;
  int minimize;
  HOST_WIDE_INT size;
  bool isr_p = cfun->machine->is_interrupt || cfun->machine->is_signal;

  size = get_frame_size() + avr_outgoing_args_size();

  /* epilogue: naked  */
  if (cfun->machine->is_naked)
    {
      gcc_assert (!sibcall_p);

      emit_jump_insn (gen_return ());
      return;
    }

  avr_regs_to_save (&set);
  live_seq = sequent_regs_live ();

  minimize = (TARGET_CALL_PROLOGUES
              && live_seq
              && !isr_p
              && !cfun->machine->is_OS_task
              && !cfun->machine->is_OS_main
              && !AVR_TINY);

  if (minimize
      && (live_seq > 4
          || frame_pointer_needed
          || size))
    {
      /*  Get rid of frame.  */

      if (!frame_pointer_needed)
        {
          emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
        }

      if (size)
        {
          emit_move_insn (frame_pointer_rtx,
                          plus_constant (Pmode, frame_pointer_rtx, size));
        }

      emit_insn (gen_epilogue_restores (gen_int_mode (live_seq, HImode)));
      return;
    }

  if (size)
    {
      /* Try two methods to adjust stack and select shortest.  */

      int irq_state = -1;
      rtx fp, my_fp;
      rtx_insn *fp_plus_insns;
      HOST_WIDE_INT size_max;

      gcc_assert (frame_pointer_needed
                  || !isr_p
                  || !crtl->is_leaf);

      fp = my_fp = (frame_pointer_needed
                    ? frame_pointer_rtx
                    : gen_rtx_REG (Pmode, REG_X));

      if (AVR_HAVE_8BIT_SP)
        {
          /* The high byte (r29) does not change:
             Prefer SUBI (1 cycle) over SBIW (2 cycles).  */

          my_fp = all_regs_rtx[FRAME_POINTER_REGNUM];
        }

      /* For rationale see comment in prologue generation.  */

      size_max = (HOST_WIDE_INT) GET_MODE_MASK (GET_MODE (my_fp));
      if (size > size_max)
        size = size_max;
      size = trunc_int_for_mode (size, GET_MODE (my_fp));

      /********** Method 1: Adjust fp register  **********/

      start_sequence ();

      if (!frame_pointer_needed)
        emit_move_insn (fp, stack_pointer_rtx);

      emit_move_insn (my_fp, plus_constant (GET_MODE (my_fp), my_fp, size));

      /* Copy to stack pointer.  */

      if (TARGET_NO_INTERRUPTS)
        irq_state = 0;

      if (AVR_HAVE_8BIT_SP)
        irq_state = 2;

      emit_insn (gen_movhi_sp_r (stack_pointer_rtx, fp,
                                 GEN_INT (irq_state)));

      fp_plus_insns = get_insns ();
      end_sequence ();

      /********** Method 2: Adjust Stack pointer  **********/

      if (avr_sp_immediate_operand (gen_int_mode (size, HImode), HImode))
        {
          rtx_insn *sp_plus_insns;

          start_sequence ();

          emit_move_insn (stack_pointer_rtx,
                          plus_constant (Pmode, stack_pointer_rtx, size));

          sp_plus_insns = get_insns ();
          end_sequence ();

          /************ Use shortest method  ************/

          emit_insn (get_sequence_length (sp_plus_insns)
                     < get_sequence_length (fp_plus_insns)
                     ? sp_plus_insns
                     : fp_plus_insns);
        }
      else
        emit_insn (fp_plus_insns);
    } /* size != 0 */

  if (frame_pointer_needed
      && !(cfun->machine->is_OS_task || cfun->machine->is_OS_main))
    {
      /* Restore previous frame_pointer.  See avr_expand_prologue for
         rationale for not using pophi.  */

      emit_pop_byte (REG_Y + 1);
      emit_pop_byte (REG_Y);
    }

  /* Restore used registers.  */

  int treg = AVR_TMP_REGNO;

  if (isr_p
      && cfun->machine->gasisr.yes)
    {
      treg = cfun->machine->gasisr.regno;
      CLEAR_HARD_REG_BIT (set, treg);
    }

  for (int reg = 31; reg >= 0; --reg)
    if (TEST_HARD_REG_BIT (set, reg))
      emit_pop_byte (reg);

  if (isr_p)
    {
      /* Restore RAMPZ/Y/X/D using tmp_reg as scratch.
         The conditions to restore them must be tha same as in prologue.  */

      if (AVR_HAVE_RAMPZ
          && TEST_HARD_REG_BIT (set, REG_Z)
          && TEST_HARD_REG_BIT (set, REG_Z + 1))
        {
          emit_pop_byte (treg);
          emit_move_insn (rampz_rtx, all_regs_rtx[treg]);
        }

      if (AVR_HAVE_RAMPY
          && (frame_pointer_needed
              || (TEST_HARD_REG_BIT (set, REG_Y)
                  && TEST_HARD_REG_BIT (set, REG_Y + 1))))
        {
          emit_pop_byte (treg);
          emit_move_insn (rampy_rtx, all_regs_rtx[treg]);
        }

      if (AVR_HAVE_RAMPX
          && TEST_HARD_REG_BIT (set, REG_X)
          && TEST_HARD_REG_BIT (set, REG_X + 1))
        {
          emit_pop_byte (treg);
          emit_move_insn (rampx_rtx, all_regs_rtx[treg]);
        }

      if (AVR_HAVE_RAMPD)
        {
          emit_pop_byte (treg);
          emit_move_insn (rampd_rtx, all_regs_rtx[treg]);
        }

      if (cfun->machine->gasisr.yes)
        {
          // Emit an Epilogue chunk.
          emit_insn (gen_gasisr (GEN_INT (GASISR_Epilogue),
                                 GEN_INT (cfun->machine->gasisr.regno)));
        }
      else // !TARGET_GASISR_PROLOGUES
        {
          /* Restore SREG using tmp_reg as scratch.  */

          emit_pop_byte (AVR_TMP_REGNO);
          emit_move_insn (sreg_rtx, tmp_reg_rtx);

          /* Restore tmp REG.  */
          emit_pop_byte (AVR_TMP_REGNO);

          /* Restore zero REG.  */
          emit_pop_byte (AVR_ZERO_REGNO);
        }
    }

  if (!sibcall_p)
    emit_jump_insn (gen_return ());
}


/* Implement `TARGET_ASM_FUNCTION_BEGIN_EPILOGUE'.  */

static void
avr_asm_function_begin_epilogue (FILE *file)
{
  app_disable();
  fprintf (file, "/* epilogue start */\n");
}


/* Implement `TARGET_CANNOT_MODITY_JUMPS_P'.  */

static bool
avr_cannot_modify_jumps_p (void)
{
  /* Naked Functions must not have any instructions after
     their epilogue, see PR42240 */

  if (reload_completed
      && cfun->machine
      && cfun->machine->is_naked)
    {
      return true;
    }

  return false;
}


/* Implement `TARGET_MODE_DEPENDENT_ADDRESS_P'.  */

static bool
avr_mode_dependent_address_p (const_rtx addr ATTRIBUTE_UNUSED, addr_space_t as)
{
  /* FIXME:  Non-generic addresses are not mode-dependent in themselves.
       This hook just serves to hack around PR rtl-optimization/52543 by
       claiming that non-generic addresses were mode-dependent so that
       lower-subreg.c will skip these addresses.  lower-subreg.c sets up fake
       RTXes to probe SET and MEM costs and assumes that MEM is always in the
       generic address space which is not true.  */

  return !ADDR_SPACE_GENERIC_P (as);
}


/* Return true if rtx X is a CONST_INT, CONST or SYMBOL_REF
   address with the `absdata' variable attribute, i.e. respective
   data can be read / written by LDS / STS instruction.
   This is used only for AVR_TINY.  */

static bool
avr_address_tiny_absdata_p (rtx x, machine_mode mode)
{
  if (CONST == GET_CODE (x))
    x = XEXP (XEXP (x, 0), 0);

  if (SYMBOL_REF_P (x))
    return SYMBOL_REF_FLAGS (x) & AVR_SYMBOL_FLAG_TINY_ABSDATA;

  if (CONST_INT_P (x)
      && IN_RANGE (INTVAL (x), 0, 0xc0 - GET_MODE_SIZE (mode)))
    return true;

  return false;
}


/* Helper function for `avr_legitimate_address_p'.  */

static inline bool
avr_reg_ok_for_addr_p (rtx reg, addr_space_t as,
                       RTX_CODE outer_code, bool strict)
{
  return (REG_P (reg)
          && (avr_regno_mode_code_ok_for_base_p (REGNO (reg), QImode,
                                                 as, outer_code, UNKNOWN)
              || (!strict
                  && REGNO (reg) >= FIRST_PSEUDO_REGISTER)));
}


/* Return nonzero if X (an RTX) is a legitimate memory address on the target
   machine for a memory operand of mode MODE.  */

static bool
avr_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  bool ok = CONSTANT_ADDRESS_P (x);

  switch (GET_CODE (x))
    {
    case REG:
      ok = avr_reg_ok_for_addr_p (x, ADDR_SPACE_GENERIC,
                                  MEM, strict);

      if (strict
          && GET_MODE_SIZE (mode) > 4
          && REG_X == REGNO (x))
        {
          ok = false;
        }
      break;

    case POST_INC:
    case PRE_DEC:
      ok = avr_reg_ok_for_addr_p (XEXP (x, 0), ADDR_SPACE_GENERIC,
                                  GET_CODE (x), strict);
      break;

    case PLUS:
      {
        rtx reg = XEXP (x, 0);
        rtx op1 = XEXP (x, 1);

        if (REG_P (reg)
            && CONST_INT_P (op1)
            && INTVAL (op1) >= 0)
          {
            bool fit = IN_RANGE (INTVAL (op1), 0, MAX_LD_OFFSET (mode));

            if (fit)
              {
                ok = (! strict
                      || avr_reg_ok_for_addr_p (reg, ADDR_SPACE_GENERIC,
                                                PLUS, strict));

                if (reg == frame_pointer_rtx
                    || reg == arg_pointer_rtx)
                  {
                    ok = true;
                  }
              }
            else if (frame_pointer_needed
                     && reg == frame_pointer_rtx)
              {
                ok = true;
              }
          }
      }
      break;

    default:
      break;
    }

  if (AVR_TINY
      && CONSTANT_ADDRESS_P (x))
    {
      /* avrtiny's load / store instructions only cover addresses 0..0xbf:
         IN / OUT range is 0..0x3f and LDS / STS can access 0x40..0xbf.  */

      ok = avr_address_tiny_absdata_p (x, mode);
    }

  if (avr_log.legitimate_address_p)
    {
      avr_edump ("\n%?: ret=%d, mode=%m strict=%d "
                 "reload_completed=%d reload_in_progress=%d %s:",
                 ok, mode, strict, reload_completed, reload_in_progress,
                 reg_renumber ? "(reg_renumber)" : "");

      if (GET_CODE (x) == PLUS
          && REG_P (XEXP (x, 0))
          && CONST_INT_P (XEXP (x, 1))
          && IN_RANGE (INTVAL (XEXP (x, 1)), 0, MAX_LD_OFFSET (mode))
          && reg_renumber)
        {
          avr_edump ("(r%d ---> r%d)", REGNO (XEXP (x, 0)),
                     true_regnum (XEXP (x, 0)));
        }

      avr_edump ("\n%r\n", x);
    }

  return ok;
}


/* Former implementation of TARGET_LEGITIMIZE_ADDRESS,
   now only a helper for avr_addr_space_legitimize_address.  */
/* Attempts to replace X with a valid
   memory address for an operand of mode MODE  */

static rtx
avr_legitimize_address (rtx x, rtx oldx, machine_mode mode)
{
  bool big_offset_p = false;

  x = oldx;

  if (AVR_TINY)
    {
      if (CONSTANT_ADDRESS_P (x)
          && ! avr_address_tiny_absdata_p (x, mode))
        {
          x = force_reg (Pmode, x);
        }
    }

  if (GET_CODE (oldx) == PLUS
      && REG_P (XEXP (oldx, 0)))
    {
      if (REG_P (XEXP (oldx, 1)))
        x = force_reg (GET_MODE (oldx), oldx);
      else if (CONST_INT_P (XEXP (oldx, 1)))
        {
          int offs = INTVAL (XEXP (oldx, 1));
          if (frame_pointer_rtx != XEXP (oldx, 0)
              && offs > MAX_LD_OFFSET (mode))
            {
              big_offset_p = true;
              x = force_reg (GET_MODE (oldx), oldx);
            }
        }
    }

  if (avr_log.legitimize_address)
    {
      avr_edump ("\n%?: mode=%m\n %r\n", mode, oldx);

      if (x != oldx)
        avr_edump (" %s --> %r\n", big_offset_p ? "(big offset)" : "", x);
    }

  return x;
}


/* Implement `LEGITIMIZE_RELOAD_ADDRESS'.  */
/* This will allow register R26/27 to be used where it is no worse than normal
   base pointers R28/29 or R30/31.  For example, if base offset is greater
   than 63 bytes or for R++ or --R addressing.  */

rtx
avr_legitimize_reload_address (rtx *px, machine_mode mode,
                               int opnum, int type, int addr_type,
                               int ind_levels ATTRIBUTE_UNUSED,
                               rtx (*mk_memloc)(rtx,int))
{
  rtx x = *px;

  if (avr_log.legitimize_reload_address)
    avr_edump ("\n%?:%m %r\n", mode, x);

  if (1 && (GET_CODE (x) == POST_INC
            || GET_CODE (x) == PRE_DEC))
    {
      push_reload (XEXP (x, 0), XEXP (x, 0), &XEXP (x, 0), &XEXP (x, 0),
                   POINTER_REGS, GET_MODE (x), GET_MODE (x), 0, 0,
                   opnum, RELOAD_OTHER);

      if (avr_log.legitimize_reload_address)
        avr_edump (" RCLASS.1 = %R\n IN = %r\n OUT = %r\n",
                   POINTER_REGS, XEXP (x, 0), XEXP (x, 0));

      return x;
    }

  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && 0 == reg_equiv_constant (REGNO (XEXP (x, 0)))
      && CONST_INT_P (XEXP (x, 1))
      && INTVAL (XEXP (x, 1)) >= 1)
    {
      bool fit = INTVAL (XEXP (x, 1)) <= MAX_LD_OFFSET (mode);

      if (fit)
        {
          if (reg_equiv_address (REGNO (XEXP (x, 0))) != 0)
            {
              int regno = REGNO (XEXP (x, 0));
              rtx mem = mk_memloc (x, regno);

              push_reload (XEXP (mem, 0), NULL_RTX, &XEXP (mem, 0), NULL,
                           POINTER_REGS, Pmode, VOIDmode, 0, 0,
                           1, (enum reload_type) addr_type);

              if (avr_log.legitimize_reload_address)
                avr_edump (" RCLASS.2 = %R\n IN = %r\n OUT = %r\n",
                           POINTER_REGS, XEXP (mem, 0), NULL_RTX);

              push_reload (mem, NULL_RTX, &XEXP (x, 0), NULL,
                           BASE_POINTER_REGS, GET_MODE (x), VOIDmode, 0, 0,
                           opnum, (enum reload_type) type);

              if (avr_log.legitimize_reload_address)
                avr_edump (" RCLASS.2 = %R\n IN = %r\n OUT = %r\n",
                           BASE_POINTER_REGS, mem, NULL_RTX);

              return x;
            }
        }
      else if (! (frame_pointer_needed
                  && XEXP (x, 0) == frame_pointer_rtx))
        {
          push_reload (x, NULL_RTX, px, NULL,
                       POINTER_REGS, GET_MODE (x), VOIDmode, 0, 0,
                       opnum, (enum reload_type) type);

          if (avr_log.legitimize_reload_address)
            avr_edump (" RCLASS.3 = %R\n IN = %r\n OUT = %r\n",
                       POINTER_REGS, x, NULL_RTX);

          return x;
        }
    }

  return NULL_RTX;
}


/* Helper function to print assembler resp. track instruction
   sequence lengths.  Always return "".

   If PLEN == NULL:
       Output assembler code from template TPL with operands supplied
       by OPERANDS.  This is just forwarding to output_asm_insn.

   If PLEN != NULL:
       If N_WORDS >= 0  Add N_WORDS to *PLEN.
       If N_WORDS < 0   Set *PLEN to -N_WORDS.
       Don't output anything.
*/

static const char*
avr_asm_len (const char* tpl, rtx* operands, int* plen, int n_words)
{
  if (NULL == plen)
    {
      output_asm_insn (tpl, operands);
    }
  else
    {
      if (n_words < 0)
        *plen = -n_words;
      else
        *plen += n_words;
    }

  return "";
}


/* Return a pointer register name as a string.  */

static const char*
ptrreg_to_str (int regno)
{
  switch (regno)
    {
    case REG_X: return "X";
    case REG_Y: return "Y";
    case REG_Z: return "Z";
    default:
      output_operand_lossage ("address operand requires constraint for"
                              " X, Y, or Z register");
    }
  return NULL;
}

/* Return the condition name as a string.
   Used in conditional jump constructing  */

static const char*
cond_string (enum rtx_code code)
{
  switch (code)
    {
    case NE:
      return "ne";
    case EQ:
      return "eq";
    case GE:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
        return "pl";
      else
        return "ge";
    case LT:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
        return "mi";
      else
        return "lt";
    case GEU:
      return "sh";
    case LTU:
      return "lo";
    default:
      gcc_unreachable ();
    }

  return "";
}


/* Return true if rtx X is a CONST or SYMBOL_REF with progmem.
   This must be used for AVR_TINY only because on other cores
   the flash memory is not visible in the RAM address range and
   cannot be read by, say,  LD instruction.  */

static bool
avr_address_tiny_pm_p (rtx x)
{
  if (CONST == GET_CODE (x))
    x = XEXP (XEXP (x, 0), 0);

  if (SYMBOL_REF_P (x))
    return SYMBOL_REF_FLAGS (x) & AVR_SYMBOL_FLAG_TINY_PM;

  return false;
}

/* Implement `TARGET_PRINT_OPERAND_ADDRESS'.  */
/* Output ADDR to FILE as address.  */

static void
avr_print_operand_address (FILE *file, machine_mode /*mode*/, rtx addr)
{
  if (AVR_TINY
      && avr_address_tiny_pm_p (addr))
    {
      addr = plus_constant (Pmode, addr, avr_arch->flash_pm_offset);
    }

  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "%s", ptrreg_to_str (REGNO (addr)));
      break;

    case PRE_DEC:
      fprintf (file, "-%s", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;

    case POST_INC:
      fprintf (file, "%s+", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;

    default:
      if (CONSTANT_ADDRESS_P (addr)
          && text_segment_operand (addr, VOIDmode))
        {
          rtx x = addr;
          if (GET_CODE (x) == CONST)
            x = XEXP (x, 0);
          if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
            {
              /* Assembler gs() will implant word address.  Make offset
                 a byte offset inside gs() for assembler.  This is
                 needed because the more logical (constant+gs(sym)) is not
                 accepted by gas.  For 128K and smaller devices this is ok.
                 For large devices it will create a trampoline to offset
                 from symbol which may not be what the user really wanted.  */

              fprintf (file, "gs(");
              output_addr_const (file, XEXP (x, 0));
              fprintf (file, "+" HOST_WIDE_INT_PRINT_DEC ")",
                       2 * INTVAL (XEXP (x, 1)));
              if (AVR_3_BYTE_PC)
                if (warning (0, "pointer offset from symbol maybe incorrect"))
                  {
                    output_addr_const (stderr, addr);
                    fprintf (stderr, "\n");
                  }
            }
          else
            {
              fprintf (file, "gs(");
              output_addr_const (file, addr);
              fprintf (file, ")");
            }
        }
      else
        output_addr_const (file, addr);
    }
}


/* Implement `TARGET_PRINT_OPERAND_PUNCT_VALID_P'.  */

static bool
avr_print_operand_punct_valid_p (unsigned char code)
{
  return code == '~' || code == '!';
}


/* Implement `TARGET_PRINT_OPERAND'.  */
/* Output X as assembler operand to file FILE.
   For a description of supported %-codes, see top of avr.md.  */

static void
avr_print_operand (FILE *file, rtx x, int code)
{
  int abcd = 0, ef = 0, ij = 0;

  if (code >= 'A' && code <= 'D')
    abcd = code - 'A';
  else if (code == 'E' || code == 'F')
    ef = code - 'E';
  else if (code == 'I' || code == 'J')
    ij = code - 'I';

  if (code == '~')
    {
      if (!AVR_HAVE_JMP_CALL)
        fputc ('r', file);
    }
  else if (code == '!')
    {
      if (AVR_HAVE_EIJMP_EICALL)
        fputc ('e', file);
    }
  else if (code == 't'
           || code == 'T')
    {
      static int t_regno = -1;
      static int t_nbits = -1;

      if (REG_P (x) && t_regno < 0 && code == 'T')
        {
          t_regno = REGNO (x);
          t_nbits = GET_MODE_BITSIZE (GET_MODE (x));
        }
      else if (CONST_INT_P (x) && t_regno >= 0
               && IN_RANGE (INTVAL (x), 0, t_nbits - 1))
        {
          int bpos = INTVAL (x);

          fprintf (file, "%s", reg_names[t_regno + bpos / 8]);
          if (code == 'T')
            fprintf (file, ",%d", bpos % 8);

          t_regno = -1;
        }
      else
        fatal_insn ("operands to %T/%t must be reg + const_int:", x);
    }
  else if (code == 'E' || code == 'F')
    {
      rtx op = XEXP (x, 0);
      fprintf (file, "%s", reg_names[REGNO (op) + ef]);
    }
  else if (code == 'I' || code == 'J')
    {
      rtx op = XEXP (XEXP (x, 0), 0);
      fprintf (file, "%s", reg_names[REGNO (op) + ij]);
    }
  else if (REG_P (x))
    {
      if (x == zero_reg_rtx)
        fprintf (file, "__zero_reg__");
      else if (code == 'r' && REGNO (x) < 32)
        fprintf (file, "%d", (int) REGNO (x));
      else
        fprintf (file, "%s", reg_names[REGNO (x) + abcd]);
    }
  else if (CONST_INT_P (x))
    {
      HOST_WIDE_INT ival = INTVAL (x);

      if ('i' != code)
        fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival + abcd);
      else if (low_io_address_operand (x, VOIDmode)
               || high_io_address_operand (x, VOIDmode))
        {
          if (AVR_HAVE_RAMPZ && ival == avr_addr.rampz)
            fprintf (file, "__RAMPZ__");
          else if (AVR_HAVE_RAMPY && ival == avr_addr.rampy)
            fprintf (file, "__RAMPY__");
          else if (AVR_HAVE_RAMPX && ival == avr_addr.rampx)
            fprintf (file, "__RAMPX__");
          else if (AVR_HAVE_RAMPD && ival == avr_addr.rampd)
            fprintf (file, "__RAMPD__");
          else if ((AVR_XMEGA || AVR_TINY) && ival == avr_addr.ccp)
            fprintf (file, "__CCP__");
          else if (ival == avr_addr.sreg)   fprintf (file, "__SREG__");
          else if (ival == avr_addr.sp_l)   fprintf (file, "__SP_L__");
          else if (ival == avr_addr.sp_h)   fprintf (file, "__SP_H__");
          else
            {
              fprintf (file, HOST_WIDE_INT_PRINT_HEX,
                       ival - avr_arch->sfr_offset);
            }
        }
      else
        fatal_insn ("bad address, not an I/O address:", x);
    }
  else if (MEM_P (x))
    {
      rtx addr = XEXP (x, 0);

      if (code == 'm')
        {
          if (!CONSTANT_P (addr))
            fatal_insn ("bad address, not a constant:", addr);
          /* Assembler template with m-code is data - not progmem section */
          if (text_segment_operand (addr, VOIDmode))
            if (warning (0, "accessing data memory with"
                         " program memory address"))
              {
                output_addr_const (stderr, addr);
                fprintf(stderr,"\n");
              }
          output_addr_const (file, addr);
        }
      else if (code == 'i')
        {
          avr_print_operand (file, addr, 'i');
        }
      else if (code == 'o')
        {
          if (GET_CODE (addr) != PLUS)
            fatal_insn ("bad address, not (reg+disp):", addr);

          avr_print_operand (file, XEXP (addr, 1), 0);
        }
      else if (code == 'b')
        {
          if (GET_CODE (addr) != PLUS)
            fatal_insn ("bad address, not (reg+disp):", addr);

          avr_print_operand_address (file, VOIDmode, XEXP (addr, 0));
        }
      else if (code == 'p' || code == 'r')
        {
          if (GET_CODE (addr) != POST_INC && GET_CODE (addr) != PRE_DEC)
            fatal_insn ("bad address, not post_inc or pre_dec:", addr);

          if (code == 'p')
            /* X, Y, Z */
            avr_print_operand_address (file, VOIDmode, XEXP (addr, 0));
          else
            avr_print_operand (file, XEXP (addr, 0), 0);  /* r26, r28, r30 */
        }
      else if (GET_CODE (addr) == PLUS)
        {
          avr_print_operand_address (file, VOIDmode, XEXP (addr, 0));
          if (REGNO (XEXP (addr, 0)) == REG_X)
            fatal_insn ("internal compiler error.  Bad address:"
                        ,addr);
          fputc ('+', file);
          avr_print_operand (file, XEXP (addr, 1), code);
        }
      else
        avr_print_operand_address (file, VOIDmode, addr);
    }
  else if (code == 'i')
    {
      if (SYMBOL_REF_P (x) && (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_IO))
	avr_print_operand_address
	  (file, VOIDmode, plus_constant (HImode, x, -avr_arch->sfr_offset));
      else
	fatal_insn ("bad address, not an I/O address:", x);
    }
  else if (code == 'x')
    {
      /* Constant progmem address - like used in jmp or call */
      if (0 == text_segment_operand (x, VOIDmode))
        if (warning (0, "accessing program memory"
                     " with data memory address"))
          {
            output_addr_const (stderr, x);
            fprintf(stderr,"\n");
          }
      /* Use normal symbol for direct address no linker trampoline needed */
      output_addr_const (file, x);
    }
  else if (CONST_FIXED_P (x))
    {
      HOST_WIDE_INT ival = INTVAL (avr_to_int_mode (x));
      if (code != 0)
        output_operand_lossage ("Unsupported code '%c' for fixed-point:",
                                code);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival);
    }
  else if (CONST_DOUBLE_P (x))
    {
      long val;
      if (GET_MODE (x) != SFmode)
        fatal_insn ("internal compiler error.  Unknown mode:", x);
      REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), val);
      fprintf (file, "0x%lx", val);
    }
  else if (GET_CODE (x) == CONST_STRING)
    fputs (XSTR (x, 0), file);
  else if (code == 'j')
    fputs (cond_string (GET_CODE (x)), file);
  else if (code == 'k')
    fputs (cond_string (reverse_condition (GET_CODE (x))), file);
  else
    avr_print_operand_address (file, VOIDmode, x);
}


/* Implement TARGET_USE_BY_PIECES_INFRASTRUCTURE_P.  */

/* Prefer sequence of loads/stores for moves of size upto
   two - two pairs of load/store instructions are always better
   than the 5 instruction sequence for a loop (1 instruction
   for loop counter setup, and 4 for the body of the loop). */

static bool
avr_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT size,
                                    unsigned int align ATTRIBUTE_UNUSED,
                                    enum by_pieces_operation op,
                                    bool speed_p)
{
  if (op != MOVE_BY_PIECES
      || (speed_p && size > MOVE_MAX_PIECES))
    return default_use_by_pieces_infrastructure_p (size, align, op, speed_p);

  return size <= MOVE_MAX_PIECES;
}


/* Worker function for `NOTICE_UPDATE_CC'.  */
/* Update the condition code in the INSN.  */

void
avr_notice_update_cc (rtx body ATTRIBUTE_UNUSED, rtx_insn *insn)
{
  rtx set;
  enum attr_cc cc = get_attr_cc (insn);

  switch (cc)
    {
    default:
      break;

    case CC_PLUS:
    case CC_LDI:
      {
        rtx *op = recog_data.operand;
        int len_dummy, icc;

        /* Extract insn's operands.  */
        extract_constrain_insn_cached (insn);

        switch (cc)
          {
          default:
            gcc_unreachable();

          case CC_PLUS:
            avr_out_plus (insn, op, &len_dummy, &icc);
            cc = (enum attr_cc) icc;
            break;

          case CC_LDI:

            cc = (op[1] == CONST0_RTX (GET_MODE (op[0]))
                  && reg_overlap_mentioned_p (op[0], zero_reg_rtx))
              /* Loading zero-reg with 0 uses CLR and thus clobbers cc0.  */
              ? CC_CLOBBER
              /* Any other "r,rL" combination does not alter cc0.  */
              : CC_NONE;

            break;
          } /* inner switch */

        break;
      }
    } /* outer swicth */

  switch (cc)
    {
    default:
      /* Special values like CC_OUT_PLUS from above have been
         mapped to "standard" CC_* values so we never come here.  */

      gcc_unreachable();
      break;

    case CC_NONE:
      /* Insn does not affect CC at all, but it might set some registers
         that are stored in cc_status.  If such a register is affected by
         the current insn, for example by means of a SET or a CLOBBER,
         then we must reset cc_status; cf. PR77326.

         Unfortunately, set_of cannot be used as reg_overlap_mentioned_p
         will abort on COMPARE (which might be found in cc_status.value1/2).
         Thus work out the registers set by the insn and regs mentioned
         in cc_status.value1/2.  */

      if (cc_status.value1
          || cc_status.value2)
        {
          HARD_REG_SET regs_used;
          HARD_REG_SET regs_set;
          CLEAR_HARD_REG_SET (regs_used);

          if (cc_status.value1
              && !CONSTANT_P (cc_status.value1))
            {
              find_all_hard_regs (cc_status.value1, &regs_used);
            }

          if (cc_status.value2
              && !CONSTANT_P (cc_status.value2))
            {
              find_all_hard_regs (cc_status.value2, &regs_used);
            }

          find_all_hard_reg_sets (insn, &regs_set, false);

          if (hard_reg_set_intersect_p (regs_used, regs_set))
            {
              CC_STATUS_INIT;
            }
        }

      break; // CC_NONE

    case CC_SET_N:
      CC_STATUS_INIT;
      break;

    case CC_SET_ZN:
      set = single_set (insn);
      CC_STATUS_INIT;
      if (set)
        {
          cc_status.flags |= CC_NO_OVERFLOW;
          cc_status.value1 = SET_DEST (set);
        }
      break;

    case CC_SET_VZN:
      /* Insn like INC, DEC, NEG that set Z,N,V.  We currently don't make use
         of this combination, cf. also PR61055.  */
      CC_STATUS_INIT;
      break;

    case CC_SET_CZN:
      /* Insn sets the Z,N,C flags of CC to recog_operand[0].
         The V flag may or may not be known but that's ok because
         alter_cond will change tests to use EQ/NE.  */
      set = single_set (insn);
      CC_STATUS_INIT;
      if (set)
        {
          cc_status.value1 = SET_DEST (set);
          cc_status.flags |= CC_OVERFLOW_UNUSABLE;
        }
      break;

    case CC_COMPARE:
      set = single_set (insn);
      CC_STATUS_INIT;
      if (set)
        cc_status.value1 = SET_SRC (set);
      break;

    case CC_CLOBBER:
      /* Insn doesn't leave CC in a usable state.  */
      CC_STATUS_INIT;
      break;
    }
}

/* Choose mode for jump insn:
   1 - relative jump in range -63 <= x <= 62 ;
   2 - relative jump in range -2046 <= x <= 2045 ;
   3 - absolute jump (only for ATmega[16]03).  */

int
avr_jump_mode (rtx x, rtx_insn *insn)
{
  int dest_addr = INSN_ADDRESSES (INSN_UID (GET_CODE (x) == LABEL_REF
                                            ? XEXP (x, 0) : x));
  int cur_addr = INSN_ADDRESSES (INSN_UID (insn));
  int jump_distance = cur_addr - dest_addr;

  if (IN_RANGE (jump_distance, -63, 62))
    return 1;
  else if (IN_RANGE (jump_distance, -2046, 2045))
    return 2;
  else if (AVR_HAVE_JMP_CALL)
    return 3;

  return 2;
}

/* Return an AVR condition jump commands.
   X is a comparison RTX.
   LEN is a number returned by avr_jump_mode function.
   If REVERSE nonzero then condition code in X must be reversed.  */

const char*
ret_cond_branch (rtx x, int len, int reverse)
{
  RTX_CODE cond = reverse ? reverse_condition (GET_CODE (x)) : GET_CODE (x);

  switch (cond)
    {
    case GT:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return (len == 1 ? ("breq .+2" CR_TAB
			    "brpl %0") :
		len == 2 ? ("breq .+4" CR_TAB
			    "brmi .+2" CR_TAB
			    "rjmp %0") :
		("breq .+6" CR_TAB
		 "brmi .+4" CR_TAB
		 "jmp %0"));

      else
	return (len == 1 ? ("breq .+2" CR_TAB
			    "brge %0") :
		len == 2 ? ("breq .+4" CR_TAB
			    "brlt .+2" CR_TAB
			    "rjmp %0") :
		("breq .+6" CR_TAB
		 "brlt .+4" CR_TAB
		 "jmp %0"));
    case GTU:
      return (len == 1 ? ("breq .+2" CR_TAB
                          "brsh %0") :
              len == 2 ? ("breq .+4" CR_TAB
                          "brlo .+2" CR_TAB
                          "rjmp %0") :
              ("breq .+6" CR_TAB
               "brlo .+4" CR_TAB
               "jmp %0"));
    case LE:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return (len == 1 ? ("breq %0" CR_TAB
			    "brmi %0") :
		len == 2 ? ("breq .+2" CR_TAB
			    "brpl .+2" CR_TAB
			    "rjmp %0") :
		("breq .+2" CR_TAB
		 "brpl .+4" CR_TAB
		 "jmp %0"));
      else
	return (len == 1 ? ("breq %0" CR_TAB
			    "brlt %0") :
		len == 2 ? ("breq .+2" CR_TAB
			    "brge .+2" CR_TAB
			    "rjmp %0") :
		("breq .+2" CR_TAB
		 "brge .+4" CR_TAB
		 "jmp %0"));
    case LEU:
      return (len == 1 ? ("breq %0" CR_TAB
                          "brlo %0") :
              len == 2 ? ("breq .+2" CR_TAB
                          "brsh .+2" CR_TAB
			  "rjmp %0") :
              ("breq .+2" CR_TAB
               "brsh .+4" CR_TAB
	       "jmp %0"));
    default:
      if (reverse)
	{
	  switch (len)
	    {
	    case 1:
	      return "br%k1 %0";
	    case 2:
	      return ("br%j1 .+2" CR_TAB
		      "rjmp %0");
	    default:
	      return ("br%j1 .+4" CR_TAB
		      "jmp %0");
	    }
	}
      else
        {
          switch (len)
            {
            case 1:
              return "br%j1 %0";
            case 2:
              return ("br%k1 .+2" CR_TAB
                      "rjmp %0");
            default:
              return ("br%k1 .+4" CR_TAB
                      "jmp %0");
            }
        }
    }
  return "";
}


/* Worker function for `FINAL_PRESCAN_INSN'.  */
/* Output insn cost for next insn.  */

void
avr_final_prescan_insn (rtx_insn *insn, rtx *operand ATTRIBUTE_UNUSED,
                        int num_operands ATTRIBUTE_UNUSED)
{
  if (avr_log.rtx_costs)
    {
      rtx set = single_set (insn);

      if (set)
        fprintf (asm_out_file, "/* DEBUG: cost = %d.  */\n",
                 set_src_cost (SET_SRC (set), GET_MODE (SET_DEST (set)),
			       optimize_insn_for_speed_p ()));
      else
        fprintf (asm_out_file, "/* DEBUG: pattern-cost = %d.  */\n",
                 rtx_cost (PATTERN (insn), VOIDmode, INSN, 0,
                           optimize_insn_for_speed_p()));
    }

  if (avr_log.insn_addresses)
    fprintf (asm_out_file, ";; ADDR = %d\n",
             (int) INSN_ADDRESSES (INSN_UID (insn)));
}


/* Implement `TARGET_ASM_FINAL_POSTSCAN_INSN'.  */
/* When GAS generates (parts of) ISR prologue / epilogue for us, we must
   hint GAS about the end of the code to scan.  There migh be code located
   after the last epilogue.  */

static void
avr_asm_final_postscan_insn (FILE *stream, rtx_insn *insn, rtx*, int)
{
  if (cfun->machine->gasisr.yes
      && !next_real_insn (insn))
    {
      app_disable();
      fprintf (stream, "\t__gcc_isr %d,r%d\n", GASISR_Done,
               cfun->machine->gasisr.regno);
    }
}


/* Return 0 if undefined, 1 if always true or always false.  */

int
avr_simplify_comparison_p (machine_mode mode, RTX_CODE op, rtx x)
{
  unsigned int max = (mode == QImode ? 0xff :
                      mode == HImode ? 0xffff :
                      mode == PSImode ? 0xffffff :
                      mode == SImode ? 0xffffffff : 0);
  if (max && op && CONST_INT_P (x))
    {
      if (unsigned_condition (op) != op)
        max >>= 1;

      if (max != (INTVAL (x) & max)
          && INTVAL (x) != 0xff)
        return 1;
    }
  return 0;
}


/* Worker function for `FUNCTION_ARG_REGNO_P'.  */
/* Returns nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */

int
avr_function_arg_regno_p (int r)
{
  return AVR_TINY ? IN_RANGE (r, 20, 25) : IN_RANGE (r, 8, 25);
}


/* Worker function for `INIT_CUMULATIVE_ARGS'.  */
/* Initializing the variable cum for the state at the beginning
   of the argument list.  */

void
avr_init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype, rtx libname,
                          tree fndecl ATTRIBUTE_UNUSED)
{
  cum->nregs = AVR_TINY ? 6 : 18;
  cum->regno = FIRST_CUM_REG;
  if (!libname && stdarg_p (fntype))
    cum->nregs = 0;

  /* Assume the calle may be tail called */

  cfun->machine->sibcall_fails = 0;
}

/* Returns the number of registers to allocate for a function argument.  */

static int
avr_num_arg_regs (machine_mode mode, const_tree type)
{
  int size;

  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  /* Align all function arguments to start in even-numbered registers.
     Odd-sized arguments leave holes above them.  */

  return (size + 1) & ~1;
}


/* Implement `TARGET_FUNCTION_ARG'.  */
/* Controls whether a function argument is passed
   in a register, and which register.  */

static rtx
avr_function_arg (cumulative_args_t cum_v, machine_mode mode,
                  const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = avr_num_arg_regs (mode, type);

  if (cum->nregs && bytes <= cum->nregs)
    return gen_rtx_REG (mode, cum->regno - bytes);

  return NULL_RTX;
}


/* Implement `TARGET_FUNCTION_ARG_ADVANCE'.  */
/* Update the summarizer variable CUM to advance past an argument
   in the argument list.  */

static void
avr_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
                          const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = avr_num_arg_regs (mode, type);

  cum->nregs -= bytes;
  cum->regno -= bytes;

  /* A parameter is being passed in a call-saved register.  As the original
     contents of these regs has to be restored before leaving the function,
     a function must not pass arguments in call-saved regs in order to get
     tail-called.  */

  if (cum->regno >= 8
      && cum->nregs >= 0
      && !call_used_regs[cum->regno])
    {
      /* FIXME: We ship info on failing tail-call in struct machine_function.
         This uses internals of calls.c:expand_call() and the way args_so_far
         is used.  targetm.function_ok_for_sibcall() needs to be extended to
         pass &args_so_far, too.  At present, CUMULATIVE_ARGS is target
         dependent so that such an extension is not wanted.  */

      cfun->machine->sibcall_fails = 1;
    }

  /* Test if all registers needed by the ABI are actually available.  If the
     user has fixed a GPR needed to pass an argument, an (implicit) function
     call will clobber that fixed register.  See PR45099 for an example.  */

  if (cum->regno >= 8
      && cum->nregs >= 0)
    {
      for (int regno = cum->regno; regno < cum->regno + bytes; regno++)
        if (fixed_regs[regno])
          warning (0, "fixed register %s used to pass parameter to function",
                   reg_names[regno]);
    }

  if (cum->nregs <= 0)
    {
      cum->nregs = 0;
      cum->regno = FIRST_CUM_REG;
    }
}

/* Implement `TARGET_FUNCTION_OK_FOR_SIBCALL' */
/* Decide whether we can make a sibling call to a function.  DECL is the
   declaration of the function being targeted by the call and EXP is the
   CALL_EXPR representing the call.  */

static bool
avr_function_ok_for_sibcall (tree decl_callee, tree exp_callee)
{
  tree fntype_callee;

  /* Tail-calling must fail if callee-saved regs are used to pass
     function args.  We must not tail-call when `epilogue_restores'
     is used.  Unfortunately, we cannot tell at this point if that
     actually will happen or not, and we cannot step back from
     tail-calling.  Thus, we inhibit tail-calling with -mcall-prologues.  */

  if (cfun->machine->sibcall_fails
      || TARGET_CALL_PROLOGUES)
    {
      return false;
    }

  fntype_callee = TREE_TYPE (CALL_EXPR_FN (exp_callee));

  if (decl_callee)
    {
      decl_callee = TREE_TYPE (decl_callee);
    }
  else
    {
      decl_callee = fntype_callee;

      while (FUNCTION_TYPE != TREE_CODE (decl_callee)
             && METHOD_TYPE != TREE_CODE (decl_callee))
        {
          decl_callee = TREE_TYPE (decl_callee);
        }
    }

  /* Ensure that caller and callee have compatible epilogues */

  if (cfun->machine->is_interrupt
      || cfun->machine->is_signal
      || cfun->machine->is_naked
      || avr_naked_function_p (decl_callee)
      /* FIXME: For OS_task and OS_main, this might be over-conservative.  */
      || (avr_OS_task_function_p (decl_callee)
          != cfun->machine->is_OS_task)
      || (avr_OS_main_function_p (decl_callee)
          != cfun->machine->is_OS_main))
    {
      return false;
    }

  return true;
}

/***********************************************************************
  Functions for outputting various mov's for a various modes
************************************************************************/

/* Return true if a value of mode MODE is read from flash by
   __load_* function from libgcc.  */

bool
avr_load_libgcc_p (rtx op)
{
  machine_mode mode = GET_MODE (op);
  int n_bytes = GET_MODE_SIZE (mode);

  return (n_bytes > 2
          && !AVR_HAVE_LPMX
          && avr_mem_flash_p (op));
}

/* Return true if a value of mode MODE is read by __xload_* function.  */

bool
avr_xload_libgcc_p (machine_mode mode)
{
  int n_bytes = GET_MODE_SIZE (mode);

  return (n_bytes > 1
          || avr_n_flash > 1);
}


/* Fixme: This is a hack because secondary reloads don't works as expected.

   Find an unused d-register to be used as scratch in INSN.
   EXCLUDE is either NULL_RTX or some register. In the case where EXCLUDE
   is a register, skip all possible return values that overlap EXCLUDE.
   The policy for the returned register is similar to that of
   `reg_unused_after', i.e. the returned register may overlap the SET_DEST
   of INSN.

   Return a QImode d-register or NULL_RTX if nothing found.  */

static rtx
avr_find_unused_d_reg (rtx_insn *insn, rtx exclude)
{
  bool isr_p = (avr_interrupt_function_p (current_function_decl)
                || avr_signal_function_p (current_function_decl));

  for (int regno = 16; regno < 32; regno++)
    {
      rtx reg = all_regs_rtx[regno];

      if ((exclude
           && reg_overlap_mentioned_p (exclude, reg))
          || fixed_regs[regno])
        {
          continue;
        }

      /* Try non-live register */

      if (!df_regs_ever_live_p (regno)
          && (TREE_THIS_VOLATILE (current_function_decl)
              || cfun->machine->is_OS_task
              || cfun->machine->is_OS_main
              || (!isr_p && call_used_regs[regno])))
        {
          return reg;
        }

      /* Any live register can be used if it is unused after.
         Prologue/epilogue will care for it as needed.  */

      if (df_regs_ever_live_p (regno)
          && reg_unused_after (insn, reg))
        {
          return reg;
        }
    }

  return NULL_RTX;
}


/* Helper function for the next function in the case where only restricted
   version of LPM instruction is available.  */

static const char*
avr_out_lpm_no_lpmx (rtx_insn *insn, rtx *xop, int *plen)
{
  rtx dest = xop[0];
  rtx addr = xop[1];
  int n_bytes = GET_MODE_SIZE (GET_MODE (dest));
  int regno_dest;

  regno_dest = REGNO (dest);

  /* The implicit target register of LPM.  */
  xop[3] = lpm_reg_rtx;

  switch (GET_CODE (addr))
    {
    default:
      gcc_unreachable();

    case REG:

      gcc_assert (REG_Z == REGNO (addr));

      switch (n_bytes)
        {
        default:
          gcc_unreachable();

        case 1:
          avr_asm_len ("%4lpm", xop, plen, 1);

          if (regno_dest != LPM_REGNO)
            avr_asm_len ("mov %0,%3", xop, plen, 1);

          return "";

        case 2:
          if (REGNO (dest) == REG_Z)
            return avr_asm_len ("%4lpm"      CR_TAB
                                "push %3"    CR_TAB
                                "adiw %2,1"  CR_TAB
                                "%4lpm"      CR_TAB
                                "mov %B0,%3" CR_TAB
                                "pop %A0", xop, plen, 6);

          avr_asm_len ("%4lpm"      CR_TAB
                       "mov %A0,%3" CR_TAB
                       "adiw %2,1"  CR_TAB
                       "%4lpm"      CR_TAB
                       "mov %B0,%3", xop, plen, 5);

          if (!reg_unused_after (insn, addr))
            avr_asm_len ("sbiw %2,1", xop, plen, 1);

          break; /* 2 */
        }

      break; /* REG */

    case POST_INC:

      gcc_assert (REG_Z == REGNO (XEXP (addr, 0))
                  && n_bytes <= 4);

      if (regno_dest == LPM_REGNO)
        avr_asm_len ("%4lpm"      CR_TAB
                     "adiw %2,1", xop, plen, 2);
      else
        avr_asm_len ("%4lpm"      CR_TAB
                     "mov %A0,%3" CR_TAB
                     "adiw %2,1", xop, plen, 3);

      if (n_bytes >= 2)
        avr_asm_len ("%4lpm"      CR_TAB
                     "mov %B0,%3" CR_TAB
                     "adiw %2,1", xop, plen, 3);

      if (n_bytes >= 3)
        avr_asm_len ("%4lpm"      CR_TAB
                     "mov %C0,%3" CR_TAB
                     "adiw %2,1", xop, plen, 3);

      if (n_bytes >= 4)
        avr_asm_len ("%4lpm"      CR_TAB
                     "mov %D0,%3" CR_TAB
                     "adiw %2,1", xop, plen, 3);

      break; /* POST_INC */

    } /* switch CODE (addr) */

  return "";
}


/* If PLEN == NULL: Ouput instructions to load a value from a memory location
   OP[1] in AS1 to register OP[0].
   If PLEN != 0 set *PLEN to the length in words of the instruction sequence.
   Return "".  */

const char*
avr_out_lpm (rtx_insn *insn, rtx *op, int *plen)
{
  rtx xop[7];
  rtx dest = op[0];
  rtx src = SET_SRC (single_set (insn));
  rtx addr;
  int n_bytes = GET_MODE_SIZE (GET_MODE (dest));
  int segment;
  RTX_CODE code;
  addr_space_t as = MEM_ADDR_SPACE (src);

  if (plen)
    *plen = 0;

  if (MEM_P (dest))
    {
      warning (0, "writing to address space %qs not supported",
               avr_addrspace[MEM_ADDR_SPACE (dest)].name);

      return "";
    }

  addr = XEXP (src, 0);
  code = GET_CODE (addr);

  gcc_assert (REG_P (dest));
  gcc_assert (REG == code || POST_INC == code);

  xop[0] = dest;
  xop[1] = addr;
  xop[2] = lpm_addr_reg_rtx;
  xop[4] = xstring_empty;
  xop[5] = tmp_reg_rtx;
  xop[6] = XEXP (rampz_rtx, 0);

  segment = avr_addrspace[as].segment;

  /* Set RAMPZ as needed.  */

  if (segment)
    {
      xop[4] = GEN_INT (segment);
      xop[3] = avr_find_unused_d_reg (insn, lpm_addr_reg_rtx);

      if (xop[3] != NULL_RTX)
        {
          avr_asm_len ("ldi %3,%4" CR_TAB
                       "out %i6,%3", xop, plen, 2);
        }
      else if (segment == 1)
        {
          avr_asm_len ("clr %5" CR_TAB
                       "inc %5" CR_TAB
                       "out %i6,%5", xop, plen, 3);
        }
      else
        {
          avr_asm_len ("mov %5,%2"   CR_TAB
                       "ldi %2,%4"   CR_TAB
                       "out %i6,%2"  CR_TAB
                       "mov %2,%5", xop, plen, 4);
        }

      xop[4] = xstring_e;

      if (!AVR_HAVE_ELPMX)
        return avr_out_lpm_no_lpmx (insn, xop, plen);
    }
  else if (!AVR_HAVE_LPMX)
    {
      return avr_out_lpm_no_lpmx (insn, xop, plen);
    }

  /* We have [E]LPMX: Output reading from Flash the comfortable way.  */

  switch (GET_CODE (addr))
    {
    default:
      gcc_unreachable();

    case REG:

      gcc_assert (REG_Z == REGNO (addr));

      switch (n_bytes)
        {
        default:
          gcc_unreachable();

        case 1:
          return avr_asm_len ("%4lpm %0,%a2", xop, plen, 1);

        case 2:
          if (REGNO (dest) == REG_Z)
            return avr_asm_len ("%4lpm %5,%a2+" CR_TAB
                                "%4lpm %B0,%a2" CR_TAB
                                "mov %A0,%5", xop, plen, 3);
          else
            {
              avr_asm_len ("%4lpm %A0,%a2+" CR_TAB
                           "%4lpm %B0,%a2", xop, plen, 2);

              if (!reg_unused_after (insn, addr))
                avr_asm_len ("sbiw %2,1", xop, plen, 1);
            }

          break; /* 2 */

        case 3:

          avr_asm_len ("%4lpm %A0,%a2+" CR_TAB
                       "%4lpm %B0,%a2+" CR_TAB
                       "%4lpm %C0,%a2", xop, plen, 3);

          if (!reg_unused_after (insn, addr))
            avr_asm_len ("sbiw %2,2", xop, plen, 1);

          break; /* 3 */

        case 4:

          avr_asm_len ("%4lpm %A0,%a2+" CR_TAB
                       "%4lpm %B0,%a2+", xop, plen, 2);

          if (REGNO (dest) == REG_Z - 2)
            return avr_asm_len ("%4lpm %5,%a2+" CR_TAB
                                "%4lpm %C0,%a2" CR_TAB
                                "mov %D0,%5", xop, plen, 3);
          else
            {
              avr_asm_len ("%4lpm %C0,%a2+" CR_TAB
                           "%4lpm %D0,%a2", xop, plen, 2);

              if (!reg_unused_after (insn, addr))
                avr_asm_len ("sbiw %2,3", xop, plen, 1);
            }

          break; /* 4 */
        } /* n_bytes */

      break; /* REG */

    case POST_INC:

      gcc_assert (REG_Z == REGNO (XEXP (addr, 0))
                  && n_bytes <= 4);

      avr_asm_len                    ("%4lpm %A0,%a2+", xop, plen, 1);
      if (n_bytes >= 2)  avr_asm_len ("%4lpm %B0,%a2+", xop, plen, 1);
      if (n_bytes >= 3)  avr_asm_len ("%4lpm %C0,%a2+", xop, plen, 1);
      if (n_bytes >= 4)  avr_asm_len ("%4lpm %D0,%a2+", xop, plen, 1);

      break; /* POST_INC */

    } /* switch CODE (addr) */

  if (xop[4] == xstring_e && AVR_HAVE_RAMPD)
    {
      /* Reset RAMPZ to 0 so that EBI devices don't read garbage from RAM.  */

      xop[0] = zero_reg_rtx;
      avr_asm_len ("out %i6,%0", xop, plen, 1);
    }

  return "";
}


/* Worker function for xload_8 insn.  */

const char*
avr_out_xload (rtx_insn *insn ATTRIBUTE_UNUSED, rtx *op, int *plen)
{
  rtx xop[4];

  xop[0] = op[0];
  xop[1] = op[1];
  xop[2] = lpm_addr_reg_rtx;
  xop[3] = AVR_HAVE_LPMX ? op[0] : lpm_reg_rtx;

  avr_asm_len (AVR_HAVE_LPMX ? "lpm %3,%a2" : "lpm", xop, plen, -1);

  avr_asm_len ("sbrc %1,7" CR_TAB
               "ld %3,%a2", xop, plen, 2);

  if (REGNO (xop[0]) != REGNO (xop[3]))
    avr_asm_len ("mov %0,%3", xop, plen, 1);

  return "";
}


const char*
output_movqi (rtx_insn *insn, rtx operands[], int *plen)
{
  rtx dest = operands[0];
  rtx src = operands[1];

  if (avr_mem_flash_p (src)
      || avr_mem_flash_p (dest))
    {
      return avr_out_lpm (insn, operands, plen);
    }

  gcc_assert (1 == GET_MODE_SIZE (GET_MODE (dest)));

  if (REG_P (dest))
    {
      if (REG_P (src)) /* mov r,r */
        {
          if (test_hard_reg_class (STACK_REG, dest))
            return avr_asm_len ("out %0,%1", operands, plen, -1);
          else if (test_hard_reg_class (STACK_REG, src))
            return avr_asm_len ("in %0,%1", operands, plen, -1);

          return avr_asm_len ("mov %0,%1", operands, plen, -1);
        }
      else if (CONSTANT_P (src))
        {
          output_reload_in_const (operands, NULL_RTX, plen, false);
          return "";
        }
      else if (MEM_P (src))
        return out_movqi_r_mr (insn, operands, plen); /* mov r,m */
    }
  else if (MEM_P (dest))
    {
      rtx xop[2];

      xop[0] = dest;
      xop[1] = src == CONST0_RTX (GET_MODE (dest)) ? zero_reg_rtx : src;

      return out_movqi_mr_r (insn, xop, plen);
    }

  return "";
}


const char *
output_movhi (rtx_insn *insn, rtx xop[], int *plen)
{
  rtx dest = xop[0];
  rtx src = xop[1];

  gcc_assert (GET_MODE_SIZE (GET_MODE (dest)) == 2);

  if (avr_mem_flash_p (src)
      || avr_mem_flash_p (dest))
    {
      return avr_out_lpm (insn, xop, plen);
    }

  if (REG_P (dest))
    {
      if (REG_P (src)) /* mov r,r */
        {
          if (test_hard_reg_class (STACK_REG, dest))
            {
              if (AVR_HAVE_8BIT_SP)
                return avr_asm_len ("out __SP_L__,%A1", xop, plen, -1);

              if (AVR_XMEGA)
                return avr_asm_len ("out __SP_L__,%A1" CR_TAB
                                    "out __SP_H__,%B1", xop, plen, -2);

              /* Use simple load of SP if no interrupts are  used.  */

              return TARGET_NO_INTERRUPTS
                ? avr_asm_len ("out __SP_H__,%B1" CR_TAB
                               "out __SP_L__,%A1", xop, plen, -2)
                : avr_asm_len ("in __tmp_reg__,__SREG__"  CR_TAB
                               "cli"                      CR_TAB
                               "out __SP_H__,%B1"         CR_TAB
                               "out __SREG__,__tmp_reg__" CR_TAB
                               "out __SP_L__,%A1", xop, plen, -5);
            }
          else if (test_hard_reg_class (STACK_REG, src))
            {
              return !AVR_HAVE_SPH
                ? avr_asm_len ("in %A0,__SP_L__" CR_TAB
                               "clr %B0", xop, plen, -2)

                : avr_asm_len ("in %A0,__SP_L__" CR_TAB
                               "in %B0,__SP_H__", xop, plen, -2);
            }

          return AVR_HAVE_MOVW
            ? avr_asm_len ("movw %0,%1", xop, plen, -1)

            : avr_asm_len ("mov %A0,%A1" CR_TAB
                           "mov %B0,%B1", xop, plen, -2);
        } /* REG_P (src) */
      else if (CONSTANT_P (src))
        {
          return output_reload_inhi (xop, NULL, plen);
        }
      else if (MEM_P (src))
        {
          return out_movhi_r_mr (insn, xop, plen); /* mov r,m */
        }
    }
  else if (MEM_P (dest))
    {
      rtx xop[2];

      xop[0] = dest;
      xop[1] = src == CONST0_RTX (GET_MODE (dest)) ? zero_reg_rtx : src;

      return out_movhi_mr_r (insn, xop, plen);
    }

  fatal_insn ("invalid insn:", insn);

  return "";
}


/* Same as out_movqi_r_mr, but TINY does not have ADIW or SBIW */

static const char*
avr_out_movqi_r_mr_reg_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (src, 0);

  avr_asm_len (TINY_ADIW (%I1, %J1, %o1) CR_TAB
               "ld %0,%b1" , op, plen, -3);

  if (!reg_overlap_mentioned_p (dest, XEXP (x, 0))
      && !reg_unused_after (insn, XEXP (x, 0)))
    avr_asm_len (TINY_SBIW (%I1, %J1, %o1), op, plen, 2);

  return "";
}

static const char*
out_movqi_r_mr (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (src, 0);

  if (CONSTANT_ADDRESS_P (x))
    {
      int n_words = AVR_TINY ? 1 : 2;
      return io_address_operand (x, QImode)
        ? avr_asm_len ("in %0,%i1", op, plen, -1)
        : avr_asm_len ("lds %0,%m1", op, plen, -n_words);
    }

  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1)))
    {
      /* memory access by reg+disp */

      int disp = INTVAL (XEXP (x, 1));

      if (AVR_TINY)
        return avr_out_movqi_r_mr_reg_disp_tiny (insn, op, plen);

      if (disp - GET_MODE_SIZE (GET_MODE (src)) >= 63)
        {
          if (REGNO (XEXP (x, 0)) != REG_Y)
            fatal_insn ("incorrect insn:",insn);

          if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
            return avr_asm_len ("adiw r28,%o1-63" CR_TAB
                                "ldd %0,Y+63"     CR_TAB
                                "sbiw r28,%o1-63", op, plen, -3);

          return avr_asm_len ("subi r28,lo8(-%o1)" CR_TAB
                              "sbci r29,hi8(-%o1)" CR_TAB
                              "ld %0,Y"            CR_TAB
                              "subi r28,lo8(%o1)"  CR_TAB
                              "sbci r29,hi8(%o1)", op, plen, -5);
        }
      else if (REGNO (XEXP (x, 0)) == REG_X)
        {
          /* This is a paranoid case LEGITIMIZE_RELOAD_ADDRESS must exclude
             it but I have this situation with extremal optimizing options.  */

          avr_asm_len ("adiw r26,%o1" CR_TAB
                       "ld %0,X", op, plen, -2);

          if (!reg_overlap_mentioned_p (dest, XEXP (x, 0))
              && !reg_unused_after (insn, XEXP (x, 0)))
            {
              avr_asm_len ("sbiw r26,%o1", op, plen, 1);
            }

          return "";
        }

      return avr_asm_len ("ldd %0,%1", op, plen, -1);
    }

  return avr_asm_len ("ld %0,%1", op, plen, -1);
}


/* Same as movhi_r_mr, but TINY does not have ADIW, SBIW and LDD */

static const char*
avr_out_movhi_r_mr_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);

  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  if (reg_dest == reg_base)         /* R = (R) */
    return avr_asm_len ("ld __tmp_reg__,%1+" CR_TAB
			"ld %B0,%1"          CR_TAB
			"mov %A0,__tmp_reg__", op, plen, -3);

  avr_asm_len ("ld %A0,%1+" CR_TAB
               "ld %B0,%1", op, plen, -2);

  if (!reg_unused_after (insn, base))
    avr_asm_len (TINY_SBIW (%E1, %F1, 1), op, plen, 2);

  return "";
}


/* Same as movhi_r_mr, but TINY does not have ADIW, SBIW and LDD */

static const char*
avr_out_movhi_r_mr_reg_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);

  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (XEXP (base, 0));

  if (reg_base == reg_dest)
    {
      return avr_asm_len (TINY_ADIW (%I1, %J1, %o1) CR_TAB
                          "ld __tmp_reg__,%b1+"     CR_TAB
                          "ld %B0,%b1"              CR_TAB
                          "mov %A0,__tmp_reg__", op, plen, -5);
    }
  else
    {
      avr_asm_len (TINY_ADIW (%I1, %J1, %o1) CR_TAB
                   "ld %A0,%b1+"             CR_TAB
                   "ld %B0,%b1", op, plen, -4);

      if (!reg_unused_after (insn, XEXP (base, 0)))
        avr_asm_len (TINY_SBIW (%I1, %J1, %o1+1), op, plen, 2);

      return "";
    }
}


/* Same as movhi_r_mr, but TINY does not have ADIW, SBIW and LDD */

static const char*
avr_out_movhi_r_mr_pre_dec_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  int mem_volatile_p = 0;
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);

  /* "volatile" forces reading low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers.  */
  mem_volatile_p = MEM_VOLATILE_P (src);

  if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
    fatal_insn ("incorrect insn:", insn);

  if (!mem_volatile_p)
    return avr_asm_len ("ld %B0,%1" CR_TAB
                        "ld %A0,%1", op, plen, -2);

  return avr_asm_len (TINY_SBIW (%I1, %J1, 2)  CR_TAB
                      "ld %A0,%p1+"            CR_TAB
                      "ld %B0,%p1"             CR_TAB
                      TINY_SBIW (%I1, %J1, 1), op, plen, -6);
}


static const char*
out_movhi_r_mr (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);
  /* "volatile" forces reading low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers.  */
  int mem_volatile_p = MEM_VOLATILE_P (src);

  if (reg_base > 0)
    {
      if (AVR_TINY)
        return avr_out_movhi_r_mr_reg_no_disp_tiny (insn, op, plen);

      if (reg_dest == reg_base)         /* R = (R) */
        return avr_asm_len ("ld __tmp_reg__,%1+" CR_TAB
                            "ld %B0,%1"          CR_TAB
                            "mov %A0,__tmp_reg__", op, plen, -3);

      if (reg_base != REG_X)
        return avr_asm_len ("ld %A0,%1" CR_TAB
                            "ldd %B0,%1+1", op, plen, -2);

      avr_asm_len ("ld %A0,X+" CR_TAB
                   "ld %B0,X", op, plen, -2);

      if (!reg_unused_after (insn, base))
        avr_asm_len ("sbiw r26,1", op, plen, 1);

      return "";
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));
      int reg_base = true_regnum (XEXP (base, 0));

      if (AVR_TINY)
        return avr_out_movhi_r_mr_reg_disp_tiny (insn, op, plen);

      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
        {
          if (REGNO (XEXP (base, 0)) != REG_Y)
            fatal_insn ("incorrect insn:",insn);

          return disp <= 63 + MAX_LD_OFFSET (GET_MODE (src))
            ? avr_asm_len ("adiw r28,%o1-62" CR_TAB
                           "ldd %A0,Y+62"    CR_TAB
                           "ldd %B0,Y+63"    CR_TAB
                           "sbiw r28,%o1-62", op, plen, -4)

            : avr_asm_len ("subi r28,lo8(-%o1)" CR_TAB
                           "sbci r29,hi8(-%o1)" CR_TAB
                           "ld %A0,Y"           CR_TAB
                           "ldd %B0,Y+1"        CR_TAB
                           "subi r28,lo8(%o1)"  CR_TAB
                           "sbci r29,hi8(%o1)", op, plen, -6);
        }

      /* This is a paranoid case. LEGITIMIZE_RELOAD_ADDRESS must exclude
         it but I have this situation with extremal
         optimization options.  */

      if (reg_base == REG_X)
        {
          if (reg_base == reg_dest)
            return avr_asm_len ("adiw r26,%o1"      CR_TAB
                                "ld __tmp_reg__,X+" CR_TAB
                                "ld %B0,X"          CR_TAB
                                "mov %A0,__tmp_reg__", op, plen, -4);

          avr_asm_len ("adiw r26,%o1" CR_TAB
                       "ld %A0,X+"    CR_TAB
                       "ld %B0,X", op, plen, -3);

          if (!reg_unused_after (insn, XEXP (base, 0)))
            avr_asm_len ("sbiw r26,%o1+1", op, plen, 1);

          return "";
        }

      return reg_base == reg_dest
        ? avr_asm_len ("ldd __tmp_reg__,%A1" CR_TAB
                       "ldd %B0,%B1"         CR_TAB
                       "mov %A0,__tmp_reg__", op, plen, -3)

        : avr_asm_len ("ldd %A0,%A1" CR_TAB
                       "ldd %B0,%B1", op, plen, -2);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    {
      if (AVR_TINY)
	return avr_out_movhi_r_mr_pre_dec_tiny (insn, op, plen);

      if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
        fatal_insn ("incorrect insn:", insn);

      if (!mem_volatile_p)
        return avr_asm_len ("ld %B0,%1" CR_TAB
                            "ld %A0,%1", op, plen, -2);

      return REGNO (XEXP (base, 0)) == REG_X
        ? avr_asm_len ("sbiw r26,2"  CR_TAB
                       "ld %A0,X+"   CR_TAB
                       "ld %B0,X"    CR_TAB
                       "sbiw r26,1", op, plen, -4)

        : avr_asm_len ("sbiw %r1,2"  CR_TAB
                       "ld %A0,%p1"  CR_TAB
                       "ldd %B0,%p1+1", op, plen, -3);
    }
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    {
      if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
        fatal_insn ("incorrect insn:", insn);

      return avr_asm_len ("ld %A0,%1"  CR_TAB
                          "ld %B0,%1", op, plen, -2);
    }
  else if (CONSTANT_ADDRESS_P (base))
    {
      int n_words = AVR_TINY ? 2 : 4;
      return io_address_operand (base, HImode)
        ? avr_asm_len ("in %A0,%i1" CR_TAB
                       "in %B0,%i1+1", op, plen, -2)

        : avr_asm_len ("lds %A0,%m1" CR_TAB
                       "lds %B0,%m1+1", op, plen, -n_words);
    }

  fatal_insn ("unknown move insn:",insn);
  return "";
}

static const char*
avr_out_movsi_r_mr_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  if (reg_dest == reg_base)
    {
      /* "ld r26,-X" is undefined */
      return *l = 9, (TINY_ADIW (%E1, %F1, 3) CR_TAB
		      "ld %D0,%1"             CR_TAB
		      "ld %C0,-%1"            CR_TAB
		      "ld __tmp_reg__,-%1"    CR_TAB
		      TINY_SBIW (%E1, %F1, 1) CR_TAB
		      "ld %A0,%1"             CR_TAB
		      "mov %B0,__tmp_reg__");
    }
  else if (reg_dest == reg_base - 2)
    {
      return *l = 5, ("ld %A0,%1+"            CR_TAB
		      "ld %B0,%1+"            CR_TAB
		      "ld __tmp_reg__,%1+"    CR_TAB
		      "ld %D0,%1"             CR_TAB
		      "mov %C0,__tmp_reg__");
    }
  else if (reg_unused_after (insn, base))
    {
      return *l = 4, ("ld %A0,%1+"    CR_TAB
		      "ld %B0,%1+"    CR_TAB
		      "ld %C0,%1+"    CR_TAB
		      "ld %D0,%1");
    }
  else
    {
      return *l = 6, ("ld %A0,%1+"    CR_TAB
		      "ld %B0,%1+"    CR_TAB
		      "ld %C0,%1+"    CR_TAB
		      "ld %D0,%1"     CR_TAB
		      TINY_SBIW (%E1, %F1, 3));
    }
}


static const char*
avr_out_movsi_r_mr_reg_disp_tiny (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (XEXP (base, 0));

  if (reg_dest == reg_base)
    {
      /* "ld r26,-X" is undefined */
      return *l = 9, (TINY_ADIW (%I1, %J1, %o1+3) CR_TAB
                      "ld %D0,%b1"                CR_TAB
                      "ld %C0,-%b1"               CR_TAB
                      "ld __tmp_reg__,-%b1"       CR_TAB
                      TINY_SBIW (%I1, %J1, 1)     CR_TAB
                      "ld %A0,%b1"                CR_TAB
                      "mov %B0,__tmp_reg__");
    }
  else if (reg_dest == reg_base - 2)
    {
      return *l = 7, (TINY_ADIW (%I1, %J1, %o1) CR_TAB
                      "ld %A0,%b1+"             CR_TAB
                      "ld %B0,%b1+"             CR_TAB
                      "ld __tmp_reg__,%b1+"     CR_TAB
                      "ld %D0,%b1"              CR_TAB
                      "mov %C0,__tmp_reg__");
    }
  else if (reg_unused_after (insn, XEXP (base, 0)))
    {
      return *l = 6, (TINY_ADIW (%I1, %J1, %o1) CR_TAB
                      "ld %A0,%b1+"             CR_TAB
                      "ld %B0,%b1+"             CR_TAB
                      "ld %C0,%b1+"             CR_TAB
                      "ld %D0,%b1");
    }
  else
    {
      return *l = 8, (TINY_ADIW (%I1, %J1, %o1)  CR_TAB
                      "ld %A0,%b1+"              CR_TAB
                      "ld %B0,%b1+"              CR_TAB
                      "ld %C0,%b1+"              CR_TAB
                      "ld %D0,%b1"               CR_TAB
                      TINY_SBIW (%I1, %J1, %o1+3));
    }
}

static const char*
out_movsi_r_mr (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);
  int tmp;

  if (!l)
    l = &tmp;

  if (reg_base > 0)
    {
      if (AVR_TINY)
        return avr_out_movsi_r_mr_reg_no_disp_tiny (insn, op, l);

      if (reg_base == REG_X)        /* (R26) */
        {
          if (reg_dest == REG_X)
	    /* "ld r26,-X" is undefined */
	    return *l=7, ("adiw r26,3"        CR_TAB
			  "ld r29,X"          CR_TAB
			  "ld r28,-X"         CR_TAB
			  "ld __tmp_reg__,-X" CR_TAB
			  "sbiw r26,1"        CR_TAB
			  "ld r26,X"          CR_TAB
			  "mov r27,__tmp_reg__");
          else if (reg_dest == REG_X - 2)
            return *l=5, ("ld %A0,X+"          CR_TAB
                          "ld %B0,X+"          CR_TAB
                          "ld __tmp_reg__,X+"  CR_TAB
                          "ld %D0,X"           CR_TAB
                          "mov %C0,__tmp_reg__");
          else if (reg_unused_after (insn, base))
            return  *l=4, ("ld %A0,X+" CR_TAB
                           "ld %B0,X+" CR_TAB
                           "ld %C0,X+" CR_TAB
                           "ld %D0,X");
          else
            return  *l=5, ("ld %A0,X+" CR_TAB
                           "ld %B0,X+" CR_TAB
                           "ld %C0,X+" CR_TAB
                           "ld %D0,X"  CR_TAB
                           "sbiw r26,3");
        }
      else
        {
          if (reg_dest == reg_base)
            return *l=5, ("ldd %D0,%1+3" CR_TAB
                          "ldd %C0,%1+2" CR_TAB
                          "ldd __tmp_reg__,%1+1"  CR_TAB
                          "ld %A0,%1"  CR_TAB
                          "mov %B0,__tmp_reg__");
          else if (reg_base == reg_dest + 2)
            return *l=5, ("ld %A0,%1"             CR_TAB
                          "ldd %B0,%1+1"          CR_TAB
                          "ldd __tmp_reg__,%1+2"  CR_TAB
                          "ldd %D0,%1+3"          CR_TAB
                          "mov %C0,__tmp_reg__");
          else
            return *l=4, ("ld %A0,%1"    CR_TAB
                          "ldd %B0,%1+1" CR_TAB
                          "ldd %C0,%1+2" CR_TAB
                          "ldd %D0,%1+3");
        }
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));

      if (AVR_TINY)
        return avr_out_movsi_r_mr_reg_disp_tiny (insn, op, l);

      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
	{
	  if (REGNO (XEXP (base, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return *l = 6, ("adiw r28,%o1-60" CR_TAB
			    "ldd %A0,Y+60"    CR_TAB
			    "ldd %B0,Y+61"    CR_TAB
			    "ldd %C0,Y+62"    CR_TAB
			    "ldd %D0,Y+63"    CR_TAB
			    "sbiw r28,%o1-60");

	  return *l = 8, ("subi r28,lo8(-%o1)" CR_TAB
			  "sbci r29,hi8(-%o1)" CR_TAB
			  "ld %A0,Y"           CR_TAB
			  "ldd %B0,Y+1"        CR_TAB
			  "ldd %C0,Y+2"        CR_TAB
			  "ldd %D0,Y+3"        CR_TAB
			  "subi r28,lo8(%o1)"  CR_TAB
			  "sbci r29,hi8(%o1)");
	}

      reg_base = true_regnum (XEXP (base, 0));
      if (reg_base == REG_X)
	{
	  /* R = (X + d) */
	  if (reg_dest == REG_X)
	    {
	      *l = 7;
	      /* "ld r26,-X" is undefined */
	      return ("adiw r26,%o1+3"    CR_TAB
		      "ld r29,X"          CR_TAB
		      "ld r28,-X"         CR_TAB
		      "ld __tmp_reg__,-X" CR_TAB
		      "sbiw r26,1"        CR_TAB
		      "ld r26,X"          CR_TAB
		      "mov r27,__tmp_reg__");
	    }
	  *l = 6;
	  if (reg_dest == REG_X - 2)
	    return ("adiw r26,%o1"      CR_TAB
		    "ld r24,X+"         CR_TAB
		    "ld r25,X+"         CR_TAB
		    "ld __tmp_reg__,X+" CR_TAB
		    "ld r27,X"          CR_TAB
		    "mov r26,__tmp_reg__");

	  return ("adiw r26,%o1" CR_TAB
		  "ld %A0,X+"    CR_TAB
		  "ld %B0,X+"    CR_TAB
		  "ld %C0,X+"    CR_TAB
		  "ld %D0,X"     CR_TAB
		  "sbiw r26,%o1+3");
	}
      if (reg_dest == reg_base)
        return *l=5, ("ldd %D0,%D1"          CR_TAB
                      "ldd %C0,%C1"          CR_TAB
                      "ldd __tmp_reg__,%B1"  CR_TAB
                      "ldd %A0,%A1"          CR_TAB
                      "mov %B0,__tmp_reg__");
      else if (reg_dest == reg_base - 2)
        return *l=5, ("ldd %A0,%A1"          CR_TAB
                      "ldd %B0,%B1"          CR_TAB
                      "ldd __tmp_reg__,%C1"  CR_TAB
                      "ldd %D0,%D1"          CR_TAB
                      "mov %C0,__tmp_reg__");
      return *l=4, ("ldd %A0,%A1" CR_TAB
                    "ldd %B0,%B1" CR_TAB
                    "ldd %C0,%C1" CR_TAB
                    "ldd %D0,%D1");
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=4, ("ld %D0,%1" CR_TAB
		  "ld %C0,%1" CR_TAB
		  "ld %B0,%1" CR_TAB
		  "ld %A0,%1");
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return *l=4, ("ld %A0,%1" CR_TAB
		  "ld %B0,%1" CR_TAB
		  "ld %C0,%1" CR_TAB
		  "ld %D0,%1");
  else if (CONSTANT_ADDRESS_P (base))
    {
      if (io_address_operand (base, SImode))
        {
          *l = 4;
          return ("in %A0,%i1"   CR_TAB
                  "in %B0,%i1+1" CR_TAB
                  "in %C0,%i1+2" CR_TAB
                  "in %D0,%i1+3");
        }
      else
        {
          *l = AVR_TINY ? 4 : 8;
          return ("lds %A0,%m1"   CR_TAB
                  "lds %B0,%m1+1" CR_TAB
                  "lds %C0,%m1+2" CR_TAB
                  "lds %D0,%m1+3");
        }
    }

  fatal_insn ("unknown move insn:",insn);
  return "";
}

static const char*
avr_out_movsi_mr_r_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);

  if (reg_base == reg_src)
    {
      /* "ld r26,-X" is undefined */
      if (reg_unused_after (insn, base))
        {
          return *l = 7, ("mov __tmp_reg__, %B1"  CR_TAB
			  "st %0,%A1"             CR_TAB
			  TINY_ADIW (%E0, %F0, 1) CR_TAB
			  "st %0+,__tmp_reg__"    CR_TAB
			  "st %0+,%C1"            CR_TAB
			  "st %0+,%D1");
        }
      else
        {
          return *l = 9, ("mov __tmp_reg__, %B1"  CR_TAB
			  "st %0,%A1"             CR_TAB
			  TINY_ADIW (%E0, %F0, 1) CR_TAB
			  "st %0+,__tmp_reg__"    CR_TAB
			  "st %0+,%C1"            CR_TAB
			  "st %0+,%D1"            CR_TAB
			  TINY_SBIW (%E0, %F0, 3));
        }
    }
  else if (reg_base == reg_src + 2)
    {
      if (reg_unused_after (insn, base))
	return *l = 7, ("mov __zero_reg__,%C1" CR_TAB
                        "mov __tmp_reg__,%D1"  CR_TAB
                        "st %0+,%A1"           CR_TAB
                        "st %0+,%B1"           CR_TAB
                        "st %0+,__zero_reg__"  CR_TAB
                        "st %0,__tmp_reg__"    CR_TAB
                        "clr __zero_reg__");
      else
	return *l = 9, ("mov __zero_reg__,%C1" CR_TAB
			"mov __tmp_reg__,%D1"  CR_TAB
			"st %0+,%A1"           CR_TAB
			"st %0+,%B1"           CR_TAB
			"st %0+,__zero_reg__"  CR_TAB
			"st %0,__tmp_reg__"    CR_TAB
			"clr __zero_reg__"     CR_TAB
			TINY_SBIW (%E0, %F0, 3));
    }

  return *l = 6, ("st %0+,%A1" CR_TAB
		  "st %0+,%B1" CR_TAB
		  "st %0+,%C1" CR_TAB
		  "st %0,%D1"  CR_TAB
		  TINY_SBIW (%E0, %F0, 3));
}

static const char*
avr_out_movsi_mr_r_reg_disp_tiny (rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = REGNO (XEXP (base, 0));
  int reg_src =true_regnum (src);

  if (reg_base == reg_src)
    {
      *l = 11;
      return ("mov __tmp_reg__,%A2"        CR_TAB
              "mov __zero_reg__,%B2"       CR_TAB
              TINY_ADIW (%I0, %J0, %o0)    CR_TAB
              "st %b0+,__tmp_reg__"        CR_TAB
              "st %b0+,__zero_reg__"       CR_TAB
              "st %b0+,%C2"                CR_TAB
              "st %b0,%D2"                 CR_TAB
              "clr __zero_reg__"           CR_TAB
              TINY_SBIW (%I0, %J0, %o0+3));
    }
  else if (reg_src == reg_base - 2)
    {
      *l = 11;
      return ("mov __tmp_reg__,%C2"         CR_TAB
              "mov __zero_reg__,%D2"        CR_TAB
              TINY_ADIW (%I0, %J0, %o0)     CR_TAB
              "st %b0+,%A0"                 CR_TAB
              "st %b0+,%B0"                 CR_TAB
              "st %b0+,__tmp_reg__"         CR_TAB
              "st %b0,__zero_reg__"         CR_TAB
              "clr __zero_reg__"            CR_TAB
              TINY_SBIW (%I0, %J0, %o0+3));
    }
  *l = 8;
  return (TINY_ADIW (%I0, %J0, %o0)     CR_TAB
          "st %b0+,%A1"                 CR_TAB
          "st %b0+,%B1"                 CR_TAB
          "st %b0+,%C1"                 CR_TAB
          "st %b0,%D1"                  CR_TAB
          TINY_SBIW (%I0, %J0, %o0+3));
}

static const char*
out_movsi_mr_r (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);
  int tmp;

  if (!l)
    l = &tmp;

  if (CONSTANT_ADDRESS_P (base))
    {
      if (io_address_operand (base, SImode))
        {
          return *l=4,("out %i0, %A1"  CR_TAB
                       "out %i0+1,%B1" CR_TAB
                       "out %i0+2,%C1" CR_TAB
                       "out %i0+3,%D1");
        }
      else
        {
          *l = AVR_TINY ? 4 : 8;
          return ("sts %m0,%A1"   CR_TAB
                  "sts %m0+1,%B1" CR_TAB
                  "sts %m0+2,%C1" CR_TAB
                  "sts %m0+3,%D1");
        }
    }

  if (reg_base > 0)                 /* (r) */
    {
      if (AVR_TINY)
        return avr_out_movsi_mr_r_reg_no_disp_tiny (insn, op, l);

      if (reg_base == REG_X)                /* (R26) */
        {
          if (reg_src == REG_X)
            {
	      /* "st X+,r26" is undefined */
              if (reg_unused_after (insn, base))
		return *l=6, ("mov __tmp_reg__,r27" CR_TAB
			      "st X,r26"            CR_TAB
			      "adiw r26,1"          CR_TAB
			      "st X+,__tmp_reg__"   CR_TAB
			      "st X+,r28"           CR_TAB
			      "st X,r29");
              else
                return *l=7, ("mov __tmp_reg__,r27" CR_TAB
			      "st X,r26"            CR_TAB
			      "adiw r26,1"          CR_TAB
			      "st X+,__tmp_reg__"   CR_TAB
			      "st X+,r28"           CR_TAB
			      "st X,r29"            CR_TAB
			      "sbiw r26,3");
            }
          else if (reg_base == reg_src + 2)
            {
              if (reg_unused_after (insn, base))
                return *l=7, ("mov __zero_reg__,%C1" CR_TAB
                              "mov __tmp_reg__,%D1"  CR_TAB
                              "st %0+,%A1"           CR_TAB
                              "st %0+,%B1"           CR_TAB
                              "st %0+,__zero_reg__"  CR_TAB
                              "st %0,__tmp_reg__"    CR_TAB
                              "clr __zero_reg__");
              else
                return *l=8, ("mov __zero_reg__,%C1" CR_TAB
                              "mov __tmp_reg__,%D1"  CR_TAB
                              "st %0+,%A1"           CR_TAB
                              "st %0+,%B1"           CR_TAB
                              "st %0+,__zero_reg__"  CR_TAB
                              "st %0,__tmp_reg__"    CR_TAB
                              "clr __zero_reg__"     CR_TAB
                              "sbiw r26,3");
            }
          return *l=5, ("st %0+,%A1" CR_TAB
                        "st %0+,%B1" CR_TAB
                        "st %0+,%C1" CR_TAB
                        "st %0,%D1"  CR_TAB
                        "sbiw r26,3");
        }
      else
        return *l=4, ("st %0,%A1"    CR_TAB
		      "std %0+1,%B1" CR_TAB
		      "std %0+2,%C1" CR_TAB
		      "std %0+3,%D1");
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));

      if (AVR_TINY)
        return avr_out_movsi_mr_r_reg_disp_tiny (op, l);

      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
	{
	  if (reg_base != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return *l = 6, ("adiw r28,%o0-60" CR_TAB
			    "std Y+60,%A1"    CR_TAB
			    "std Y+61,%B1"    CR_TAB
			    "std Y+62,%C1"    CR_TAB
			    "std Y+63,%D1"    CR_TAB
			    "sbiw r28,%o0-60");

	  return *l = 8, ("subi r28,lo8(-%o0)" CR_TAB
			  "sbci r29,hi8(-%o0)" CR_TAB
			  "st Y,%A1"           CR_TAB
			  "std Y+1,%B1"        CR_TAB
			  "std Y+2,%C1"        CR_TAB
			  "std Y+3,%D1"        CR_TAB
			  "subi r28,lo8(%o0)"  CR_TAB
			  "sbci r29,hi8(%o0)");
	}
      if (reg_base == REG_X)
	{
	  /* (X + d) = R */
	  if (reg_src == REG_X)
	    {
	      *l = 9;
	      return ("mov __tmp_reg__,r26"  CR_TAB
		      "mov __zero_reg__,r27" CR_TAB
		      "adiw r26,%o0"         CR_TAB
		      "st X+,__tmp_reg__"    CR_TAB
		      "st X+,__zero_reg__"   CR_TAB
		      "st X+,r28"            CR_TAB
		      "st X,r29"             CR_TAB
		      "clr __zero_reg__"     CR_TAB
		      "sbiw r26,%o0+3");
	    }
	  else if (reg_src == REG_X - 2)
	    {
	      *l = 9;
	      return ("mov __tmp_reg__,r26"  CR_TAB
		      "mov __zero_reg__,r27" CR_TAB
		      "adiw r26,%o0"         CR_TAB
		      "st X+,r24"            CR_TAB
		      "st X+,r25"            CR_TAB
		      "st X+,__tmp_reg__"    CR_TAB
		      "st X,__zero_reg__"    CR_TAB
		      "clr __zero_reg__"     CR_TAB
		      "sbiw r26,%o0+3");
	    }
	  *l = 6;
	  return ("adiw r26,%o0" CR_TAB
		  "st X+,%A1"    CR_TAB
		  "st X+,%B1"    CR_TAB
		  "st X+,%C1"    CR_TAB
		  "st X,%D1"     CR_TAB
		  "sbiw r26,%o0+3");
	}
      return *l=4, ("std %A0,%A1" CR_TAB
		    "std %B0,%B1" CR_TAB
		    "std %C0,%C1" CR_TAB
		    "std %D0,%D1");
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=4, ("st %0,%D1" CR_TAB
		  "st %0,%C1" CR_TAB
		  "st %0,%B1" CR_TAB
		  "st %0,%A1");
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return *l=4, ("st %0,%A1" CR_TAB
		  "st %0,%B1" CR_TAB
		  "st %0,%C1" CR_TAB
		  "st %0,%D1");
  fatal_insn ("unknown move insn:",insn);
  return "";
}

const char *
output_movsisf (rtx_insn *insn, rtx operands[], int *l)
{
  int dummy;
  rtx dest = operands[0];
  rtx src = operands[1];
  int *real_l = l;

  if (avr_mem_flash_p (src)
      || avr_mem_flash_p (dest))
    {
      return avr_out_lpm (insn, operands, real_l);
    }

  if (!l)
    l = &dummy;

  gcc_assert (4 == GET_MODE_SIZE (GET_MODE (dest)));

  if (REG_P (dest))
    {
      if (REG_P (src)) /* mov r,r */
	{
	  if (true_regnum (dest) > true_regnum (src))
	    {
	      if (AVR_HAVE_MOVW)
		{
		  *l = 2;
		  return ("movw %C0,%C1" CR_TAB
			  "movw %A0,%A1");
		}
	      *l = 4;
	      return ("mov %D0,%D1" CR_TAB
		      "mov %C0,%C1" CR_TAB
		      "mov %B0,%B1" CR_TAB
		      "mov %A0,%A1");
	    }
	  else
	    {
	      if (AVR_HAVE_MOVW)
		{
		  *l = 2;
		  return ("movw %A0,%A1" CR_TAB
			  "movw %C0,%C1");
		}
	      *l = 4;
	      return ("mov %A0,%A1" CR_TAB
		      "mov %B0,%B1" CR_TAB
		      "mov %C0,%C1" CR_TAB
		      "mov %D0,%D1");
	    }
	}
      else if (CONSTANT_P (src))
	{
          return output_reload_insisf (operands, NULL_RTX, real_l);
        }
      else if (MEM_P (src))
	return out_movsi_r_mr (insn, operands, real_l); /* mov r,m */
    }
  else if (MEM_P (dest))
    {
      const char *templ;

      if (src == CONST0_RTX (GET_MODE (dest)))
        operands[1] = zero_reg_rtx;

      templ = out_movsi_mr_r (insn, operands, real_l);

      if (!real_l)
	output_asm_insn (templ, operands);

      operands[1] = src;
      return "";
    }
  fatal_insn ("invalid insn:", insn);
  return "";
}


/* Handle loads of 24-bit types from memory to register.  */

static const char*
avr_out_load_psi_reg_no_disp_tiny (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  if (reg_base == reg_dest)
    {
      return avr_asm_len (TINY_ADIW (%E1, %F1, 2)   CR_TAB
                          "ld %C0,%1"               CR_TAB
                          "ld __tmp_reg__,-%1"      CR_TAB
                          TINY_SBIW (%E1, %F1, 1)   CR_TAB
                          "ld %A0,%1"               CR_TAB
                          "mov %B0,__tmp_reg__", op, plen, -8);
    }
  else
    {
      avr_asm_len ("ld %A0,%1+"  CR_TAB
		   "ld %B0,%1+"  CR_TAB
		   "ld %C0,%1", op, plen, -3);

      if (reg_dest != reg_base - 2
          && !reg_unused_after (insn, base))
        {
          avr_asm_len (TINY_SBIW (%E1, %F1, 2), op, plen, 2);
        }
      return "";
    }
}

static const char*
avr_out_load_psi_reg_disp_tiny (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  reg_base = true_regnum (XEXP (base, 0));
  if (reg_base == reg_dest)
    {
      return avr_asm_len (TINY_ADIW (%I1, %J1, %o1+2) CR_TAB
                          "ld %C0,%b1"                CR_TAB
                          "ld __tmp_reg__,-%b1"       CR_TAB
                          TINY_SBIW (%I1, %J1, 1)     CR_TAB
                          "ld %A0,%b1"                CR_TAB
                          "mov %B0,__tmp_reg__", op, plen, -8);
    }
  else
    {
      avr_asm_len (TINY_ADIW (%I1, %J1, %o1)   CR_TAB
                   "ld %A0,%b1+"               CR_TAB
                   "ld %B0,%b1+"               CR_TAB
                   "ld %C0,%b1", op, plen, -5);

      if (reg_dest != reg_base - 2
          && !reg_unused_after (insn, XEXP (base, 0)))
        avr_asm_len (TINY_SBIW (%I1, %J1, %o1+2), op, plen, 2);

      return "";
    }
}

static const char*
avr_out_load_psi (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  if (reg_base > 0)
    {
      if (AVR_TINY)
        return avr_out_load_psi_reg_no_disp_tiny (insn, op, plen);

      if (reg_base == REG_X)        /* (R26) */
        {
          if (reg_dest == REG_X)
            /* "ld r26,-X" is undefined */
            return avr_asm_len ("adiw r26,2"        CR_TAB
                                "ld r28,X"          CR_TAB
                                "ld __tmp_reg__,-X" CR_TAB
                                "sbiw r26,1"        CR_TAB
                                "ld r26,X"          CR_TAB
                                "mov r27,__tmp_reg__", op, plen, -6);
          else
            {
              avr_asm_len ("ld %A0,X+" CR_TAB
                           "ld %B0,X+" CR_TAB
                           "ld %C0,X", op, plen, -3);

              if (reg_dest != REG_X - 2
                  && !reg_unused_after (insn, base))
                {
                  avr_asm_len ("sbiw r26,2", op, plen, 1);
                }

              return "";
            }
        }
      else /* reg_base != REG_X */
        {
          if (reg_dest == reg_base)
            return avr_asm_len ("ldd %C0,%1+2"          CR_TAB
                                "ldd __tmp_reg__,%1+1"  CR_TAB
                                "ld  %A0,%1"            CR_TAB
                                "mov %B0,__tmp_reg__", op, plen, -4);
          else
            return avr_asm_len ("ld  %A0,%1"    CR_TAB
                                "ldd %B0,%1+1"  CR_TAB
                                "ldd %C0,%1+2", op, plen, -3);
        }
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));

      if (AVR_TINY)
        return avr_out_load_psi_reg_disp_tiny (insn, op, plen);

      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
        {
          if (REGNO (XEXP (base, 0)) != REG_Y)
            fatal_insn ("incorrect insn:",insn);

          if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
            return avr_asm_len ("adiw r28,%o1-61" CR_TAB
                                "ldd %A0,Y+61"    CR_TAB
                                "ldd %B0,Y+62"    CR_TAB
                                "ldd %C0,Y+63"    CR_TAB
                                "sbiw r28,%o1-61", op, plen, -5);

          return avr_asm_len ("subi r28,lo8(-%o1)" CR_TAB
                              "sbci r29,hi8(-%o1)" CR_TAB
                              "ld  %A0,Y"          CR_TAB
                              "ldd %B0,Y+1"        CR_TAB
                              "ldd %C0,Y+2"        CR_TAB
                              "subi r28,lo8(%o1)"  CR_TAB
                              "sbci r29,hi8(%o1)", op, plen, -7);
        }

      reg_base = true_regnum (XEXP (base, 0));
      if (reg_base == REG_X)
        {
          /* R = (X + d) */
          if (reg_dest == REG_X)
            {
              /* "ld r26,-X" is undefined */
              return avr_asm_len ("adiw r26,%o1+2"     CR_TAB
                                  "ld  r28,X"          CR_TAB
                                  "ld  __tmp_reg__,-X" CR_TAB
                                  "sbiw r26,1"         CR_TAB
                                  "ld  r26,X"          CR_TAB
                                  "mov r27,__tmp_reg__", op, plen, -6);
            }

          avr_asm_len ("adiw r26,%o1" CR_TAB
                       "ld %A0,X+"    CR_TAB
                       "ld %B0,X+"    CR_TAB
                       "ld %C0,X", op, plen, -4);

          if (reg_dest != REG_W
              && !reg_unused_after (insn, XEXP (base, 0)))
            avr_asm_len ("sbiw r26,%o1+2", op, plen, 1);

          return "";
        }

      if (reg_dest == reg_base)
        return avr_asm_len ("ldd %C0,%C1" CR_TAB
                            "ldd __tmp_reg__,%B1"  CR_TAB
                            "ldd %A0,%A1" CR_TAB
                            "mov %B0,__tmp_reg__", op, plen, -4);

      return avr_asm_len ("ldd %A0,%A1" CR_TAB
                          "ldd %B0,%B1" CR_TAB
                          "ldd %C0,%C1", op, plen, -3);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return avr_asm_len ("ld %C0,%1" CR_TAB
                        "ld %B0,%1" CR_TAB
                        "ld %A0,%1", op, plen, -3);
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return avr_asm_len ("ld %A0,%1" CR_TAB
                        "ld %B0,%1" CR_TAB
                        "ld %C0,%1", op, plen, -3);

  else if (CONSTANT_ADDRESS_P (base))
    {
      int n_words = AVR_TINY ? 3 : 6;
      return avr_asm_len ("lds %A0,%m1" CR_TAB
                          "lds %B0,%m1+1" CR_TAB
                          "lds %C0,%m1+2", op, plen , -n_words);
    }

  fatal_insn ("unknown move insn:",insn);
  return "";
}


static const char*
avr_out_store_psi_reg_no_disp_tiny (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);

  if (reg_base == reg_src)
    {
      avr_asm_len ("st %0,%A1"              CR_TAB
                   "mov __tmp_reg__,%B1"    CR_TAB
                   TINY_ADIW (%E0, %F0, 1)  CR_TAB /* st X+, r27 is undefined */
                   "st %0+,__tmp_reg__"     CR_TAB
                   "st %0,%C1", op, plen, -6);

    }
  else if (reg_src == reg_base - 2)
    {
      avr_asm_len ("st %0,%A1"              CR_TAB
                   "mov __tmp_reg__,%C1"    CR_TAB
                   TINY_ADIW (%E0, %F0, 1)  CR_TAB
                   "st %0+,%B1"             CR_TAB
                   "st %0,__tmp_reg__", op, plen, 6);
    }
  else
    {
      avr_asm_len ("st %0+,%A1"  CR_TAB
                   "st %0+,%B1" CR_TAB
                   "st %0,%C1", op, plen, -3);
    }

  if (!reg_unused_after (insn, base))
    avr_asm_len (TINY_SBIW (%E0, %F0, 2), op, plen, 2);

  return "";
}

static const char*
avr_out_store_psi_reg_disp_tiny (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = REGNO (XEXP (base, 0));
  int reg_src = true_regnum (src);

  if (reg_src == reg_base)
    avr_asm_len ("mov __tmp_reg__,%A1"          CR_TAB
                 "mov __zero_reg__,%B1"         CR_TAB
                 TINY_ADIW (%I0, %J0, %o0)      CR_TAB
                 "st %b0+,__tmp_reg__"          CR_TAB
                 "st %b0+,__zero_reg__"         CR_TAB
                 "st %b0,%C1"                   CR_TAB
                 "clr __zero_reg__", op, plen, -8);
  else if (reg_src == reg_base - 2)
    avr_asm_len ("mov __tmp_reg__,%C1"          CR_TAB
                 TINY_ADIW (%I0, %J0, %o0)      CR_TAB
                 "st %b0+,%A1"                  CR_TAB
                 "st %b0+,%B1"                  CR_TAB
                 "st %b0,__tmp_reg__", op, plen, -6);
  else
    avr_asm_len (TINY_ADIW (%I0, %J0, %o0)      CR_TAB
                 "st %b0+,%A1"                  CR_TAB
                 "st %b0+,%B1"                  CR_TAB
                 "st %b0,%C1", op, plen, -5);

  if (!reg_unused_after (insn, XEXP (base, 0)))
    avr_asm_len (TINY_SBIW (%I0, %J0, %o0+2), op, plen, 2);

  return "";
}

/* Handle store of 24-bit type from register or zero to memory.  */

static const char*
avr_out_store_psi (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);

  if (CONSTANT_ADDRESS_P (base))
    {
      int n_words = AVR_TINY ? 3 : 6;
      return avr_asm_len ("sts %m0,%A1"   CR_TAB
                          "sts %m0+1,%B1" CR_TAB
                          "sts %m0+2,%C1", op, plen, -n_words);
    }

  if (reg_base > 0)                 /* (r) */
    {
      if (AVR_TINY)
        return avr_out_store_psi_reg_no_disp_tiny (insn, op, plen);

      if (reg_base == REG_X)        /* (R26) */
        {
          gcc_assert (!reg_overlap_mentioned_p (base, src));

          avr_asm_len ("st %0+,%A1"  CR_TAB
                       "st %0+,%B1" CR_TAB
                       "st %0,%C1", op, plen, -3);

          if (!reg_unused_after (insn, base))
            avr_asm_len ("sbiw r26,2", op, plen, 1);

          return "";
        }
      else
        return avr_asm_len ("st %0,%A1"    CR_TAB
                            "std %0+1,%B1" CR_TAB
                            "std %0+2,%C1", op, plen, -3);
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));

      if (AVR_TINY)
        return avr_out_store_psi_reg_disp_tiny (insn, op, plen);

      reg_base = REGNO (XEXP (base, 0));

      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
        {
          if (reg_base != REG_Y)
            fatal_insn ("incorrect insn:",insn);

          if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
            return avr_asm_len ("adiw r28,%o0-61" CR_TAB
                                "std Y+61,%A1"    CR_TAB
                                "std Y+62,%B1"    CR_TAB
                                "std Y+63,%C1"    CR_TAB
                                "sbiw r28,%o0-61", op, plen, -5);

          return avr_asm_len ("subi r28,lo8(-%o0)" CR_TAB
                              "sbci r29,hi8(-%o0)" CR_TAB
                              "st Y,%A1"           CR_TAB
                              "std Y+1,%B1"        CR_TAB
                              "std Y+2,%C1"        CR_TAB
                              "subi r28,lo8(%o0)"  CR_TAB
                              "sbci r29,hi8(%o0)", op, plen, -7);
        }
      if (reg_base == REG_X)
        {
          /* (X + d) = R */
          gcc_assert (!reg_overlap_mentioned_p (XEXP (base, 0), src));

          avr_asm_len ("adiw r26,%o0" CR_TAB
                       "st X+,%A1"    CR_TAB
                       "st X+,%B1"    CR_TAB
                       "st X,%C1", op, plen, -4);

          if (!reg_unused_after (insn, XEXP (base, 0)))
            avr_asm_len ("sbiw r26,%o0+2", op, plen, 1);

          return "";
        }

      return avr_asm_len ("std %A0,%A1" CR_TAB
                          "std %B0,%B1" CR_TAB
                          "std %C0,%C1", op, plen, -3);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return avr_asm_len ("st %0,%C1" CR_TAB
                        "st %0,%B1" CR_TAB
                        "st %0,%A1", op, plen, -3);
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return avr_asm_len ("st %0,%A1" CR_TAB
                        "st %0,%B1" CR_TAB
                        "st %0,%C1", op, plen, -3);

  fatal_insn ("unknown move insn:",insn);
  return "";
}


/* Move around 24-bit stuff.  */

const char *
avr_out_movpsi (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];

  if (avr_mem_flash_p (src)
      || avr_mem_flash_p (dest))
    {
      return avr_out_lpm (insn, op, plen);
    }

  if (register_operand (dest, VOIDmode))
    {
      if (register_operand (src, VOIDmode)) /* mov r,r */
        {
          if (true_regnum (dest) > true_regnum (src))
            {
              avr_asm_len ("mov %C0,%C1", op, plen, -1);

              if (AVR_HAVE_MOVW)
                return avr_asm_len ("movw %A0,%A1", op, plen, 1);
              else
                return avr_asm_len ("mov %B0,%B1"  CR_TAB
                                    "mov %A0,%A1", op, plen, 2);
            }
          else
            {
              if (AVR_HAVE_MOVW)
                avr_asm_len ("movw %A0,%A1", op, plen, -1);
              else
                avr_asm_len ("mov %A0,%A1"  CR_TAB
                             "mov %B0,%B1", op, plen, -2);

              return avr_asm_len ("mov %C0,%C1", op, plen, 1);
            }
        }
      else if (CONSTANT_P (src))
        {
          return avr_out_reload_inpsi (op, NULL_RTX, plen);
        }
      else if (MEM_P (src))
        return avr_out_load_psi (insn, op, plen); /* mov r,m */
    }
  else if (MEM_P (dest))
    {
      rtx xop[2];

      xop[0] = dest;
      xop[1] = src == CONST0_RTX (GET_MODE (dest)) ? zero_reg_rtx : src;

      return avr_out_store_psi (insn, xop, plen);
    }

  fatal_insn ("invalid insn:", insn);
  return "";
}

static const char*
avr_out_movqi_mr_r_reg_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (dest, 0);

  if (reg_overlap_mentioned_p (src, XEXP (x, 0)))
    {
      avr_asm_len ("mov __tmp_reg__,%1"      CR_TAB
                   TINY_ADIW (%I0, %J0, %o0) CR_TAB
                   "st %b0,__tmp_reg__", op, plen, -4);
    }
  else
    {
      avr_asm_len (TINY_ADIW (%I0, %J0, %o0) CR_TAB
                   "st %b0,%1", op, plen, -3);
    }

  if (!reg_unused_after (insn, XEXP (x, 0)))
    avr_asm_len (TINY_SBIW (%I0, %J0, %o0), op, plen, 2);

  return "";
}

static const char*
out_movqi_mr_r (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (dest, 0);

  if (CONSTANT_ADDRESS_P (x))
    {
      int n_words = AVR_TINY ? 1 : 2;
      return io_address_operand (x, QImode)
        ? avr_asm_len ("out %i0,%1", op, plen, -1)
        : avr_asm_len ("sts %m0,%1", op, plen, -n_words);
    }
  else if (GET_CODE (x) == PLUS
           && REG_P (XEXP (x, 0))
           && CONST_INT_P (XEXP (x, 1)))
    {
      /* memory access by reg+disp */

      int disp = INTVAL (XEXP (x, 1));

      if (AVR_TINY)
        return avr_out_movqi_mr_r_reg_disp_tiny (insn, op, plen);

      if (disp - GET_MODE_SIZE (GET_MODE (dest)) >= 63)
        {
          if (REGNO (XEXP (x, 0)) != REG_Y)
            fatal_insn ("incorrect insn:",insn);

          if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
            return avr_asm_len ("adiw r28,%o0-63" CR_TAB
                                "std Y+63,%1"     CR_TAB
                                "sbiw r28,%o0-63", op, plen, -3);

          return avr_asm_len ("subi r28,lo8(-%o0)" CR_TAB
                              "sbci r29,hi8(-%o0)" CR_TAB
                              "st Y,%1"            CR_TAB
                              "subi r28,lo8(%o0)"  CR_TAB
                              "sbci r29,hi8(%o0)", op, plen, -5);
        }
      else if (REGNO (XEXP (x, 0)) == REG_X)
        {
          if (reg_overlap_mentioned_p (src, XEXP (x, 0)))
            {
              avr_asm_len ("mov __tmp_reg__,%1" CR_TAB
                           "adiw r26,%o0"       CR_TAB
                           "st X,__tmp_reg__", op, plen, -3);
            }
          else
            {
              avr_asm_len ("adiw r26,%o0" CR_TAB
                           "st X,%1", op, plen, -2);
            }

          if (!reg_unused_after (insn, XEXP (x, 0)))
            avr_asm_len ("sbiw r26,%o0", op, plen, 1);

          return "";
        }

      return avr_asm_len ("std %0,%1", op, plen, -1);
    }

  return avr_asm_len ("st %0,%1", op, plen, -1);
}


/* Helper for the next function for XMEGA.  It does the same
   but with low byte first.  */

static const char*
avr_out_movhi_mr_r_xmega (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);

  /* "volatile" forces writing low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers like SP.  */
  int mem_volatile_p = MEM_VOLATILE_P (dest);

  if (CONSTANT_ADDRESS_P (base))
    {
      return io_address_operand (base, HImode)
        ? avr_asm_len ("out %i0,%A1" CR_TAB
                       "out %i0+1,%B1", op, plen, -2)

        : avr_asm_len ("sts %m0,%A1" CR_TAB
                       "sts %m0+1,%B1", op, plen, -4);
    }

  if (reg_base > 0)
    {
      if (reg_base != REG_X)
        return avr_asm_len ("st %0,%A1" CR_TAB
                            "std %0+1,%B1", op, plen, -2);

      if (reg_src == REG_X)
        /* "st X+,r26" and "st -X,r26" are undefined.  */
        avr_asm_len ("mov __tmp_reg__,r27" CR_TAB
                     "st X,r26"            CR_TAB
                     "adiw r26,1"          CR_TAB
                     "st X,__tmp_reg__", op, plen, -4);
      else
        avr_asm_len ("st X+,%A1" CR_TAB
                     "st X,%B1", op, plen, -2);

      return reg_unused_after (insn, base)
        ? ""
        : avr_asm_len ("sbiw r26,1", op, plen, 1);
    }
  else if (GET_CODE (base) == PLUS)
    {
      int disp = INTVAL (XEXP (base, 1));
      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
        {
          if (reg_base != REG_Y)
            fatal_insn ("incorrect insn:",insn);

          return disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest))
            ? avr_asm_len ("adiw r28,%o0-62" CR_TAB
                           "std Y+62,%A1"    CR_TAB
                           "std Y+63,%B1"    CR_TAB
                           "sbiw r28,%o0-62", op, plen, -4)

            : avr_asm_len ("subi r28,lo8(-%o0)" CR_TAB
                           "sbci r29,hi8(-%o0)" CR_TAB
                           "st Y,%A1"           CR_TAB
                           "std Y+1,%B1"        CR_TAB
                           "subi r28,lo8(%o0)"  CR_TAB
                           "sbci r29,hi8(%o0)", op, plen, -6);
        }

      if (reg_base != REG_X)
        return avr_asm_len ("std %A0,%A1" CR_TAB
                            "std %B0,%B1", op, plen, -2);
      /* (X + d) = R */
      return reg_src == REG_X
        ? avr_asm_len ("mov __tmp_reg__,r26"  CR_TAB
                       "mov __zero_reg__,r27" CR_TAB
                       "adiw r26,%o0"         CR_TAB
                       "st X+,__tmp_reg__"    CR_TAB
                       "st X,__zero_reg__"    CR_TAB
                       "clr __zero_reg__"     CR_TAB
                       "sbiw r26,%o0+1", op, plen, -7)

        : avr_asm_len ("adiw r26,%o0" CR_TAB
                       "st X+,%A1"    CR_TAB
                       "st X,%B1"     CR_TAB
                       "sbiw r26,%o0+1", op, plen, -4);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    {
      if (!mem_volatile_p)
        return avr_asm_len ("st %0,%B1" CR_TAB
                            "st %0,%A1", op, plen, -2);

      return REGNO (XEXP (base, 0)) == REG_X
        ? avr_asm_len ("sbiw r26,2"  CR_TAB
                       "st X+,%A1"   CR_TAB
                       "st X,%B1"    CR_TAB
                       "sbiw r26,1", op, plen, -4)

        : avr_asm_len ("sbiw %r0,2"  CR_TAB
                       "st %p0,%A1"  CR_TAB
                       "std %p0+1,%B1", op, plen, -3);
    }
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    {
      return avr_asm_len ("st %0,%A1"  CR_TAB
                          "st %0,%B1", op, plen, -2);

    }
  fatal_insn ("unknown move insn:",insn);
  return "";
}

static const char*
avr_out_movhi_mr_r_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);
  int mem_volatile_p = MEM_VOLATILE_P (dest);

  if (reg_base == reg_src)
    {
      return !mem_volatile_p && reg_unused_after (insn, src)
        ? avr_asm_len ("mov __tmp_reg__,%B1"   CR_TAB
                       "st %0,%A1"             CR_TAB
                       TINY_ADIW (%E0, %F0, 1) CR_TAB
                       "st %0,__tmp_reg__", op, plen, -5)
        : avr_asm_len ("mov __tmp_reg__,%B1"   CR_TAB
                       TINY_ADIW (%E0, %F0, 1) CR_TAB
                       "st %0,__tmp_reg__"     CR_TAB
                       TINY_SBIW (%E0, %F0, 1) CR_TAB
                       "st %0, %A1", op, plen, -7);
    }

  return !mem_volatile_p && reg_unused_after (insn, base)
    ? avr_asm_len ("st %0+,%A1" CR_TAB
                   "st %0,%B1", op, plen, -2)
    : avr_asm_len (TINY_ADIW (%E0, %F0, 1) CR_TAB
                   "st %0,%B1"             CR_TAB
                   "st -%0,%A1", op, plen, -4);
}

static const char*
avr_out_movhi_mr_r_reg_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = REGNO (XEXP (base, 0));
  int reg_src = true_regnum (src);

  if (reg_src == reg_base)
    avr_asm_len ("mov __tmp_reg__,%A1"          CR_TAB
                 "mov __zero_reg__,%B1"         CR_TAB
                 TINY_ADIW (%I0, %J0, %o0+1)    CR_TAB
                 "st %b0,__zero_reg__"          CR_TAB
                 "st -%b0,__tmp_reg__"          CR_TAB
                 "clr __zero_reg__", op, plen, -7);
  else
    avr_asm_len (TINY_ADIW (%I0, %J0, %o0+1) CR_TAB
                 "st %b0,%B1"                CR_TAB
                 "st -%b0,%A1", op, plen, -4);

  if (!reg_unused_after (insn, XEXP (base, 0)))
    avr_asm_len (TINY_SBIW (%I0, %J0, %o0), op, plen, 2);

  return "";
}

static const char*
avr_out_movhi_mr_r_post_inc_tiny (rtx op[], int *plen)
{
  return avr_asm_len (TINY_ADIW (%I0, %J0, 1)  CR_TAB
                      "st %p0,%B1"    CR_TAB
                      "st -%p0,%A1"   CR_TAB
                      TINY_ADIW (%I0, %J0, 2), op, plen, -6);
}

static const char*
out_movhi_mr_r (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);
  int mem_volatile_p;

  /* "volatile" forces writing high-byte first (no-xmega) resp.
     low-byte first (xmega) even if less efficient, for correct
     operation with 16-bit I/O registers like.  */

  if (AVR_XMEGA)
    return avr_out_movhi_mr_r_xmega (insn, op, plen);

  mem_volatile_p = MEM_VOLATILE_P (dest);

  if (CONSTANT_ADDRESS_P (base))
    {
      int n_words = AVR_TINY ? 2 : 4;
      return io_address_operand (base, HImode)
        ? avr_asm_len ("out %i0+1,%B1" CR_TAB
                       "out %i0,%A1", op, plen, -2)

        : avr_asm_len ("sts %m0+1,%B1" CR_TAB
                       "sts %m0,%A1", op, plen, -n_words);
    }

  if (reg_base > 0)
    {
      if (AVR_TINY)
        return avr_out_movhi_mr_r_reg_no_disp_tiny (insn, op, plen);

      if (reg_base != REG_X)
        return avr_asm_len ("std %0+1,%B1" CR_TAB
                            "st %0,%A1", op, plen, -2);

      if (reg_src == REG_X)
        /* "st X+,r26" and "st -X,r26" are undefined.  */
        return !mem_volatile_p && reg_unused_after (insn, src)
          ? avr_asm_len ("mov __tmp_reg__,r27" CR_TAB
                         "st X,r26"            CR_TAB
                         "adiw r26,1"          CR_TAB
                         "st X,__tmp_reg__", op, plen, -4)

          : avr_asm_len ("mov __tmp_reg__,r27" CR_TAB
                         "adiw r26,1"          CR_TAB
                         "st X,__tmp_reg__"    CR_TAB
                         "sbiw r26,1"          CR_TAB
                         "st X,r26", op, plen, -5);

      return !mem_volatile_p && reg_unused_after (insn, base)
        ? avr_asm_len ("st X+,%A1" CR_TAB
                       "st X,%B1", op, plen, -2)
        : avr_asm_len ("adiw r26,1" CR_TAB
                       "st X,%B1"   CR_TAB
                       "st -X,%A1", op, plen, -3);
    }
  else if (GET_CODE (base) == PLUS)
    {
      int disp = INTVAL (XEXP (base, 1));

      if (AVR_TINY)
        return avr_out_movhi_mr_r_reg_disp_tiny (insn, op, plen);

      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
        {
          if (reg_base != REG_Y)
            fatal_insn ("incorrect insn:",insn);

          return disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest))
            ? avr_asm_len ("adiw r28,%o0-62" CR_TAB
                           "std Y+63,%B1"    CR_TAB
                           "std Y+62,%A1"    CR_TAB
                           "sbiw r28,%o0-62", op, plen, -4)

            : avr_asm_len ("subi r28,lo8(-%o0)" CR_TAB
                           "sbci r29,hi8(-%o0)" CR_TAB
                           "std Y+1,%B1"        CR_TAB
                           "st Y,%A1"           CR_TAB
                           "subi r28,lo8(%o0)"  CR_TAB
                           "sbci r29,hi8(%o0)", op, plen, -6);
        }

      if (reg_base != REG_X)
        return avr_asm_len ("std %B0,%B1" CR_TAB
                            "std %A0,%A1", op, plen, -2);
      /* (X + d) = R */
      return reg_src == REG_X
        ? avr_asm_len ("mov __tmp_reg__,r26"  CR_TAB
                       "mov __zero_reg__,r27" CR_TAB
                       "adiw r26,%o0+1"       CR_TAB
                       "st X,__zero_reg__"    CR_TAB
                       "st -X,__tmp_reg__"    CR_TAB
                       "clr __zero_reg__"     CR_TAB
                       "sbiw r26,%o0", op, plen, -7)

        : avr_asm_len ("adiw r26,%o0+1" CR_TAB
                       "st X,%B1"       CR_TAB
                       "st -X,%A1"      CR_TAB
                       "sbiw r26,%o0", op, plen, -4);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    {
      return avr_asm_len ("st %0,%B1" CR_TAB
                          "st %0,%A1", op, plen, -2);
    }
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    {
      if (!mem_volatile_p)
        return avr_asm_len ("st %0,%A1"  CR_TAB
                            "st %0,%B1", op, plen, -2);

      if (AVR_TINY)
        return avr_out_movhi_mr_r_post_inc_tiny (op, plen);

      return REGNO (XEXP (base, 0)) == REG_X
        ? avr_asm_len ("adiw r26,1"  CR_TAB
                       "st X,%B1"    CR_TAB
                       "st -X,%A1"   CR_TAB
                       "adiw r26,2", op, plen, -4)

        : avr_asm_len ("std %p0+1,%B1" CR_TAB
                       "st %p0,%A1"    CR_TAB
                       "adiw %r0,2", op, plen, -3);
    }
  fatal_insn ("unknown move insn:",insn);
  return "";
}

/* Return 1 if frame pointer for current function required.  */

static bool
avr_frame_pointer_required_p (void)
{
  return (cfun->calls_alloca
          || cfun->calls_setjmp
          || cfun->has_nonlocal_label
          || crtl->args.info.nregs == 0
          || get_frame_size () > 0);
}

/* Returns the condition of compare insn INSN, or UNKNOWN.  */

static RTX_CODE
compare_condition (rtx_insn *insn)
{
  rtx_insn *next = next_real_insn (insn);

  if (next && JUMP_P (next))
    {
      rtx pat = PATTERN (next);
      rtx src = SET_SRC (pat);

      if (IF_THEN_ELSE == GET_CODE (src))
        return GET_CODE (XEXP (src, 0));
    }

  return UNKNOWN;
}


/* Returns true iff INSN is a tst insn that only tests the sign.  */

static bool
compare_sign_p (rtx_insn *insn)
{
  RTX_CODE cond = compare_condition (insn);
  return (cond == GE || cond == LT);
}


/* Returns true iff the next insn is a JUMP_INSN with a condition
   that needs to be swapped (GT, GTU, LE, LEU).  */

static bool
compare_diff_p (rtx_insn *insn)
{
  RTX_CODE cond = compare_condition (insn);
  return (cond == GT || cond == GTU || cond == LE || cond == LEU) ? cond : 0;
}

/* Returns true iff INSN is a compare insn with the EQ or NE condition.  */

static bool
compare_eq_p (rtx_insn *insn)
{
  RTX_CODE cond = compare_condition (insn);
  return (cond == EQ || cond == NE);
}


/* Output compare instruction

      compare (XOP[0], XOP[1])

   for a register XOP[0] and a compile-time constant XOP[1].  Return "".
   XOP[2] is an 8-bit scratch register as needed.

   PLEN == NULL:  Output instructions.
   PLEN != NULL:  Set *PLEN to the length (in words) of the sequence.
                  Don't output anything.  */

const char*
avr_out_compare (rtx_insn *insn, rtx *xop, int *plen)
{
  /* Register to compare and value to compare against. */
  rtx xreg = xop[0];
  rtx xval = xop[1];

  /* MODE of the comparison.  */
  machine_mode mode;

  /* Number of bytes to operate on.  */
  int n_bytes = GET_MODE_SIZE (GET_MODE (xreg));

  /* Value (0..0xff) held in clobber register xop[2] or -1 if unknown.  */
  int clobber_val = -1;

  /* Map fixed mode operands to integer operands with the same binary
     representation.  They are easier to handle in the remainder.  */

  if (CONST_FIXED_P (xval))
    {
      xreg = avr_to_int_mode (xop[0]);
      xval = avr_to_int_mode (xop[1]);
    }

  mode = GET_MODE (xreg);

  gcc_assert (REG_P (xreg));
  gcc_assert ((CONST_INT_P (xval) && n_bytes <= 4)
              || (const_double_operand (xval, VOIDmode) && n_bytes == 8));

  if (plen)
    *plen = 0;

  /* Comparisons == +/-1 and != +/-1 can be done similar to camparing
     against 0 by ORing the bytes.  This is one instruction shorter.
     Notice that 64-bit comparisons are always against reg:ALL8 18 (ACC_A)
     and therefore don't use this.  */

  if (!test_hard_reg_class (LD_REGS, xreg)
      && compare_eq_p (insn)
      && reg_unused_after (insn, xreg))
    {
      if (xval == const1_rtx)
        {
          avr_asm_len ("dec %A0" CR_TAB
                       "or %A0,%B0", xop, plen, 2);

          if (n_bytes >= 3)
            avr_asm_len ("or %A0,%C0", xop, plen, 1);

          if (n_bytes >= 4)
            avr_asm_len ("or %A0,%D0", xop, plen, 1);

          return "";
        }
      else if (xval == constm1_rtx)
        {
          if (n_bytes >= 4)
            avr_asm_len ("and %A0,%D0", xop, plen, 1);

          if (n_bytes >= 3)
            avr_asm_len ("and %A0,%C0", xop, plen, 1);

          return avr_asm_len ("and %A0,%B0" CR_TAB
                              "com %A0", xop, plen, 2);
        }
    }

  /* Comparisons == -1 and != -1 of a d-register that's used after the
     comparison.  (If it's unused after we use CPI / SBCI or ADIW sequence
     from below.)  Instead of  CPI Rlo,-1 / LDI Rx,-1 / CPC Rhi,Rx  we can
     use  CPI Rlo,-1 / CPC Rhi,Rlo  which is 1 instruction shorter:
     If CPI is true then Rlo contains -1 and we can use Rlo instead of Rx
     when CPC'ing the high part.  If CPI is false then CPC cannot render
     the result to true.  This also works for the more generic case where
     the constant is of the form 0xabab.  */

  if (n_bytes == 2
      && xval != const0_rtx
      && test_hard_reg_class (LD_REGS, xreg)
      && compare_eq_p (insn)
      && !reg_unused_after (insn, xreg))
    {
      rtx xlo8 = simplify_gen_subreg (QImode, xval, mode, 0);
      rtx xhi8 = simplify_gen_subreg (QImode, xval, mode, 1);

      if (INTVAL (xlo8) == INTVAL (xhi8))
        {
          xop[0] = xreg;
          xop[1] = xlo8;

          return avr_asm_len ("cpi %A0,%1"  CR_TAB
                              "cpc %B0,%A0", xop, plen, 2);
        }
    }

  for (int i = 0; i < n_bytes; i++)
    {
      /* We compare byte-wise.  */
      rtx reg8 = simplify_gen_subreg (QImode, xreg, mode, i);
      rtx xval8 = simplify_gen_subreg (QImode, xval, mode, i);

      /* 8-bit value to compare with this byte.  */
      unsigned int val8 = UINTVAL (xval8) & GET_MODE_MASK (QImode);

      /* Registers R16..R31 can operate with immediate.  */
      bool ld_reg_p = test_hard_reg_class (LD_REGS, reg8);

      xop[0] = reg8;
      xop[1] = gen_int_mode (val8, QImode);

      /* Word registers >= R24 can use SBIW/ADIW with 0..63.  */

      if (i == 0
          && test_hard_reg_class (ADDW_REGS, reg8))
        {
          int val16 = trunc_int_for_mode (INTVAL (xval), HImode);

          if (IN_RANGE (val16, 0, 63)
              && (val8 == 0
                  || reg_unused_after (insn, xreg)))
            {
              if (AVR_TINY)
                avr_asm_len (TINY_SBIW (%A0, %B0, %1), xop, plen, 2);
              else
                avr_asm_len ("sbiw %0,%1", xop, plen, 1);

              i++;
              continue;
            }

          if (n_bytes == 2
              && IN_RANGE (val16, -63, -1)
              && compare_eq_p (insn)
              && reg_unused_after (insn, xreg))
            {
              return AVR_TINY
                ? avr_asm_len (TINY_ADIW (%A0, %B0, %n1), xop, plen, 2)
                : avr_asm_len ("adiw %0,%n1", xop, plen, 1);
            }
        }

      /* Comparing against 0 is easy.  */

      if (val8 == 0)
        {
          avr_asm_len (i == 0
                       ? "cp %0,__zero_reg__"
                       : "cpc %0,__zero_reg__", xop, plen, 1);
          continue;
        }

      /* Upper registers can compare and subtract-with-carry immediates.
         Notice that compare instructions do the same as respective subtract
         instruction; the only difference is that comparisons don't write
         the result back to the target register.  */

      if (ld_reg_p)
        {
          if (i == 0)
            {
              avr_asm_len ("cpi %0,%1", xop, plen, 1);
              continue;
            }
          else if (reg_unused_after (insn, xreg))
            {
              avr_asm_len ("sbci %0,%1", xop, plen, 1);
              continue;
            }
        }

      /* Must load the value into the scratch register.  */

      gcc_assert (REG_P (xop[2]));

      if (clobber_val != (int) val8)
        avr_asm_len ("ldi %2,%1", xop, plen, 1);
      clobber_val = (int) val8;

      avr_asm_len (i == 0
                   ? "cp %0,%2"
                   : "cpc %0,%2", xop, plen, 1);
    }

  return "";
}


/* Prepare operands of compare_const_di2 to be used with avr_out_compare.  */

const char*
avr_out_compare64 (rtx_insn *insn, rtx *op, int *plen)
{
  rtx xop[3];

  xop[0] = gen_rtx_REG (DImode, 18);
  xop[1] = op[0];
  xop[2] = op[1];

  return avr_out_compare (insn, xop, plen);
}

/* Output test instruction for HImode.  */

const char*
avr_out_tsthi (rtx_insn *insn, rtx *op, int *plen)
{
  if (compare_sign_p (insn))
    {
      avr_asm_len ("tst %B0", op, plen, -1);
    }
  else if (reg_unused_after (insn, op[0])
           && compare_eq_p (insn))
    {
      /* Faster than sbiw if we can clobber the operand.  */
      avr_asm_len ("or %A0,%B0", op, plen, -1);
    }
  else
    {
      avr_out_compare (insn, op, plen);
    }

  return "";
}


/* Output test instruction for PSImode.  */

const char*
avr_out_tstpsi (rtx_insn *insn, rtx *op, int *plen)
{
  if (compare_sign_p (insn))
    {
      avr_asm_len ("tst %C0", op, plen, -1);
    }
  else if (reg_unused_after (insn, op[0])
           && compare_eq_p (insn))
    {
      /* Faster than sbiw if we can clobber the operand.  */
      avr_asm_len ("or %A0,%B0" CR_TAB
                   "or %A0,%C0", op, plen, -2);
    }
  else
    {
      avr_out_compare (insn, op, plen);
    }

  return "";
}


/* Output test instruction for SImode.  */

const char*
avr_out_tstsi (rtx_insn *insn, rtx *op, int *plen)
{
  if (compare_sign_p (insn))
    {
      avr_asm_len ("tst %D0", op, plen, -1);
    }
  else if (reg_unused_after (insn, op[0])
           && compare_eq_p (insn))
    {
      /* Faster than sbiw if we can clobber the operand.  */
      avr_asm_len ("or %A0,%B0" CR_TAB
                   "or %A0,%C0" CR_TAB
                   "or %A0,%D0", op, plen, -3);
    }
  else
    {
      avr_out_compare (insn, op, plen);
    }

  return "";
}


/* Generate asm equivalent for various shifts.  This only handles cases
   that are not already carefully hand-optimized in ?sh??i3_out.

   OPERANDS[0] resp. %0 in TEMPL is the operand to be shifted.
   OPERANDS[2] is the shift count as CONST_INT, MEM or REG.
   OPERANDS[3] is a QImode scratch register from LD regs if
               available and SCRATCH, otherwise (no scratch available)

   TEMPL is an assembler template that shifts by one position.
   T_LEN is the length of this template.  */

void
out_shift_with_cnt (const char *templ, rtx_insn *insn, rtx operands[],
		    int *plen, int t_len)
{
  bool second_label = true;
  bool saved_in_tmp = false;
  bool use_zero_reg = false;
  rtx op[5];

  op[0] = operands[0];
  op[1] = operands[1];
  op[2] = operands[2];
  op[3] = operands[3];

  if (plen)
    *plen = 0;

  if (CONST_INT_P (operands[2]))
    {
      bool scratch = (GET_CODE (PATTERN (insn)) == PARALLEL
                      && REG_P (operands[3]));
      int count = INTVAL (operands[2]);
      int max_len = 10;  /* If larger than this, always use a loop.  */

      if (count <= 0)
        return;

      if (count < 8 && !scratch)
        use_zero_reg = true;

      if (optimize_size)
        max_len = t_len + (scratch ? 3 : (use_zero_reg ? 4 : 5));

      if (t_len * count <= max_len)
        {
          /* Output shifts inline with no loop - faster.  */

          while (count-- > 0)
            avr_asm_len (templ, op, plen, t_len);

          return;
        }

      if (scratch)
        {
          avr_asm_len ("ldi %3,%2", op, plen, 1);
        }
      else if (use_zero_reg)
        {
          /* Hack to save one word: use __zero_reg__ as loop counter.
             Set one bit, then shift in a loop until it is 0 again.  */

          op[3] = zero_reg_rtx;

          avr_asm_len ("set" CR_TAB
                       "bld %3,%2-1", op, plen, 2);
        }
      else
        {
          /* No scratch register available, use one from LD_REGS (saved in
             __tmp_reg__) that doesn't overlap with registers to shift.  */

          op[3] = all_regs_rtx[((REGNO (op[0]) - 1) & 15) + 16];
          op[4] = tmp_reg_rtx;
          saved_in_tmp = true;

          avr_asm_len ("mov %4,%3" CR_TAB
                       "ldi %3,%2", op, plen, 2);
        }

      second_label = false;
    }
  else if (MEM_P (op[2]))
    {
      rtx op_mov[2];

      op_mov[0] = op[3] = tmp_reg_rtx;
      op_mov[1] = op[2];

      out_movqi_r_mr (insn, op_mov, plen);
    }
  else if (register_operand (op[2], QImode))
    {
      op[3] = op[2];

      if (!reg_unused_after (insn, op[2])
          || reg_overlap_mentioned_p (op[0], op[2]))
        {
          op[3] = tmp_reg_rtx;
          avr_asm_len ("mov %3,%2", op, plen, 1);
        }
    }
  else
    fatal_insn ("bad shift insn:", insn);

  if (second_label)
    avr_asm_len ("rjmp 2f", op, plen, 1);

  avr_asm_len ("1:", op, plen, 0);
  avr_asm_len (templ, op, plen, t_len);

  if (second_label)
    avr_asm_len ("2:", op, plen, 0);

  avr_asm_len (use_zero_reg ? "lsr %3" : "dec %3", op, plen, 1);
  avr_asm_len (second_label ? "brpl 1b" : "brne 1b", op, plen, 1);

  if (saved_in_tmp)
    avr_asm_len ("mov %3,%4", op, plen, 1);
}


/* 8bit shift left ((char)x << i)   */

const char *
ashlqi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 8)
	    break;

	  *len = 1;
	  return "clr %0";

	case 1:
	  *len = 1;
	  return "lsl %0";

	case 2:
	  *len = 2;
	  return ("lsl %0" CR_TAB
		  "lsl %0");

	case 3:
	  *len = 3;
	  return ("lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0");

	case 4:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 2;
	      return ("swap %0" CR_TAB
		      "andi %0,0xf0");
	    }
	  *len = 4;
	  return ("lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0");

	case 5:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 3;
	      return ("swap %0" CR_TAB
		      "lsl %0"  CR_TAB
		      "andi %0,0xe0");
	    }
	  *len = 5;
	  return ("lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0");

	case 6:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 4;
	      return ("swap %0" CR_TAB
		      "lsl %0"  CR_TAB
		      "lsl %0"  CR_TAB
		      "andi %0,0xc0");
	    }
	  *len = 6;
	  return ("lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0");

	case 7:
	  *len = 3;
	  return ("ror %0" CR_TAB
		  "clr %0" CR_TAB
		  "ror %0");
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt ("lsl %0",
                      insn, operands, len, 1);
  return "";
}


/* 16bit shift left ((short)x << i)   */

const char *
ashlhi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL);
      int ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 16)
	    break;

	  *len = 2;
	  return ("clr %B0" CR_TAB
		  "clr %A0");

	case 4:
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    {
	      *len = 6;
	      return ("swap %A0"      CR_TAB
		      "swap %B0"      CR_TAB
		      "andi %B0,0xf0" CR_TAB
		      "eor %B0,%A0"   CR_TAB
		      "andi %A0,0xf0" CR_TAB
		      "eor %B0,%A0");
	    }
	  if (scratch)
	    {
	      *len = 7;
	      return ("swap %A0"    CR_TAB
		      "swap %B0"    CR_TAB
		      "ldi %3,0xf0" CR_TAB
		      "and %B0,%3"  CR_TAB
		      "eor %B0,%A0" CR_TAB
		      "and %A0,%3"  CR_TAB
		      "eor %B0,%A0");
	    }
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    {
	      *len = 8;
	      return ("lsl %A0"       CR_TAB
		      "rol %B0"       CR_TAB
		      "swap %A0"      CR_TAB
		      "swap %B0"      CR_TAB
		      "andi %B0,0xf0" CR_TAB
		      "eor %B0,%A0"   CR_TAB
		      "andi %A0,0xf0" CR_TAB
		      "eor %B0,%A0");
	    }
	  if (scratch)
	    {
	      *len = 9;
	      return ("lsl %A0"     CR_TAB
		      "rol %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "swap %B0"    CR_TAB
		      "ldi %3,0xf0" CR_TAB
		      "and %B0,%3"  CR_TAB
		      "eor %B0,%A0" CR_TAB
		      "and %A0,%3"  CR_TAB
		      "eor %B0,%A0");
	    }
	  break;  /* 10 */

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 9;
	  return ("clr __tmp_reg__" CR_TAB
		  "lsr %B0"         CR_TAB
		  "ror %A0"         CR_TAB
		  "ror __tmp_reg__" CR_TAB
		  "lsr %B0"         CR_TAB
		  "ror %A0"         CR_TAB
		  "ror __tmp_reg__" CR_TAB
		  "mov %B0,%A0"     CR_TAB
		  "mov %A0,__tmp_reg__");

	case 7:
	  *len = 5;
	  return ("lsr %B0"     CR_TAB
		  "mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "ror %B0"     CR_TAB
		  "ror %A0");

	case 8:
	  return *len = 2, ("mov %B0,%A1" CR_TAB
			    "clr %A0");

	case 9:
	  *len = 3;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0");

	case 10:
	  *len = 4;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0");

	case 11:
	  *len = 5;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0");

	case 12:
	  if (ldi_ok)
	    {
	      *len = 4;
	      return ("mov %B0,%A0" CR_TAB
		      "clr %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "andi %B0,0xf0");
	    }
	  if (scratch)
	    {
	      *len = 5;
	      return ("mov %B0,%A0" CR_TAB
		      "clr %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "ldi %3,0xf0" CR_TAB
		      "and %B0,%3");
	    }
	  *len = 6;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0");

	case 13:
	  if (ldi_ok)
	    {
	      *len = 5;
	      return ("mov %B0,%A0" CR_TAB
		      "clr %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "lsl %B0"     CR_TAB
		      "andi %B0,0xe0");
	    }
	  if (AVR_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return ("ldi %3,0x20" CR_TAB
		      "mul %A0,%3"  CR_TAB
		      "mov %B0,r0"  CR_TAB
		      "clr %A0"     CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (scratch)
	    {
	      *len = 6;
	      return ("mov %B0,%A0" CR_TAB
		      "clr %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "lsl %B0"     CR_TAB
		      "ldi %3,0xe0" CR_TAB
		      "and %B0,%3");
	    }
	  if (AVR_HAVE_MUL)
	    {
	      *len = 6;
	      return ("set"        CR_TAB
		      "bld r1,5"   CR_TAB
		      "mul %A0,r1" CR_TAB
		      "mov %B0,r0" CR_TAB
		      "clr %A0"    CR_TAB
		      "clr __zero_reg__");
	    }
	  *len = 7;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0");

	case 14:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %B0,0x40" CR_TAB
		      "mul %A0,%B0"  CR_TAB
		      "mov %B0,r0"   CR_TAB
		      "clr %A0"      CR_TAB
		      "clr __zero_reg__");
	    }
	  if (AVR_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return ("ldi %3,0x40" CR_TAB
		      "mul %A0,%3"  CR_TAB
		      "mov %B0,r0"  CR_TAB
		      "clr %A0"     CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && ldi_ok)
	    {
	      *len = 5;
	      return ("mov %B0,%A0" CR_TAB
		      "ldi %A0,6" "\n1:\t"
		      "lsl %B0"     CR_TAB
		      "dec %A0"     CR_TAB
		      "brne 1b");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return ("clr %B0" CR_TAB
		  "lsr %A0" CR_TAB
		  "ror %B0" CR_TAB
		  "lsr %A0" CR_TAB
		  "ror %B0" CR_TAB
		  "clr %A0");

	case 15:
	  *len = 4;
	  return ("clr %B0" CR_TAB
		  "lsr %A0" CR_TAB
		  "ror %B0" CR_TAB
		  "clr %A0");
	}
      len = t;
    }
  out_shift_with_cnt ("lsl %A0" CR_TAB
                      "rol %B0", insn, operands, len, 2);
  return "";
}


/* 24-bit shift left */

const char*
avr_out_ashlpsi3 (rtx_insn *insn, rtx *op, int *plen)
{
  if (plen)
    *plen = 0;

  if (CONST_INT_P (op[2]))
    {
      switch (INTVAL (op[2]))
        {
        default:
          if (INTVAL (op[2]) < 24)
            break;

          return avr_asm_len ("clr %A0" CR_TAB
                              "clr %B0" CR_TAB
                              "clr %C0", op, plen, 3);

        case 8:
          {
            int reg0 = REGNO (op[0]);
            int reg1 = REGNO (op[1]);

            if (reg0 >= reg1)
              return avr_asm_len ("mov %C0,%B1"  CR_TAB
                                  "mov %B0,%A1"  CR_TAB
                                  "clr %A0", op, plen, 3);
            else
              return avr_asm_len ("clr %A0"      CR_TAB
                                  "mov %B0,%A1"  CR_TAB
                                  "mov %C0,%B1", op, plen, 3);
          }

        case 16:
          {
            int reg0 = REGNO (op[0]);
            int reg1 = REGNO (op[1]);

            if (reg0 + 2 != reg1)
              avr_asm_len ("mov %C0,%A0", op, plen, 1);

            return avr_asm_len ("clr %B0"  CR_TAB
                                "clr %A0", op, plen, 2);
          }

        case 23:
          return avr_asm_len ("clr %C0" CR_TAB
                              "lsr %A0" CR_TAB
                              "ror %C0" CR_TAB
                              "clr %B0" CR_TAB
                              "clr %A0", op, plen, 5);
        }
    }

  out_shift_with_cnt ("lsl %A0" CR_TAB
                      "rol %B0" CR_TAB
                      "rol %C0", insn, op, plen, 3);
  return "";
}


/* 32bit shift left ((long)x << i)   */

const char *
ashlsi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 32)
	    break;

	  if (AVR_HAVE_MOVW)
	    return *len = 3, ("clr %D0" CR_TAB
			      "clr %C0" CR_TAB
			      "movw %A0,%C0");
	  *len = 4;
	  return ("clr %D0" CR_TAB
		  "clr %C0" CR_TAB
		  "clr %B0" CR_TAB
		  "clr %A0");

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (reg0 >= reg1)
	      return ("mov %D0,%C1"  CR_TAB
		      "mov %C0,%B1"  CR_TAB
		      "mov %B0,%A1"  CR_TAB
		      "clr %A0");
	    else
	      return ("clr %A0"      CR_TAB
		      "mov %B0,%A1"  CR_TAB
		      "mov %C0,%B1"  CR_TAB
		      "mov %D0,%C1");
	  }

	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    if (reg0 + 2 == reg1)
	      return *len = 2, ("clr %B0"      CR_TAB
				"clr %A0");
	    if (AVR_HAVE_MOVW)
	      return *len = 3, ("movw %C0,%A1" CR_TAB
				"clr %B0"      CR_TAB
				"clr %A0");
	    else
	      return *len = 4, ("mov %C0,%A1"  CR_TAB
				"mov %D0,%B1"  CR_TAB
				"clr %B0"      CR_TAB
				"clr %A0");
	  }

	case 24:
	  *len = 4;
	  return ("mov %D0,%A1"  CR_TAB
		  "clr %C0"      CR_TAB
		  "clr %B0"      CR_TAB
		  "clr %A0");

	case 31:
	  *len = 6;
	  return ("clr %D0" CR_TAB
		  "lsr %A0" CR_TAB
		  "ror %D0" CR_TAB
		  "clr %C0" CR_TAB
		  "clr %B0" CR_TAB
		  "clr %A0");
	}
      len = t;
    }
  out_shift_with_cnt ("lsl %A0" CR_TAB
                      "rol %B0" CR_TAB
                      "rol %C0" CR_TAB
                      "rol %D0", insn, operands, len, 4);
  return "";
}

/* 8bit arithmetic shift right  ((signed char)x >> i) */

const char *
ashrqi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	case 1:
	  *len = 1;
	  return "asr %0";

	case 2:
	  *len = 2;
	  return ("asr %0" CR_TAB
		  "asr %0");

	case 3:
	  *len = 3;
	  return ("asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0");

	case 4:
	  *len = 4;
	  return ("asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0");

	case 5:
	  *len = 5;
	  return ("asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0");

	case 6:
	  *len = 4;
	  return ("bst %0,6"  CR_TAB
		  "lsl %0"    CR_TAB
		  "sbc %0,%0" CR_TAB
		  "bld %0,0");

	default:
	  if (INTVAL (operands[2]) < 8)
	    break;

	  /* fall through */

	case 7:
	  *len = 2;
	  return ("lsl %0" CR_TAB
		  "sbc %0,%0");
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt ("asr %0",
                      insn, operands, len, 1);
  return "";
}


/* 16bit arithmetic shift right  ((signed short)x >> i) */

const char *
ashrhi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL);
      int ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	case 4:
	case 5:
	  /* XXX try to optimize this too? */
	  break;

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 8;
	  return ("mov __tmp_reg__,%A0" CR_TAB
		  "mov %A0,%B0"         CR_TAB
		  "lsl __tmp_reg__"     CR_TAB
		  "rol %A0"             CR_TAB
		  "sbc %B0,%B0"         CR_TAB
		  "lsl __tmp_reg__"     CR_TAB
		  "rol %A0"             CR_TAB
		  "rol %B0");

	case 7:
	  *len = 4;
	  return ("lsl %A0"     CR_TAB
		  "mov %A0,%B0" CR_TAB
		  "rol %A0"     CR_TAB
		  "sbc %B0,%B0");

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    if (reg0 == reg1)
	      return *len = 3, ("mov %A0,%B0" CR_TAB
				"lsl %B0"     CR_TAB
				"sbc %B0,%B0");
	    else
	      return *len = 4, ("mov %A0,%B1" CR_TAB
			        "clr %B0"     CR_TAB
			        "sbrc %A0,7"  CR_TAB
			        "dec %B0");
	  }

	case 9:
	  *len = 4;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"      CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0");

	case 10:
	  *len = 5;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0");

	case 11:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %A0,0x20" CR_TAB
		      "muls %B0,%A0" CR_TAB
		      "mov %A0,r1"   CR_TAB
		      "sbc %B0,%B0"  CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0");

	case 12:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %A0,0x10" CR_TAB
		      "muls %B0,%A0" CR_TAB
		      "mov %A0,r1"   CR_TAB
		      "sbc %B0,%B0"  CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 7;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0");

	case 13:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %A0,0x08" CR_TAB
		      "muls %B0,%A0" CR_TAB
		      "mov %A0,r1"   CR_TAB
		      "sbc %B0,%B0"  CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size)
	    break;  /* scratch ? 5 : 7 */
	  *len = 8;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0");

	case 14:
	  *len = 5;
	  return ("lsl %B0"     CR_TAB
		  "sbc %A0,%A0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "mov %B0,%A0" CR_TAB
		  "rol %A0");

	default:
	  if (INTVAL (operands[2]) < 16)
	    break;

	  /* fall through */

	case 15:
	  return *len = 3, ("lsl %B0"     CR_TAB
			    "sbc %A0,%A0" CR_TAB
			    "mov %B0,%A0");
	}
      len = t;
    }
  out_shift_with_cnt ("asr %B0" CR_TAB
                      "ror %A0", insn, operands, len, 2);
  return "";
}


/* 24-bit arithmetic shift right */

const char*
avr_out_ashrpsi3 (rtx_insn *insn, rtx *op, int *plen)
{
  int dest = REGNO (op[0]);
  int src = REGNO (op[1]);

  if (CONST_INT_P (op[2]))
    {
      if (plen)
        *plen = 0;

      switch (INTVAL (op[2]))
        {
        case 8:
          if (dest <= src)
            return avr_asm_len ("mov %A0,%B1" CR_TAB
                                "mov %B0,%C1" CR_TAB
                                "clr %C0"     CR_TAB
                                "sbrc %B0,7"  CR_TAB
                                "dec %C0", op, plen, 5);
          else
            return avr_asm_len ("clr %C0"     CR_TAB
                                "sbrc %C1,7"  CR_TAB
                                "dec %C0"     CR_TAB
                                "mov %B0,%C1" CR_TAB
                                "mov %A0,%B1", op, plen, 5);

        case 16:
          if (dest != src + 2)
            avr_asm_len ("mov %A0,%C1", op, plen, 1);

          return avr_asm_len ("clr %B0"     CR_TAB
                              "sbrc %A0,7"  CR_TAB
                              "com %B0"     CR_TAB
                              "mov %C0,%B0", op, plen, 4);

        default:
          if (INTVAL (op[2]) < 24)
            break;

          /* fall through */

        case 23:
          return avr_asm_len ("lsl %C0"     CR_TAB
                              "sbc %A0,%A0" CR_TAB
                              "mov %B0,%A0" CR_TAB
                              "mov %C0,%A0", op, plen, 4);
        } /* switch */
    }

  out_shift_with_cnt ("asr %C0" CR_TAB
                      "ror %B0" CR_TAB
                      "ror %A0", insn, op, plen, 3);
  return "";
}


/* 32-bit arithmetic shift right  ((signed long)x >> i) */

const char *
ashrsi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=6;
	    if (reg0 <= reg1)
	      return ("mov %A0,%B1" CR_TAB
		      "mov %B0,%C1" CR_TAB
		      "mov %C0,%D1" CR_TAB
		      "clr %D0"     CR_TAB
		      "sbrc %C0,7"  CR_TAB
		      "dec %D0");
	    else
	      return ("clr %D0"     CR_TAB
		      "sbrc %D1,7"  CR_TAB
		      "dec %D0"     CR_TAB
		      "mov %C0,%D1" CR_TAB
		      "mov %B0,%C1" CR_TAB
		      "mov %A0,%B1");
	  }

	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    if (reg0 == reg1 + 2)
	      return *len = 4, ("clr %D0"     CR_TAB
				"sbrc %B0,7"  CR_TAB
				"com %D0"     CR_TAB
				"mov %C0,%D0");
	    if (AVR_HAVE_MOVW)
	      return *len = 5, ("movw %A0,%C1" CR_TAB
				"clr %D0"      CR_TAB
				"sbrc %B0,7"   CR_TAB
				"com %D0"      CR_TAB
				"mov %C0,%D0");
	    else
	      return *len = 6, ("mov %B0,%D1" CR_TAB
				"mov %A0,%C1" CR_TAB
				"clr %D0"     CR_TAB
				"sbrc %B0,7"  CR_TAB
				"com %D0"     CR_TAB
				"mov %C0,%D0");
	  }

	case 24:
	  return *len = 6, ("mov %A0,%D1" CR_TAB
			    "clr %D0"     CR_TAB
			    "sbrc %A0,7"  CR_TAB
			    "com %D0"     CR_TAB
			    "mov %B0,%D0" CR_TAB
			    "mov %C0,%D0");

	default:
	  if (INTVAL (operands[2]) < 32)
	    break;

	  /* fall through */

	case 31:
	  if (AVR_HAVE_MOVW)
	    return *len = 4, ("lsl %D0"     CR_TAB
			      "sbc %A0,%A0" CR_TAB
			      "mov %B0,%A0" CR_TAB
			      "movw %C0,%A0");
	  else
	    return *len = 5, ("lsl %D0"     CR_TAB
			      "sbc %A0,%A0" CR_TAB
			      "mov %B0,%A0" CR_TAB
			      "mov %C0,%A0" CR_TAB
			      "mov %D0,%A0");
	}
      len = t;
    }
  out_shift_with_cnt ("asr %D0" CR_TAB
                      "ror %C0" CR_TAB
                      "ror %B0" CR_TAB
                      "ror %A0", insn, operands, len, 4);
  return "";
}

/* 8-bit logic shift right ((unsigned char)x >> i) */

const char *
lshrqi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 8)
	    break;

	  *len = 1;
	  return "clr %0";

	case 1:
	  *len = 1;
	  return "lsr %0";

	case 2:
	  *len = 2;
	  return ("lsr %0" CR_TAB
		  "lsr %0");
	case 3:
	  *len = 3;
	  return ("lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0");

	case 4:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len=2;
	      return ("swap %0" CR_TAB
		      "andi %0,0x0f");
	    }
	  *len = 4;
	  return ("lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0");

	case 5:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 3;
	      return ("swap %0" CR_TAB
		      "lsr %0"  CR_TAB
		      "andi %0,0x7");
	    }
	  *len = 5;
	  return ("lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0");

	case 6:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 4;
	      return ("swap %0" CR_TAB
		      "lsr %0"  CR_TAB
		      "lsr %0"  CR_TAB
		      "andi %0,0x3");
	    }
	  *len = 6;
	  return ("lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0");

	case 7:
	  *len = 3;
	  return ("rol %0" CR_TAB
		  "clr %0" CR_TAB
		  "rol %0");
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt ("lsr %0",
                      insn, operands, len, 1);
  return "";
}

/* 16-bit logic shift right ((unsigned short)x >> i) */

const char *
lshrhi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL);
      int ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 16)
	    break;

	  *len = 2;
	  return ("clr %B0" CR_TAB
		  "clr %A0");

	case 4:
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    {
	      *len = 6;
	      return ("swap %B0"      CR_TAB
		      "swap %A0"      CR_TAB
		      "andi %A0,0x0f" CR_TAB
		      "eor %A0,%B0"   CR_TAB
		      "andi %B0,0x0f" CR_TAB
		      "eor %A0,%B0");
	    }
	  if (scratch)
	    {
	      *len = 7;
	      return ("swap %B0"    CR_TAB
		      "swap %A0"    CR_TAB
		      "ldi %3,0x0f" CR_TAB
		      "and %A0,%3"  CR_TAB
		      "eor %A0,%B0" CR_TAB
		      "and %B0,%3"  CR_TAB
		      "eor %A0,%B0");
	    }
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    {
	      *len = 8;
	      return ("lsr %B0"       CR_TAB
		      "ror %A0"       CR_TAB
		      "swap %B0"      CR_TAB
		      "swap %A0"      CR_TAB
		      "andi %A0,0x0f" CR_TAB
		      "eor %A0,%B0"   CR_TAB
		      "andi %B0,0x0f" CR_TAB
		      "eor %A0,%B0");
	    }
	  if (scratch)
	    {
	      *len = 9;
	      return ("lsr %B0"     CR_TAB
		      "ror %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "swap %A0"    CR_TAB
		      "ldi %3,0x0f" CR_TAB
		      "and %A0,%3"  CR_TAB
		      "eor %A0,%B0" CR_TAB
		      "and %B0,%3"  CR_TAB
		      "eor %A0,%B0");
	    }
	  break;  /* 10 */

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 9;
	  return ("clr __tmp_reg__" CR_TAB
		  "lsl %A0"         CR_TAB
		  "rol %B0"         CR_TAB
		  "rol __tmp_reg__" CR_TAB
		  "lsl %A0"         CR_TAB
		  "rol %B0"         CR_TAB
		  "rol __tmp_reg__" CR_TAB
		  "mov %A0,%B0"     CR_TAB
		  "mov %B0,__tmp_reg__");

	case 7:
	  *len = 5;
	  return ("lsl %A0"     CR_TAB
		  "mov %A0,%B0" CR_TAB
		  "rol %A0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "neg %B0");

	case 8:
	  return *len = 2, ("mov %A0,%B1" CR_TAB
			    "clr %B0");

	case 9:
	  *len = 3;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0");

	case 10:
	  *len = 4;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0");

	case 11:
	  *len = 5;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0");

	case 12:
	  if (ldi_ok)
	    {
	      *len = 4;
	      return ("mov %A0,%B0" CR_TAB
		      "clr %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "andi %A0,0x0f");
	    }
	  if (scratch)
	    {
	      *len = 5;
	      return ("mov %A0,%B0" CR_TAB
		      "clr %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "ldi %3,0x0f" CR_TAB
		      "and %A0,%3");
	    }
	  *len = 6;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0");

	case 13:
	  if (ldi_ok)
	    {
	      *len = 5;
	      return ("mov %A0,%B0" CR_TAB
		      "clr %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "lsr %A0"     CR_TAB
		      "andi %A0,0x07");
	    }
	  if (AVR_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return ("ldi %3,0x08" CR_TAB
		      "mul %B0,%3"  CR_TAB
		      "mov %A0,r1"  CR_TAB
		      "clr %B0"     CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (scratch)
	    {
	      *len = 6;
	      return ("mov %A0,%B0" CR_TAB
		      "clr %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "lsr %A0"     CR_TAB
		      "ldi %3,0x07" CR_TAB
		      "and %A0,%3");
	    }
	  if (AVR_HAVE_MUL)
	    {
	      *len = 6;
	      return ("set"        CR_TAB
		      "bld r1,3"   CR_TAB
		      "mul %B0,r1" CR_TAB
		      "mov %A0,r1" CR_TAB
		      "clr %B0"    CR_TAB
		      "clr __zero_reg__");
	    }
	  *len = 7;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0");

	case 14:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %A0,0x04" CR_TAB
		      "mul %B0,%A0"  CR_TAB
		      "mov %A0,r1"   CR_TAB
		      "clr %B0"      CR_TAB
		      "clr __zero_reg__");
	    }
	  if (AVR_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return ("ldi %3,0x04" CR_TAB
		      "mul %B0,%3"  CR_TAB
		      "mov %A0,r1"  CR_TAB
		      "clr %B0"     CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && ldi_ok)
	    {
	      *len = 5;
	      return ("mov %A0,%B0" CR_TAB
		      "ldi %B0,6" "\n1:\t"
		      "lsr %A0"     CR_TAB
		      "dec %B0"     CR_TAB
		      "brne 1b");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return ("clr %A0" CR_TAB
		  "lsl %B0" CR_TAB
		  "rol %A0" CR_TAB
		  "lsl %B0" CR_TAB
		  "rol %A0" CR_TAB
		  "clr %B0");

	case 15:
	  *len = 4;
	  return ("clr %A0" CR_TAB
		  "lsl %B0" CR_TAB
		  "rol %A0" CR_TAB
		  "clr %B0");
	}
      len = t;
    }
  out_shift_with_cnt ("lsr %B0" CR_TAB
                      "ror %A0", insn, operands, len, 2);
  return "";
}


/* 24-bit logic shift right */

const char*
avr_out_lshrpsi3 (rtx_insn *insn, rtx *op, int *plen)
{
  int dest = REGNO (op[0]);
  int src = REGNO (op[1]);

  if (CONST_INT_P (op[2]))
    {
      if (plen)
        *plen = 0;

      switch (INTVAL (op[2]))
        {
        case 8:
          if (dest <= src)
            return avr_asm_len ("mov %A0,%B1" CR_TAB
                                "mov %B0,%C1" CR_TAB
                                "clr %C0", op, plen, 3);
          else
            return avr_asm_len ("clr %C0"     CR_TAB
                                "mov %B0,%C1" CR_TAB
                                "mov %A0,%B1", op, plen, 3);

        case 16:
          if (dest != src + 2)
            avr_asm_len ("mov %A0,%C1", op, plen, 1);

          return avr_asm_len ("clr %B0"  CR_TAB
                              "clr %C0", op, plen, 2);

        default:
          if (INTVAL (op[2]) < 24)
            break;

          /* fall through */

        case 23:
          return avr_asm_len ("clr %A0"    CR_TAB
                              "sbrc %C0,7" CR_TAB
                              "inc %A0"    CR_TAB
                              "clr %B0"    CR_TAB
                              "clr %C0", op, plen, 5);
        } /* switch */
    }

  out_shift_with_cnt ("lsr %C0" CR_TAB
                      "ror %B0" CR_TAB
                      "ror %A0", insn, op, plen, 3);
  return "";
}


/* 32-bit logic shift right ((unsigned int)x >> i) */

const char *
lshrsi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 32)
	    break;

	  if (AVR_HAVE_MOVW)
	    return *len = 3, ("clr %D0" CR_TAB
			      "clr %C0" CR_TAB
			      "movw %A0,%C0");
	  *len = 4;
	  return ("clr %D0" CR_TAB
		  "clr %C0" CR_TAB
		  "clr %B0" CR_TAB
		  "clr %A0");

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (reg0 <= reg1)
	      return ("mov %A0,%B1" CR_TAB
		      "mov %B0,%C1" CR_TAB
		      "mov %C0,%D1" CR_TAB
		      "clr %D0");
	    else
	      return ("clr %D0"     CR_TAB
		      "mov %C0,%D1" CR_TAB
		      "mov %B0,%C1" CR_TAB
		      "mov %A0,%B1");
	  }

	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    if (reg0 == reg1 + 2)
	      return *len = 2, ("clr %C0"     CR_TAB
				"clr %D0");
	    if (AVR_HAVE_MOVW)
	      return *len = 3, ("movw %A0,%C1" CR_TAB
				"clr %C0"      CR_TAB
				"clr %D0");
	    else
	      return *len = 4, ("mov %B0,%D1" CR_TAB
				"mov %A0,%C1" CR_TAB
				"clr %C0"     CR_TAB
				"clr %D0");
	  }

	case 24:
	  return *len = 4, ("mov %A0,%D1" CR_TAB
			    "clr %B0"     CR_TAB
			    "clr %C0"     CR_TAB
			    "clr %D0");

	case 31:
	  *len = 6;
	  return ("clr %A0"    CR_TAB
		  "sbrc %D0,7" CR_TAB
		  "inc %A0"    CR_TAB
		  "clr %B0"    CR_TAB
		  "clr %C0"    CR_TAB
		  "clr %D0");
	}
      len = t;
    }
  out_shift_with_cnt ("lsr %D0" CR_TAB
                      "ror %C0" CR_TAB
                      "ror %B0" CR_TAB
                      "ror %A0", insn, operands, len, 4);
  return "";
}


/* Output addition of register XOP[0] and compile time constant XOP[2].
   CODE == PLUS:  perform addition by using ADD instructions or
   CODE == MINUS: perform addition by using SUB instructions:

      XOP[0] = XOP[0] + XOP[2]

   Or perform addition/subtraction with register XOP[2] depending on CODE:

      XOP[0] = XOP[0] +/- XOP[2]

   If PLEN == NULL, print assembler instructions to perform the operation;
   otherwise, set *PLEN to the length of the instruction sequence (in words)
   printed with PLEN == NULL.  XOP[3] is an 8-bit scratch register or NULL_RTX.
   Set *PCC to effect on cc0 according to respective CC_* insn attribute.

   CODE_SAT == UNKNOWN: Perform ordinary, non-saturating operation.
   CODE_SAT != UNKNOWN: Perform operation and saturate according to CODE_SAT.
   If  CODE_SAT != UNKNOWN  then SIGN contains the sign of the summand resp.
   the subtrahend in the original insn, provided it is a compile time constant.
   In all other cases, SIGN is 0.

   If OUT_LABEL is true, print the final 0: label which is needed for
   saturated addition / subtraction.  The only case where OUT_LABEL = false
   is useful is for saturated addition / subtraction performed during
   fixed-point rounding, cf. `avr_out_round'.  */

static void
avr_out_plus_1 (rtx *xop, int *plen, enum rtx_code code, int *pcc,
                enum rtx_code code_sat, int sign, bool out_label)
{
  /* MODE of the operation.  */
  machine_mode mode = GET_MODE (xop[0]);

  /* INT_MODE of the same size.  */
  scalar_int_mode imode = int_mode_for_mode (mode).require ();

  /* Number of bytes to operate on.  */
  int n_bytes = GET_MODE_SIZE (mode);

  /* Value (0..0xff) held in clobber register op[3] or -1 if unknown.  */
  int clobber_val = -1;

  /* op[0]: 8-bit destination register
     op[1]: 8-bit const int
     op[2]: 8-bit scratch register */
  rtx op[3];

  /* Started the operation?  Before starting the operation we may skip
     adding 0.  This is no more true after the operation started because
     carry must be taken into account.  */
  bool started = false;

  /* Value to add.  There are two ways to add VAL: R += VAL and R -= -VAL.  */
  rtx xval = xop[2];

  /* Output a BRVC instruction.  Only needed with saturation.  */
  bool out_brvc = true;

  if (plen)
    *plen = 0;

  if (REG_P (xop[2]))
    {
      *pcc = MINUS == code ? (int) CC_SET_CZN : (int) CC_CLOBBER;

      for (int i = 0; i < n_bytes; i++)
        {
          /* We operate byte-wise on the destination.  */
          op[0] = simplify_gen_subreg (QImode, xop[0], mode, i);
          op[1] = simplify_gen_subreg (QImode, xop[2], mode, i);

          if (i == 0)
            avr_asm_len (code == PLUS ? "add %0,%1" : "sub %0,%1",
                         op, plen, 1);
          else
            avr_asm_len (code == PLUS ? "adc %0,%1" : "sbc %0,%1",
                         op, plen, 1);
        }

      if (reg_overlap_mentioned_p (xop[0], xop[2]))
        {
          gcc_assert (REGNO (xop[0]) == REGNO (xop[2]));

          if (MINUS == code)
            return;
        }

      goto saturate;
    }

  /* Except in the case of ADIW with 16-bit register (see below)
     addition does not set cc0 in a usable way.  */

  *pcc = (MINUS == code) ? CC_SET_CZN : CC_CLOBBER;

  if (CONST_FIXED_P (xval))
    xval = avr_to_int_mode (xval);

  /* Adding/Subtracting zero is a no-op.  */

  if (xval == const0_rtx)
    {
      *pcc = CC_NONE;
      return;
    }

  if (MINUS == code)
    xval = simplify_unary_operation (NEG, imode, xval, imode);

  op[2] = xop[3];

  if (SS_PLUS == code_sat && MINUS == code
      && sign < 0
      && 0x80 == (INTVAL (simplify_gen_subreg (QImode, xval, imode, n_bytes-1))
                  & GET_MODE_MASK (QImode)))
    {
      /* We compute x + 0x80 by means of SUB instructions.  We negated the
         constant subtrahend above and are left with  x - (-128)  so that we
         need something like SUBI r,128 which does not exist because SUBI sets
         V according to the sign of the subtrahend.  Notice the only case
         where this must be done is when NEG overflowed in case [2s] because
         the V computation needs the right sign of the subtrahend.  */

      rtx msb = simplify_gen_subreg (QImode, xop[0], mode, n_bytes - 1);

      avr_asm_len ("subi %0,128" CR_TAB
                   "brmi 0f", &msb, plen, 2);
      out_brvc = false;

      goto saturate;
    }

  for (int i = 0; i < n_bytes; i++)
    {
      /* We operate byte-wise on the destination.  */
      rtx reg8 = simplify_gen_subreg (QImode, xop[0], mode, i);
      rtx xval8 = simplify_gen_subreg (QImode, xval, imode, i);

      /* 8-bit value to operate with this byte. */
      unsigned int val8 = UINTVAL (xval8) & GET_MODE_MASK (QImode);

      /* Registers R16..R31 can operate with immediate.  */
      bool ld_reg_p = test_hard_reg_class (LD_REGS, reg8);

      op[0] = reg8;
      op[1] = gen_int_mode (val8, QImode);

      /* To get usable cc0 no low-bytes must have been skipped.  */

      if (i && !started)
        *pcc = CC_CLOBBER;

      if (!started
          && i % 2 == 0
          && i + 2 <= n_bytes
          && test_hard_reg_class (ADDW_REGS, reg8))
        {
          rtx xval16 = simplify_gen_subreg (HImode, xval, imode, i);
          unsigned int val16 = UINTVAL (xval16) & GET_MODE_MASK (HImode);

          /* Registers R24, X, Y, Z can use ADIW/SBIW with constants < 64
             i.e. operate word-wise.  */

          if (val16 < 64)
            {
              if (val16 != 0)
                {
                  started = true;
                  avr_asm_len (code == PLUS ? "adiw %0,%1" : "sbiw %0,%1",
                               op, plen, 1);

                  if (n_bytes == 2 && PLUS == code)
                    *pcc = CC_SET_CZN;
                }

              i++;
              continue;
            }
        }

      if (val8 == 0)
        {
          if (started)
            avr_asm_len (code == PLUS
                         ? "adc %0,__zero_reg__" : "sbc %0,__zero_reg__",
                         op, plen, 1);
          continue;
        }
      else if ((val8 == 1 || val8 == 0xff)
               && UNKNOWN == code_sat
               && !started
               && i == n_bytes - 1)
        {
          avr_asm_len ((code == PLUS) ^ (val8 == 1) ? "dec %0" : "inc %0",
                       op, plen, 1);
          *pcc = CC_CLOBBER;
          break;
        }

      switch (code)
        {
        case PLUS:

          gcc_assert (plen != NULL || (op[2] && REG_P (op[2])));

          if (plen != NULL && UNKNOWN != code_sat)
            {
              /* This belongs to the x + 0x80 corner case.  The code with
                 ADD instruction is not smaller, thus make this case
                 expensive so that the caller won't pick it.  */

              *plen += 10;
              break;
            }

          if (clobber_val != (int) val8)
            avr_asm_len ("ldi %2,%1", op, plen, 1);
          clobber_val = (int) val8;

          avr_asm_len (started ? "adc %0,%2" : "add %0,%2", op, plen, 1);

          break; /* PLUS */

        case MINUS:

          if (ld_reg_p)
            avr_asm_len (started ? "sbci %0,%1" : "subi %0,%1", op, plen, 1);
          else
            {
              gcc_assert (plen != NULL || REG_P (op[2]));

              if (clobber_val != (int) val8)
                avr_asm_len ("ldi %2,%1", op, plen, 1);
              clobber_val = (int) val8;

              avr_asm_len (started ? "sbc %0,%2" : "sub %0,%2", op, plen, 1);
            }

          break; /* MINUS */

        default:
          /* Unknown code */
          gcc_unreachable();
        }

      started = true;

    } /* for all sub-bytes */

 saturate:

  if (UNKNOWN == code_sat)
    return;

  *pcc = (int) CC_CLOBBER;

  /* Vanilla addition/subtraction is done.  We are left with saturation.

     We have to compute  A = A <op> B  where  A  is a register and
     B is a register or a non-zero compile time constant CONST.
     A is register class "r" if unsigned && B is REG.  Otherwise, A is in "d".
     B stands for the original operand $2 in INSN.  In the case of B = CONST,
     SIGN in { -1, 1 } is the sign of B.  Otherwise, SIGN is 0.

     CODE is the instruction flavor we use in the asm sequence to perform <op>.


     unsigned
     operation        |  code |  sat if  |    b is      | sat value |  case
     -----------------+-------+----------+--------------+-----------+-------
     +  as  a + b     |  add  |  C == 1  |  const, reg  | u+ = 0xff |  [1u]
     +  as  a - (-b)  |  sub  |  C == 0  |  const       | u+ = 0xff |  [2u]
     -  as  a - b     |  sub  |  C == 1  |  const, reg  | u- = 0    |  [3u]
     -  as  a + (-b)  |  add  |  C == 0  |  const       | u- = 0    |  [4u]


     signed
     operation        |  code |  sat if  |    b is      | sat value |  case
     -----------------+-------+----------+--------------+-----------+-------
     +  as  a + b     |  add  |  V == 1  |  const, reg  | s+        |  [1s]
     +  as  a - (-b)  |  sub  |  V == 1  |  const       | s+        |  [2s]
     -  as  a - b     |  sub  |  V == 1  |  const, reg  | s-        |  [3s]
     -  as  a + (-b)  |  add  |  V == 1  |  const       | s-        |  [4s]

     s+  =  b < 0  ?  -0x80 :  0x7f
     s-  =  b < 0  ?   0x7f : -0x80

     The cases a - b actually perform  a - (-(-b))  if B is CONST.
  */

  op[0] = simplify_gen_subreg (QImode, xop[0], mode, n_bytes-1);
  op[1] = n_bytes > 1
    ? simplify_gen_subreg (QImode, xop[0], mode, n_bytes-2)
    : NULL_RTX;

  bool need_copy = true;
  int len_call = 1 + AVR_HAVE_JMP_CALL;

  switch (code_sat)
    {
    default:
      gcc_unreachable();

    case SS_PLUS:
    case SS_MINUS:

      if (out_brvc)
        avr_asm_len ("brvc 0f", op, plen, 1);

      if (reg_overlap_mentioned_p (xop[0], xop[2]))
        {
          /* [1s,reg] */

          if (n_bytes == 1)
            avr_asm_len ("ldi %0,0x7f" CR_TAB
                         "adc %0,__zero_reg__", op, plen, 2);
          else
            avr_asm_len ("ldi %0,0x7f" CR_TAB
                         "ldi %1,0xff" CR_TAB
                         "adc %1,__zero_reg__" CR_TAB
                         "adc %0,__zero_reg__", op, plen, 4);
        }
      else if (sign == 0 && PLUS == code)
        {
          /* [1s,reg] */

          op[2] = simplify_gen_subreg (QImode, xop[2], mode, n_bytes-1);

          if (n_bytes == 1)
            avr_asm_len ("ldi %0,0x80" CR_TAB
                         "sbrs %2,7"   CR_TAB
                         "dec %0", op, plen, 3);
          else
            avr_asm_len ("ldi %0,0x80" CR_TAB
                         "cp %2,%0"    CR_TAB
                         "sbc %1,%1"   CR_TAB
                         "sbci %0,0", op, plen, 4);
        }
      else if (sign == 0 && MINUS == code)
        {
          /* [3s,reg] */

          op[2] = simplify_gen_subreg (QImode, xop[2], mode, n_bytes-1);

          if (n_bytes == 1)
            avr_asm_len ("ldi %0,0x7f" CR_TAB
                         "sbrs %2,7"   CR_TAB
                         "inc %0", op, plen, 3);
          else
            avr_asm_len ("ldi %0,0x7f" CR_TAB
                         "cp %0,%2"    CR_TAB
                         "sbc %1,%1"   CR_TAB
                         "sbci %0,-1", op, plen, 4);
        }
      else if ((sign < 0) ^ (SS_MINUS == code_sat))
        {
          /* [1s,const,B < 0] [2s,B < 0] */
          /* [3s,const,B > 0] [4s,B > 0] */

          if (n_bytes == 8)
            {
              avr_asm_len ("%~call __clr_8", op, plen, len_call);
              need_copy = false;
            }

          avr_asm_len ("ldi %0,0x80", op, plen, 1);
          if (n_bytes > 1 && need_copy)
            avr_asm_len ("clr %1", op, plen, 1);
        }
      else if ((sign > 0) ^ (SS_MINUS == code_sat))
        {
          /* [1s,const,B > 0] [2s,B > 0] */
          /* [3s,const,B < 0] [4s,B < 0] */

          if (n_bytes == 8)
            {
              avr_asm_len ("sec" CR_TAB
                           "%~call __sbc_8", op, plen, 1 + len_call);
              need_copy = false;
            }

          avr_asm_len ("ldi %0,0x7f", op, plen, 1);
          if (n_bytes > 1 && need_copy)
            avr_asm_len ("ldi %1,0xff", op, plen, 1);
        }
      else
        gcc_unreachable();

      break;

    case US_PLUS:
      /* [1u] : [2u] */

      avr_asm_len (PLUS == code ? "brcc 0f" : "brcs 0f", op, plen, 1);

      if (n_bytes == 8)
        {
          if (MINUS == code)
            avr_asm_len ("sec", op, plen, 1);
          avr_asm_len ("%~call __sbc_8", op, plen, len_call);

          need_copy = false;
        }
      else
        {
          if (MINUS == code && !test_hard_reg_class (LD_REGS, op[0]))
            avr_asm_len ("sec" CR_TAB
                         "sbc %0,%0", op, plen, 2);
          else
            avr_asm_len (PLUS == code ? "sbc %0,%0" : "ldi %0,0xff",
                         op, plen, 1);
        }
      break; /* US_PLUS */

    case US_MINUS:
      /* [4u] : [3u] */

      avr_asm_len (PLUS == code ? "brcs 0f" : "brcc 0f", op, plen, 1);

      if (n_bytes == 8)
        {
          avr_asm_len ("%~call __clr_8", op, plen, len_call);
          need_copy = false;
        }
      else
        avr_asm_len ("clr %0", op, plen, 1);

      break;
    }

  /* We set the MSB in the unsigned case and the 2 MSBs in the signed case.
     Now copy the right value to the LSBs.  */

  if (need_copy && n_bytes > 1)
    {
      if (US_MINUS == code_sat || US_PLUS == code_sat)
        {
          avr_asm_len ("mov %1,%0", op, plen, 1);

          if (n_bytes > 2)
            {
              op[0] = xop[0];
              if (AVR_HAVE_MOVW)
                avr_asm_len ("movw %0,%1", op, plen, 1);
              else
                avr_asm_len ("mov %A0,%1" CR_TAB
                             "mov %B0,%1", op, plen, 2);
            }
        }
      else if (n_bytes > 2)
        {
          op[0] = xop[0];
          avr_asm_len ("mov %A0,%1" CR_TAB
                       "mov %B0,%1", op, plen, 2);
        }
    }

  if (need_copy && n_bytes == 8)
    {
      if (AVR_HAVE_MOVW)
        avr_asm_len ("movw %r0+2,%0" CR_TAB
                     "movw %r0+4,%0", xop, plen, 2);
      else
        avr_asm_len ("mov %r0+2,%0" CR_TAB
                     "mov %r0+3,%0" CR_TAB
                     "mov %r0+4,%0" CR_TAB
                     "mov %r0+5,%0", xop, plen, 4);
    }

  if (out_label)
    avr_asm_len ("0:", op, plen, 0);
}


/* Output addition/subtraction of register XOP[0] and a constant XOP[2] that
   is ont a compile-time constant:

      XOP[0] = XOP[0] +/- XOP[2]

   This is a helper for the function below.  The only insns that need this
   are additions/subtraction for pointer modes, i.e. HImode and PSImode.  */

static const char*
avr_out_plus_symbol (rtx *xop, enum rtx_code code, int *plen, int *pcc)
{
  machine_mode mode = GET_MODE (xop[0]);

  /* Only pointer modes want to add symbols.  */

  gcc_assert (mode == HImode || mode == PSImode);

  *pcc = MINUS == code ? (int) CC_SET_CZN : (int) CC_SET_N;

  avr_asm_len (PLUS == code
               ? "subi %A0,lo8(-(%2))" CR_TAB "sbci %B0,hi8(-(%2))"
               : "subi %A0,lo8(%2)"    CR_TAB "sbci %B0,hi8(%2)",
               xop, plen, -2);

  if (PSImode == mode)
    avr_asm_len (PLUS == code
                 ? "sbci %C0,hlo8(-(%2))"
                 : "sbci %C0,hlo8(%2)", xop, plen, 1);
  return "";
}


/* Prepare operands of addition/subtraction to be used with avr_out_plus_1.

   INSN is a single_set insn or an insn pattern with a binary operation as
   SET_SRC that is one of: PLUS, SS_PLUS, US_PLUS, MINUS, SS_MINUS, US_MINUS.

   XOP are the operands of INSN.  In the case of 64-bit operations with
   constant XOP[] has just one element:  The summand/subtrahend in XOP[0].
   The non-saturating insns up to 32 bits may or may not supply a "d" class
   scratch as XOP[3].

   If PLEN == NULL output the instructions.
   If PLEN != NULL set *PLEN to the length of the sequence in words.

   PCC is a pointer to store the instructions' effect on cc0.
   PCC may be NULL.

   PLEN and PCC default to NULL.

   OUT_LABEL defaults to TRUE.  For a description, see AVR_OUT_PLUS_1.

   Return ""  */

const char*
avr_out_plus (rtx insn, rtx *xop, int *plen, int *pcc, bool out_label)
{
  int cc_plus, cc_minus, cc_dummy;
  int len_plus, len_minus;
  rtx op[4];
  rtx xpattern = INSN_P (insn) ? single_set (as_a <rtx_insn *> (insn)) : insn;
  rtx xdest = SET_DEST (xpattern);
  machine_mode mode = GET_MODE (xdest);
  scalar_int_mode imode = int_mode_for_mode (mode).require ();
  int n_bytes = GET_MODE_SIZE (mode);
  enum rtx_code code_sat = GET_CODE (SET_SRC (xpattern));
  enum rtx_code code
    = (PLUS == code_sat || SS_PLUS == code_sat || US_PLUS == code_sat
       ? PLUS : MINUS);

  if (!pcc)
    pcc = &cc_dummy;

  /* PLUS and MINUS don't saturate:  Use modular wrap-around.  */

  if (PLUS == code_sat || MINUS == code_sat)
    code_sat = UNKNOWN;

  if (n_bytes <= 4 && REG_P (xop[2]))
    {
      avr_out_plus_1 (xop, plen, code, pcc, code_sat, 0, out_label);
      return "";
    }

  if (8 == n_bytes)
    {
      op[0] = gen_rtx_REG (DImode, ACC_A);
      op[1] = gen_rtx_REG (DImode, ACC_A);
      op[2] = avr_to_int_mode (xop[0]);
    }
  else
    {
      if (!REG_P (xop[2])
          && !CONST_INT_P (xop[2])
          && !CONST_FIXED_P (xop[2]))
        {
          return avr_out_plus_symbol (xop, code, plen, pcc);
        }

      op[0] = avr_to_int_mode (xop[0]);
      op[1] = avr_to_int_mode (xop[1]);
      op[2] = avr_to_int_mode (xop[2]);
    }

  /* Saturations and 64-bit operations don't have a clobber operand.
     For the other cases, the caller will provide a proper XOP[3].  */

  xpattern = INSN_P (insn) ? PATTERN (insn) : insn;
  op[3] = PARALLEL == GET_CODE (xpattern) ? xop[3] : NULL_RTX;

  /* Saturation will need the sign of the original operand.  */

  rtx xmsb = simplify_gen_subreg (QImode, op[2], imode, n_bytes-1);
  int sign = INTVAL (xmsb) < 0 ? -1 : 1;

  /* If we subtract and the subtrahend is a constant, then negate it
     so that avr_out_plus_1 can be used.  */

  if (MINUS == code)
    op[2] = simplify_unary_operation (NEG, imode, op[2], imode);

  /* Work out the shortest sequence.  */

  avr_out_plus_1 (op, &len_minus, MINUS, &cc_minus, code_sat, sign, out_label);
  avr_out_plus_1 (op, &len_plus, PLUS, &cc_plus, code_sat, sign, out_label);

  if (plen)
    {
      *plen = (len_minus <= len_plus) ? len_minus : len_plus;
      *pcc  = (len_minus <= len_plus) ? cc_minus : cc_plus;
    }
  else if (len_minus <= len_plus)
    avr_out_plus_1 (op, NULL, MINUS, pcc, code_sat, sign, out_label);
  else
    avr_out_plus_1 (op, NULL, PLUS, pcc, code_sat, sign, out_label);

  return "";
}


/* Output bit operation (IOR, AND, XOR) with register XOP[0] and compile
   time constant XOP[2]:

      XOP[0] = XOP[0] <op> XOP[2]

   and return "".  If PLEN == NULL, print assembler instructions to perform the
   operation; otherwise, set *PLEN to the length of the instruction sequence
   (in words) printed with PLEN == NULL.  XOP[3] is either an 8-bit clobber
   register or SCRATCH if no clobber register is needed for the operation.
   INSN is an INSN_P or a pattern of an insn.  */

const char*
avr_out_bitop (rtx insn, rtx *xop, int *plen)
{
  /* CODE and MODE of the operation.  */
  rtx xpattern = INSN_P (insn) ? single_set (as_a <rtx_insn *> (insn)) : insn;
  enum rtx_code code = GET_CODE (SET_SRC (xpattern));
  machine_mode mode = GET_MODE (xop[0]);

  /* Number of bytes to operate on.  */
  int n_bytes = GET_MODE_SIZE (mode);

  /* Value of T-flag (0 or 1) or -1 if unknow.  */
  int set_t = -1;

  /* Value (0..0xff) held in clobber register op[3] or -1 if unknown.  */
  int clobber_val = -1;

  /* op[0]: 8-bit destination register
     op[1]: 8-bit const int
     op[2]: 8-bit clobber register, SCRATCH or NULL_RTX.
     op[3]: 8-bit register containing 0xff or NULL_RTX  */
  rtx op[4];

  op[2] = QImode == mode ? NULL_RTX : xop[3];
  op[3] = NULL_RTX;

  if (plen)
    *plen = 0;

  for (int i = 0; i < n_bytes; i++)
    {
      /* We operate byte-wise on the destination.  */
      rtx reg8 = simplify_gen_subreg (QImode, xop[0], mode, i);
      rtx xval8 = simplify_gen_subreg (QImode, xop[2], mode, i);

      /* 8-bit value to operate with this byte. */
      unsigned int val8 = UINTVAL (xval8) & GET_MODE_MASK (QImode);

      /* Number of bits set in the current byte of the constant.  */
      int pop8 = popcount_hwi (val8);

      /* Registers R16..R31 can operate with immediate.  */
      bool ld_reg_p = test_hard_reg_class (LD_REGS, reg8);

      op[0] = reg8;
      op[1] = GEN_INT (val8);

      switch (code)
        {
        case IOR:

          if (0 == pop8)
            continue;
          else if (ld_reg_p)
            avr_asm_len ("ori %0,%1", op, plen, 1);
          else if (1 == pop8)
            {
              if (set_t != 1)
                avr_asm_len ("set", op, plen, 1);
              set_t = 1;

              op[1] = GEN_INT (exact_log2 (val8));
              avr_asm_len ("bld %0,%1", op, plen, 1);
            }
          else if (8 == pop8)
            {
              if (op[3] != NULL_RTX)
                avr_asm_len ("mov %0,%3", op, plen, 1);
              else
                avr_asm_len ("clr %0" CR_TAB
                             "dec %0", op, plen, 2);

              op[3] = op[0];
            }
          else
            {
              if (clobber_val != (int) val8)
                avr_asm_len ("ldi %2,%1", op, plen, 1);
              clobber_val = (int) val8;

              avr_asm_len ("or %0,%2", op, plen, 1);
            }

          continue; /* IOR */

        case AND:

          if (8 == pop8)
            continue;
          else if (0 == pop8)
            avr_asm_len ("clr %0", op, plen, 1);
          else if (ld_reg_p)
            avr_asm_len ("andi %0,%1", op, plen, 1);
          else if (7 == pop8)
            {
              if (set_t != 0)
                avr_asm_len ("clt", op, plen, 1);
              set_t = 0;

              op[1] = GEN_INT (exact_log2 (GET_MODE_MASK (QImode) & ~val8));
              avr_asm_len ("bld %0,%1", op, plen, 1);
            }
          else
            {
              if (clobber_val != (int) val8)
                avr_asm_len ("ldi %2,%1", op, plen, 1);
              clobber_val = (int) val8;

              avr_asm_len ("and %0,%2", op, plen, 1);
            }

          continue; /* AND */

        case XOR:

          if (0 == pop8)
            continue;
          else if (8 == pop8)
            avr_asm_len ("com %0", op, plen, 1);
          else if (ld_reg_p && val8 == (1 << 7))
            avr_asm_len ("subi %0,%1", op, plen, 1);
          else
            {
              if (clobber_val != (int) val8)
                avr_asm_len ("ldi %2,%1", op, plen, 1);
              clobber_val = (int) val8;

              avr_asm_len ("eor %0,%2", op, plen, 1);
            }

          continue; /* XOR */

        default:
          /* Unknown rtx_code */
          gcc_unreachable();
        }
    } /* for all sub-bytes */

  return "";
}


/* Output sign extension from XOP[1] to XOP[0] and return "".
   If PLEN == NULL, print assembler instructions to perform the operation;
   otherwise, set *PLEN to the length of the instruction sequence (in words)
   as printed with PLEN == NULL.  */

const char*
avr_out_sign_extend (rtx_insn *insn, rtx *xop, int *plen)
{
  // Size in bytes of source resp. destination operand.
  unsigned n_src = GET_MODE_SIZE (GET_MODE (xop[1]));
  unsigned n_dest = GET_MODE_SIZE (GET_MODE (xop[0]));
  rtx r_msb = all_regs_rtx[REGNO (xop[1]) + n_src - 1];

  if (plen)
    *plen = 0;

  // Copy destination to source

  if (REGNO (xop[0]) != REGNO (xop[1]))
    {
      gcc_assert (n_src <= 2);

      if (n_src == 2)
        avr_asm_len (AVR_HAVE_MOVW
                     ? "movw %0,%1"
                     : "mov %B0,%B1", xop, plen, 1);
      if (n_src == 1 || !AVR_HAVE_MOVW)
        avr_asm_len ("mov %A0,%A1", xop, plen, 1);
    }

  // Set Carry to the sign bit MSB.7...

  if (REGNO (xop[0]) == REGNO (xop[1])
      || !reg_unused_after (insn, r_msb))
    {
      avr_asm_len ("mov __tmp_reg__,%0", &r_msb, plen, 1);
      r_msb = tmp_reg_rtx;
    }

  avr_asm_len ("lsl %0", &r_msb, plen, 1);

  // ...and propagate it to all the new sign bits

  for (unsigned n = n_src; n < n_dest; n++)
    avr_asm_len ("sbc %0,%0", &all_regs_rtx[REGNO (xop[0]) + n], plen, 1);

  return "";
}


/* PLEN == NULL: Output code to add CONST_INT OP[0] to SP.
   PLEN != NULL: Set *PLEN to the length of that sequence.
   Return "".  */

const char*
avr_out_addto_sp (rtx *op, int *plen)
{
  int pc_len = AVR_2_BYTE_PC ? 2 : 3;
  int addend = INTVAL (op[0]);

  if (plen)
    *plen = 0;

  if (addend < 0)
    {
      if (flag_verbose_asm || flag_print_asm_name)
        avr_asm_len (ASM_COMMENT_START "SP -= %n0", op, plen, 0);

      while (addend <= -pc_len)
        {
          addend += pc_len;
          avr_asm_len ("rcall .", op, plen, 1);
        }

      while (addend++ < 0)
        avr_asm_len ("push __tmp_reg__", op, plen, 1);
    }
  else if (addend > 0)
    {
      if (flag_verbose_asm || flag_print_asm_name)
        avr_asm_len (ASM_COMMENT_START "SP += %0", op, plen, 0);

      while (addend-- > 0)
        avr_asm_len ("pop __tmp_reg__", op, plen, 1);
    }

  return "";
}


/* Output instructions to insert an inverted bit into OPERANDS[0]:
   $0.$1 = ~$2.$3      if XBITNO = NULL
   $0.$1 = ~$2.XBITNO  if XBITNO != NULL.
   If PLEN = NULL then output the respective instruction sequence which
   is a combination of BST / BLD and some instruction(s) to invert the bit.
   If PLEN != NULL then store the length of the sequence (in words) in *PLEN.
   Return "".  */

const char*
avr_out_insert_notbit (rtx_insn *insn, rtx operands[], rtx xbitno, int *plen)
{
  rtx op[4] = { operands[0], operands[1], operands[2],
                xbitno == NULL_RTX ? operands [3] : xbitno };

  if (INTVAL (op[1]) == 7
      && test_hard_reg_class (LD_REGS, op[0]))
    {
      /* If the inserted bit number is 7 and we have a d-reg, then invert
         the bit after the insertion by means of SUBI *,0x80.  */

      if (INTVAL (op[3]) == 7
          && REGNO (op[0]) == REGNO (op[2]))
        {
          avr_asm_len ("subi %0,0x80", op, plen, -1);
        }
      else
        {
          avr_asm_len ("bst %2,%3" CR_TAB
                       "bld %0,%1" CR_TAB
                       "subi %0,0x80", op, plen, -3);
        }
    }
  else if (test_hard_reg_class (LD_REGS, op[0])
           && (INTVAL (op[1]) != INTVAL (op[3])
               || !reg_overlap_mentioned_p (op[0], op[2])))
    {
      /* If the destination bit is in a d-reg we can jump depending
         on the source bit and use ANDI / ORI.  This just applies if we
         have not an early-clobber situation with the bit.  */

      avr_asm_len ("andi %0,~(1<<%1)" CR_TAB
                   "sbrs %2,%3"       CR_TAB
                   "ori %0,1<<%1", op, plen, -3);
    }
  else
    {
      /* Otherwise, invert the bit by means of COM before we store it with
         BST and then undo the COM if needed.  */

      avr_asm_len ("com %2" CR_TAB
                   "bst %2,%3", op, plen, -2);

      if (!reg_unused_after (insn, op[2])
          // A simple 'reg_unused_after' is not enough because that function
          // assumes that the destination register is overwritten completely
          // and hence is in order for our purpose.  This is not the case
          // with BLD which just changes one bit of the destination.
          || reg_overlap_mentioned_p (op[0], op[2]))
        {
          /* Undo the COM from above.  */
          avr_asm_len ("com %2", op, plen, 1);
        }

      avr_asm_len ("bld %0,%1", op, plen, 1);
    }

  return "";
}


/* Outputs instructions needed for fixed point type conversion.
   This includes converting between any fixed point type, as well
   as converting to any integer type.  Conversion between integer
   types is not supported.

   Converting signed fractional types requires a bit shift if converting
   to or from any unsigned fractional type because the decimal place is
   shifted by 1 bit.  When the destination is a signed fractional, the sign
   is stored in either the carry or T bit.  */

const char*
avr_out_fract (rtx_insn *insn, rtx operands[], bool intsigned, int *plen)
{
  rtx xop[6];
  RTX_CODE shift = UNKNOWN;
  bool sign_in_carry = false;
  bool msb_in_carry = false;
  bool lsb_in_tmp_reg = false;
  bool lsb_in_carry = false;
  bool frac_rounded = false;
  const char *code_ashift = "lsl %0";


#define MAY_CLOBBER(RR)                                                 \
  /* Shorthand used below.  */                                          \
  ((sign_bytes                                                          \
    && IN_RANGE (RR, dest.regno_msb - sign_bytes + 1, dest.regno_msb))  \
   || (offset && IN_RANGE (RR, dest.regno, dest.regno_msb))		\
   || (reg_unused_after (insn, all_regs_rtx[RR])                        \
       && !IN_RANGE (RR, dest.regno, dest.regno_msb)))

  struct
  {
    /* bytes       : Length of operand in bytes.
       ibyte       : Length of integral part in bytes.
       fbyte, fbit : Length of fractional part in bytes, bits.  */

    bool sbit;
    unsigned fbit, bytes, ibyte, fbyte;
    unsigned regno, regno_msb;
  } dest, src, *val[2] = { &dest, &src };

  if (plen)
    *plen = 0;

  /* Step 0:  Determine information on source and destination operand we
     ======   will need in the remainder.  */

  for (size_t i = 0; i < ARRAY_SIZE (val); i++)
    {
      machine_mode mode;

      xop[i] = operands[i];

      mode = GET_MODE (xop[i]);

      val[i]->bytes = GET_MODE_SIZE (mode);
      val[i]->regno = REGNO (xop[i]);
      val[i]->regno_msb = REGNO (xop[i]) + val[i]->bytes - 1;

      if (SCALAR_INT_MODE_P (mode))
        {
          val[i]->sbit = intsigned;
          val[i]->fbit = 0;
        }
      else if (ALL_SCALAR_FIXED_POINT_MODE_P (mode))
        {
          val[i]->sbit = SIGNED_SCALAR_FIXED_POINT_MODE_P (mode);
          val[i]->fbit = GET_MODE_FBIT (mode);
        }
      else
        fatal_insn ("unsupported fixed-point conversion", insn);

      val[i]->fbyte = (1 + val[i]->fbit) / BITS_PER_UNIT;
      val[i]->ibyte = val[i]->bytes - val[i]->fbyte;
    }

  // Byte offset of the decimal point taking into account different place
  // of the decimal point in input and output and different register numbers
  // of input and output.
  int offset = dest.regno - src.regno + dest.fbyte - src.fbyte;

  // Number of destination bytes that will come from sign / zero extension.
  int sign_bytes = (dest.ibyte - src.ibyte) * (dest.ibyte > src.ibyte);

  // Number of bytes at the low end to be filled with zeros.
  int zero_bytes = (dest.fbyte - src.fbyte) * (dest.fbyte > src.fbyte);

  // Do we have a 16-Bit register that is cleared?
  rtx clrw = NULL_RTX;

  bool sign_extend = src.sbit && sign_bytes;

  if (0 == dest.fbit % 8 && 7 == src.fbit % 8)
    shift = ASHIFT;
  else if (7 == dest.fbit % 8 && 0 == src.fbit % 8)
    shift = ASHIFTRT;
  else if (dest.fbit % 8 == src.fbit % 8)
    shift = UNKNOWN;
  else
    gcc_unreachable();

  /* If we need to round the fraction part, we might need to save/round it
     before clobbering any of it in Step 1.  Also, we might want to do
     the rounding now to make use of LD_REGS.  */
  if (SCALAR_INT_MODE_P (GET_MODE (xop[0]))
      && SCALAR_ACCUM_MODE_P (GET_MODE (xop[1]))
      && !TARGET_FRACT_CONV_TRUNC)
    {
      bool overlap
        = (src.regno <=
           (offset ? dest.regno_msb - sign_bytes : dest.regno + zero_bytes - 1)
           && dest.regno - offset -1 >= dest.regno);
      unsigned s0 = dest.regno - offset -1;
      bool use_src = true;
      unsigned sn;
      unsigned copied_msb = src.regno_msb;
      bool have_carry = false;

      if (src.ibyte > dest.ibyte)
        copied_msb -= src.ibyte - dest.ibyte;

      for (sn = s0; sn <= copied_msb; sn++)
        if (!IN_RANGE (sn, dest.regno, dest.regno_msb)
            && !reg_unused_after (insn, all_regs_rtx[sn]))
          use_src = false;
      if (use_src && TEST_HARD_REG_BIT (reg_class_contents[LD_REGS], s0))
        {
          avr_asm_len ("tst %0" CR_TAB "brpl 0f",
                       &all_regs_rtx[src.regno_msb], plen, 2);
          sn = src.regno;
          if (sn < s0)
            {
              if (TEST_HARD_REG_BIT (reg_class_contents[LD_REGS], sn))
                avr_asm_len ("cpi %0,1", &all_regs_rtx[sn], plen, 1);
              else
                avr_asm_len ("sec" CR_TAB
                             "cpc %0,__zero_reg__",
                             &all_regs_rtx[sn], plen, 2);
              have_carry = true;
            }
          while (++sn < s0)
            avr_asm_len ("cpc %0,__zero_reg__", &all_regs_rtx[sn], plen, 1);

          avr_asm_len (have_carry ? "sbci %0,128" : "subi %0,129",
                       &all_regs_rtx[s0], plen, 1);
          for (sn = src.regno + src.fbyte; sn <= copied_msb; sn++)
            avr_asm_len ("sbci %0,255", &all_regs_rtx[sn], plen, 1);
          avr_asm_len ("\n0:", NULL, plen, 0);
          frac_rounded = true;
        }
      else if (use_src && overlap)
        {
          avr_asm_len ("clr __tmp_reg__" CR_TAB
                       "sbrc %1,0"       CR_TAB
                       "dec __tmp_reg__", xop, plen, 1);
          sn = src.regno;
          if (sn < s0)
            {
              avr_asm_len ("add %0,__tmp_reg__", &all_regs_rtx[sn], plen, 1);
              have_carry = true;
            }

          while (++sn < s0)
            avr_asm_len ("adc %0,__tmp_reg__", &all_regs_rtx[sn], plen, 1);

          if (have_carry)
            avr_asm_len ("clt"                CR_TAB
                         "bld __tmp_reg__,7"  CR_TAB
                         "adc %0,__tmp_reg__",
                         &all_regs_rtx[s0], plen, 1);
          else
            avr_asm_len ("lsr __tmp_reg" CR_TAB
                         "add %0,__tmp_reg__",
                         &all_regs_rtx[s0], plen, 2);
          for (sn = src.regno + src.fbyte; sn <= copied_msb; sn++)
            avr_asm_len ("adc %0,__zero_reg__", &all_regs_rtx[sn], plen, 1);
          frac_rounded = true;
        }
      else if (overlap)
        {
          bool use_src
            = (TEST_HARD_REG_BIT (reg_class_contents[LD_REGS], s0)
               && (IN_RANGE (s0, dest.regno, dest.regno_msb)
                   || reg_unused_after (insn, all_regs_rtx[s0])));
          xop[2] = all_regs_rtx[s0];
          unsigned sn = src.regno;
          if (!use_src || sn == s0)
            avr_asm_len ("mov __tmp_reg__,%2", xop, plen, 1);
          /* We need to consider to-be-discarded bits
             if the value is negative.  */
          if (sn < s0)
            {
              avr_asm_len ("tst %0" CR_TAB
                           "brpl 0f",
                           &all_regs_rtx[src.regno_msb], plen, 2);
              /* Test to-be-discarded bytes for any nozero bits.
                 ??? Could use OR or SBIW to test two registers at once.  */
              if (sn < s0)
                avr_asm_len ("cp %0,__zero_reg__", &all_regs_rtx[sn], plen, 1);

              while (++sn < s0)
                avr_asm_len ("cpc %0,__zero_reg__", &all_regs_rtx[sn], plen, 1);
              /* Set bit 0 in __tmp_reg__ if any of the lower bits was set.  */
              if (use_src)
                avr_asm_len ("breq 0f" CR_TAB
                             "ori %2,1"
                             "\n0:\t" "mov __tmp_reg__,%2",
                             xop, plen, 3);
              else
                avr_asm_len ("breq 0f" CR_TAB
                             "set"     CR_TAB
                             "bld __tmp_reg__,0\n0:",
                             xop, plen, 3);
            }
          lsb_in_tmp_reg = true;
        }
    }

  /* Step 1:  Clear bytes at the low end and copy payload bits from source
     ======   to destination.  */

  int step = offset < 0 ? 1 : -1;
  unsigned d0 = offset < 0 ? dest.regno : dest.regno_msb;

  // We cleared at least that number of registers.
  int clr_n = 0;

  for (; d0 >= dest.regno && d0 <= dest.regno_msb; d0 += step)
    {
      // Next regno of destination is needed for MOVW
      unsigned d1 = d0 + step;

      // Current and next regno of source
      signed s0 = d0 - offset;
      signed s1 = s0 + step;

      // Must current resp. next regno be CLRed?  This applies to the low
      // bytes of the destination that have no associated source bytes.
      bool clr0 = s0 < (signed) src.regno;
      bool clr1 = s1 < (signed) src.regno && d1 >= dest.regno;

      // First gather what code to emit (if any) and additional step to
      // apply if a MOVW is in use.  xop[2] is destination rtx and xop[3]
      // is the source rtx for the current loop iteration.
      const char *code = NULL;
      int stepw = 0;

      if (clr0)
        {
          if (AVR_HAVE_MOVW && clr1 && clrw)
            {
              xop[2] = all_regs_rtx[d0 & ~1];
              xop[3] = clrw;
              code = "movw %2,%3";
              stepw = step;
            }
          else
            {
              xop[2] = all_regs_rtx[d0];
              code = "clr %2";

              if (++clr_n >= 2
                  && !clrw
                  && d0 % 2 == (step > 0))
                {
                  clrw = all_regs_rtx[d0 & ~1];
                }
            }
        }
      else if (offset && s0 <= (signed) src.regno_msb)
        {
          int movw = AVR_HAVE_MOVW && offset % 2 == 0
            && d0 % 2 == (offset > 0)
            && d1 <= dest.regno_msb && d1 >= dest.regno
            && s1 <= (signed) src.regno_msb  && s1 >= (signed) src.regno;

          xop[2] = all_regs_rtx[d0 & ~movw];
          xop[3] = all_regs_rtx[s0 & ~movw];
          code = movw ? "movw %2,%3" : "mov %2,%3";
          stepw = step * movw;
        }

      if (code)
        {
          if (sign_extend && shift != ASHIFT && !sign_in_carry
              && (d0 == src.regno_msb || d0 + stepw == src.regno_msb))
            {
              /* We are going to override the sign bit.  If we sign-extend,
                 store the sign in the Carry flag.  This is not needed if
                 the destination will be ASHIFT in the remainder because
                 the ASHIFT will set Carry without extra instruction.  */

              avr_asm_len ("lsl %0", &all_regs_rtx[src.regno_msb], plen, 1);
              sign_in_carry = true;
            }

          unsigned src_msb = dest.regno_msb - sign_bytes - offset + 1;

          if (!sign_extend && shift == ASHIFTRT && !msb_in_carry
              && src.ibyte > dest.ibyte
              && (d0 == src_msb || d0 + stepw == src_msb))
            {
              /* We are going to override the MSB.  If we shift right,
                 store the MSB in the Carry flag.  This is only needed if
                 we don't sign-extend becaue with sign-extension the MSB
                 (the sign) will be produced by the sign extension.  */

              avr_asm_len ("lsr %0", &all_regs_rtx[src_msb], plen, 1);
              msb_in_carry = true;
            }

          unsigned src_lsb = dest.regno - offset -1;

          if (shift == ASHIFT && src.fbyte > dest.fbyte && !lsb_in_carry
	      && !lsb_in_tmp_reg
              && (d0 == src_lsb || d0 + stepw == src_lsb))
            {
              /* We are going to override the new LSB; store it into carry.  */

              avr_asm_len ("lsl %0", &all_regs_rtx[src_lsb], plen, 1);
              code_ashift = "rol %0";
              lsb_in_carry = true;
            }

          avr_asm_len (code, xop, plen, 1);
          d0 += stepw;
        }
    }

  /* Step 2:  Shift destination left by 1 bit position.  This might be needed
     ======   for signed input and unsigned output.  */

  if (shift == ASHIFT && src.fbyte > dest.fbyte && !lsb_in_carry)
    {
      unsigned s0 = dest.regno - offset -1;

      /* n1169 4.1.4 says:
	 "Conversions from a fixed-point to an integer type round toward zero."
	 Hence, converting a fract type to integer only gives a non-zero result
	 for -1.  */
      if (SCALAR_INT_MODE_P (GET_MODE (xop[0]))
	  && SCALAR_FRACT_MODE_P (GET_MODE (xop[1]))
	  && !TARGET_FRACT_CONV_TRUNC)
	{
	  gcc_assert (s0 == src.regno_msb);
	  /* Check if the input is -1.  We do that by checking if negating
	     the input causes an integer overflow.  */
	  unsigned sn = src.regno;
	  avr_asm_len ("cp __zero_reg__,%0", &all_regs_rtx[sn++], plen, 1);
	  while (sn <= s0)
	    avr_asm_len ("cpc __zero_reg__,%0", &all_regs_rtx[sn++], plen, 1);

	  /* Overflow goes with set carry.  Clear carry otherwise.  */
	  avr_asm_len ("brvs 0f" CR_TAB
                       "clc\n0:", NULL, plen, 2);
	}
      /* Likewise, when converting from accumulator types to integer, we
	 need to round up negative values.  */
      else if (SCALAR_INT_MODE_P (GET_MODE (xop[0]))
	       && SCALAR_ACCUM_MODE_P (GET_MODE (xop[1]))
	       && !TARGET_FRACT_CONV_TRUNC
	       && !frac_rounded)
	{
	  bool have_carry = false;

	  xop[2] = all_regs_rtx[s0];
	  if (!lsb_in_tmp_reg && !MAY_CLOBBER (s0))
	    avr_asm_len ("mov __tmp_reg__,%2", xop, plen, 1);
	  avr_asm_len ("tst %0" CR_TAB "brpl 0f",
		       &all_regs_rtx[src.regno_msb], plen, 2);
	  if (!lsb_in_tmp_reg)
	    {
	      unsigned sn = src.regno;
	      if (sn < s0)
		{
		  avr_asm_len ("cp __zero_reg__,%0", &all_regs_rtx[sn],
			       plen, 1);
		  have_carry = true;
		}
	      while (++sn < s0)
		avr_asm_len ("cpc __zero_reg__,%0", &all_regs_rtx[sn], plen, 1);
	      lsb_in_tmp_reg = !MAY_CLOBBER (s0);
	    }
	  /* Add in C and the rounding value 127.  */
	  /* If the destination msb is a sign byte, and in LD_REGS,
	     grab it as a temporary.  */
	  if (sign_bytes
	      && TEST_HARD_REG_BIT (reg_class_contents[LD_REGS],
				    dest.regno_msb))
	    {
	      xop[3] = all_regs_rtx[dest.regno_msb];
	      avr_asm_len ("ldi %3,127", xop, plen, 1);
	      avr_asm_len ((have_carry && lsb_in_tmp_reg ? "adc __tmp_reg__,%3"
			    : have_carry ? "adc %2,%3"
			    : lsb_in_tmp_reg ? "add __tmp_reg__,%3"
			    : "add %2,%3"),
			   xop, plen, 1);
	    }
	  else
	    {
	      /* Fall back to use __zero_reg__ as a temporary.  */
	      avr_asm_len ("dec __zero_reg__", NULL, plen, 1);
	      if (have_carry)
		avr_asm_len ("clt" CR_TAB
                             "bld __zero_reg__,7", NULL, plen, 2);
	      else
		avr_asm_len ("lsr __zero_reg__", NULL, plen, 1);
	      avr_asm_len (have_carry && lsb_in_tmp_reg
                           ? "adc __tmp_reg__,__zero_reg__"
                           : have_carry ? "adc %2,__zero_reg__"
                           : lsb_in_tmp_reg ? "add __tmp_reg__,__zero_reg__"
                           : "add %2,__zero_reg__",
			   xop, plen, 1);
	      avr_asm_len ("eor __zero_reg__,__zero_reg__", NULL, plen, 1);
	    }

          for (d0 = dest.regno + zero_bytes;
	       d0 <= dest.regno_msb - sign_bytes; d0++)
	    avr_asm_len ("adc %0,__zero_reg__", &all_regs_rtx[d0], plen, 1);

          avr_asm_len (lsb_in_tmp_reg
		       ? "\n0:\t" "lsl __tmp_reg__"
                       : "\n0:\t" "lsl %2",
		       xop, plen, 1);
	}
      else if (MAY_CLOBBER (s0))
        avr_asm_len ("lsl %0", &all_regs_rtx[s0], plen, 1);
      else
        avr_asm_len ("mov __tmp_reg__,%0" CR_TAB
                     "lsl __tmp_reg__", &all_regs_rtx[s0], plen, 2);

      code_ashift = "rol %0";
      lsb_in_carry = true;
    }

  if (shift == ASHIFT)
    {
      for (d0 = dest.regno + zero_bytes;
           d0 <= dest.regno_msb - sign_bytes; d0++)
        {
          avr_asm_len (code_ashift, &all_regs_rtx[d0], plen, 1);
          code_ashift = "rol %0";
        }

      lsb_in_carry = false;
      sign_in_carry = true;
    }

  /* Step 4a:  Store MSB in carry if we don't already have it or will produce
     =======   it in sign-extension below.  */

  if (!sign_extend && shift == ASHIFTRT && !msb_in_carry
      && src.ibyte > dest.ibyte)
    {
      unsigned s0 = dest.regno_msb - sign_bytes - offset + 1;

      if (MAY_CLOBBER (s0))
        avr_asm_len ("lsr %0", &all_regs_rtx[s0], plen, 1);
      else
        avr_asm_len ("mov __tmp_reg__,%0" CR_TAB
                     "lsr __tmp_reg__", &all_regs_rtx[s0], plen, 2);

      msb_in_carry = true;
    }

  /* Step 3:  Sign-extend or zero-extend the destination as needed.
     ======   */

  if (sign_extend && !sign_in_carry)
    {
      unsigned s0 = src.regno_msb;

      if (MAY_CLOBBER (s0))
        avr_asm_len ("lsl %0", &all_regs_rtx[s0], plen, 1);
      else
        avr_asm_len ("mov __tmp_reg__,%0" CR_TAB
                     "lsl __tmp_reg__", &all_regs_rtx[s0], plen, 2);

      sign_in_carry = true;
    }

  gcc_assert (sign_in_carry + msb_in_carry + lsb_in_carry <= 1);

  unsigned copies = 0;
  rtx movw = sign_extend ? NULL_RTX : clrw;

  for (d0 = dest.regno_msb - sign_bytes + 1; d0 <= dest.regno_msb; d0++)
    {
      if (AVR_HAVE_MOVW && movw
          && d0 % 2 == 0 && d0 + 1 <= dest.regno_msb)
        {
          xop[2] = all_regs_rtx[d0];
          xop[3] = movw;
          avr_asm_len ("movw %2,%3", xop, plen, 1);
          d0++;
        }
      else
        {
          avr_asm_len (sign_extend ? "sbc %0,%0" : "clr %0",
                       &all_regs_rtx[d0], plen, 1);

          if (++copies >= 2 && !movw && d0 % 2 == 1)
            movw = all_regs_rtx[d0-1];
        }
    } /* for */


  /* Step 4:  Right shift the destination.  This might be needed for
     ======   conversions from unsigned to signed.  */

  if (shift == ASHIFTRT)
    {
      const char *code_ashiftrt = "lsr %0";

      if (sign_extend || msb_in_carry)
        code_ashiftrt = "ror %0";

      if (src.sbit && src.ibyte == dest.ibyte)
        code_ashiftrt = "asr %0";

      for (d0 = dest.regno_msb - sign_bytes;
           d0 >= dest.regno + zero_bytes - 1 && d0 >= dest.regno; d0--)
        {
          avr_asm_len (code_ashiftrt, &all_regs_rtx[d0], plen, 1);
          code_ashiftrt = "ror %0";
        }
    }

#undef MAY_CLOBBER

  return "";
}


/* Output fixed-point rounding.  XOP[0] = XOP[1] is the operand to round.
   XOP[2] is the rounding point, a CONST_INT.  The function prints the
   instruction sequence if PLEN = NULL and computes the length in words
   of the sequence if PLEN != NULL.  Most of this function deals with
   preparing operands for calls to `avr_out_plus' and `avr_out_bitop'.  */

const char*
avr_out_round (rtx_insn *insn ATTRIBUTE_UNUSED, rtx *xop, int *plen)
{
  scalar_mode mode = as_a <scalar_mode> (GET_MODE (xop[0]));
  scalar_int_mode imode = int_mode_for_mode (mode).require ();
  // The smallest fractional bit not cleared by the rounding is 2^(-RP).
  int fbit = (int) GET_MODE_FBIT (mode);
  double_int i_add = double_int_zero.set_bit (fbit-1 - INTVAL (xop[2]));
  wide_int wi_add = wi::set_bit_in_zero (fbit-1 - INTVAL (xop[2]),
					 GET_MODE_PRECISION (imode));
  // Lengths of PLUS and AND parts.
  int len_add = 0, *plen_add = plen ? &len_add : NULL;
  int len_and = 0, *plen_and = plen ? &len_and : NULL;

  // Add-Saturate  1/2 * 2^(-RP).  Don't print the label "0:" when printing
  // the saturated addition so that we can emit the "rjmp 1f" before the
  // "0:" below.

  rtx xadd = const_fixed_from_double_int (i_add, mode);
  rtx xpattern, xsrc, op[4];

  xsrc = SIGNED_FIXED_POINT_MODE_P (mode)
    ? gen_rtx_SS_PLUS (mode, xop[1], xadd)
    : gen_rtx_US_PLUS (mode, xop[1], xadd);
  xpattern = gen_rtx_SET (xop[0], xsrc);

  op[0] = xop[0];
  op[1] = xop[1];
  op[2] = xadd;
  avr_out_plus (xpattern, op, plen_add, NULL, false /* Don't print "0:" */);

  avr_asm_len ("rjmp 1f" CR_TAB
               "0:", NULL, plen_add, 1);

  // Keep  all bits from RP and higher:   ... 2^(-RP)
  // Clear all bits from RP+1 and lower:              2^(-RP-1) ...
  // Rounding point                           ^^^^^^^
  // Added above                                      ^^^^^^^^^
  rtx xreg = simplify_gen_subreg (imode, xop[0], mode, 0);
  rtx xmask = immed_wide_int_const (-wi_add - wi_add, imode);

  xpattern = gen_rtx_SET (xreg, gen_rtx_AND (imode, xreg, xmask));

  op[0] = xreg;
  op[1] = xreg;
  op[2] = xmask;
  op[3] = gen_rtx_SCRATCH (QImode);
  avr_out_bitop (xpattern, op, plen_and);
  avr_asm_len ("1:", NULL, plen, 0);

  if (plen)
    *plen = len_add + len_and;

  return "";
}


/* Create RTL split patterns for byte sized rotate expressions.  This
   produces a series of move instructions and considers overlap situations.
   Overlapping non-HImode operands need a scratch register.  */

bool
avr_rotate_bytes (rtx operands[])
{
  machine_mode mode = GET_MODE (operands[0]);
  bool overlapped = reg_overlap_mentioned_p (operands[0], operands[1]);
  bool same_reg = rtx_equal_p (operands[0], operands[1]);
  int num = INTVAL (operands[2]);
  rtx scratch = operands[3];
  /* Work out if byte or word move is needed.  Odd byte rotates need QImode.
     Word move if no scratch is needed, otherwise use size of scratch.  */
  machine_mode move_mode = QImode;
  int move_size, offset, size;

  if (num & 0xf)
    move_mode = QImode;
  else if ((mode == SImode && !same_reg) || !overlapped)
    move_mode = HImode;
  else
    move_mode = GET_MODE (scratch);

  /* Force DI rotate to use QI moves since other DI moves are currently split
     into QI moves so forward propagation works better.  */
  if (mode == DImode)
    move_mode = QImode;
  /* Make scratch smaller if needed.  */
  if (SCRATCH != GET_CODE (scratch)
      && HImode == GET_MODE (scratch)
      && QImode == move_mode)
    scratch = simplify_gen_subreg (move_mode, scratch, HImode, 0);

  move_size = GET_MODE_SIZE (move_mode);
  /* Number of bytes/words to rotate.  */
  offset = (num  >> 3) / move_size;
  /* Number of moves needed.  */
  size = GET_MODE_SIZE (mode) / move_size;
  /* Himode byte swap is special case to avoid a scratch register.  */
  if (mode == HImode && same_reg)
    {
      /* HImode byte swap, using xor.  This is as quick as using scratch.  */
      rtx src, dst;
      src = simplify_gen_subreg (move_mode, operands[1], mode, 0);
      dst = simplify_gen_subreg (move_mode, operands[0], mode, 1);
      if (!rtx_equal_p (dst, src))
        {
          emit_move_insn (dst, gen_rtx_XOR (QImode, dst, src));
          emit_move_insn (src, gen_rtx_XOR (QImode, src, dst));
          emit_move_insn (dst, gen_rtx_XOR (QImode, dst, src));
        }
    }
  else
    {
#define MAX_SIZE 8 /* GET_MODE_SIZE (DImode) / GET_MODE_SIZE (QImode)  */
      /* Create linked list of moves to determine move order.  */
      struct {
        rtx src, dst;
        int links;
      } move[MAX_SIZE + 8];
      int blocked, moves;

      gcc_assert (size <= MAX_SIZE);
      /* Generate list of subreg moves.  */
      for (int i = 0; i < size; i++)
        {
          int from = i;
          int to = (from + offset) % size;
          move[i].src = simplify_gen_subreg (move_mode, operands[1],
                                             mode, from * move_size);
          move[i].dst = simplify_gen_subreg (move_mode, operands[0],
                                             mode, to * move_size);
          move[i].links = -1;
        }
      /* Mark dependence where a dst of one move is the src of another move.
         The first move is a conflict as it must wait until second is
         performed.  We ignore moves to self - we catch this later.  */
      if (overlapped)
        for (int i = 0; i < size; i++)
          if (reg_overlap_mentioned_p (move[i].dst, operands[1]))
            for (int j = 0; j < size; j++)
              if (j != i && rtx_equal_p (move[j].src, move[i].dst))
                {
                  /* The dst of move i is the src of move j.  */
                  move[i].links = j;
                  break;
                }

      blocked = -1;
      moves = 0;
      /* Go through move list and perform non-conflicting moves.  As each
         non-overlapping move is made, it may remove other conflicts
         so the process is repeated until no conflicts remain.  */
      do
        {
          blocked = -1;
          moves = 0;
          /* Emit move where dst is not also a src or we have used that
             src already.  */
          for (int i = 0; i < size; i++)
            if (move[i].src != NULL_RTX)
              {
                if (move[i].links == -1
                    || move[move[i].links].src == NULL_RTX)
                  {
                    moves++;
                    /* Ignore NOP moves to self.  */
                    if (!rtx_equal_p (move[i].dst, move[i].src))
                      emit_move_insn (move[i].dst, move[i].src);

                    /* Remove  conflict from list.  */
                    move[i].src = NULL_RTX;
                  }
                else
                  blocked = i;
              }

          /* Check for deadlock. This is when no moves occurred and we have
             at least one blocked move.  */
          if (moves == 0 && blocked != -1)
            {
              /* Need to use scratch register to break deadlock.
                 Add move to put dst of blocked move into scratch.
                 When this move occurs, it will break chain deadlock.
                 The scratch register is substituted for real move.  */

              gcc_assert (SCRATCH != GET_CODE (scratch));

              move[size].src = move[blocked].dst;
              move[size].dst =  scratch;
              /* Scratch move is never blocked.  */
              move[size].links = -1;
              /* Make sure we have valid link.  */
              gcc_assert (move[blocked].links != -1);
              /* Replace src of  blocking move with scratch reg.  */
              move[move[blocked].links].src = scratch;
              /* Make dependent on scratch move occurring.  */
              move[blocked].links = size;
              size=size+1;
            }
        }
      while (blocked != -1);
    }
  return true;
}


/* Worker function for `ADJUST_INSN_LENGTH'.  */
/* Modifies the length assigned to instruction INSN
   LEN is the initially computed length of the insn.  */

int
avr_adjust_insn_length (rtx_insn *insn, int len)
{
  rtx *op = recog_data.operand;
  enum attr_adjust_len adjust_len;

  /* As we pretend jump tables in .text, fix branch offsets crossing jump
     tables now.  */

  if (JUMP_TABLE_DATA_P (insn))
    return 0;

  /* Some complex insns don't need length adjustment and therefore
     the length need not/must not be adjusted for these insns.
     It is easier to state this in an insn attribute "adjust_len" than
     to clutter up code here...  */

  if (!NONDEBUG_INSN_P (insn)
      || -1 == recog_memoized (insn))
    {
      return len;
    }

  /* Read from insn attribute "adjust_len" if/how length is to be adjusted.  */

  adjust_len = get_attr_adjust_len (insn);

  if (adjust_len == ADJUST_LEN_NO)
    {
      /* Nothing to adjust: The length from attribute "length" is fine.
         This is the default.  */

      return len;
    }

  /* Extract insn's operands.  */

  extract_constrain_insn_cached (insn);

  /* Dispatch to right function.  */

  switch (adjust_len)
    {
    case ADJUST_LEN_RELOAD_IN16: output_reload_inhi (op, op[2], &len); break;
    case ADJUST_LEN_RELOAD_IN24: avr_out_reload_inpsi (op, op[2], &len); break;
    case ADJUST_LEN_RELOAD_IN32: output_reload_insisf (op, op[2], &len); break;

    case ADJUST_LEN_OUT_BITOP: avr_out_bitop (insn, op, &len); break;

    case ADJUST_LEN_PLUS: avr_out_plus (insn, op, &len); break;
    case ADJUST_LEN_ADDTO_SP: avr_out_addto_sp (op, &len); break;

    case ADJUST_LEN_MOV8:  output_movqi (insn, op, &len); break;
    case ADJUST_LEN_MOV16: output_movhi (insn, op, &len); break;
    case ADJUST_LEN_MOV24: avr_out_movpsi (insn, op, &len); break;
    case ADJUST_LEN_MOV32: output_movsisf (insn, op, &len); break;
    case ADJUST_LEN_MOVMEM: avr_out_movmem (insn, op, &len); break;
    case ADJUST_LEN_XLOAD: avr_out_xload (insn, op, &len); break;
    case ADJUST_LEN_SEXT: avr_out_sign_extend (insn, op, &len); break;

    case ADJUST_LEN_SFRACT: avr_out_fract (insn, op, true, &len); break;
    case ADJUST_LEN_UFRACT: avr_out_fract (insn, op, false, &len); break;
    case ADJUST_LEN_ROUND: avr_out_round (insn, op, &len); break;

    case ADJUST_LEN_TSTHI: avr_out_tsthi (insn, op, &len); break;
    case ADJUST_LEN_TSTPSI: avr_out_tstpsi (insn, op, &len); break;
    case ADJUST_LEN_TSTSI: avr_out_tstsi (insn, op, &len); break;
    case ADJUST_LEN_COMPARE: avr_out_compare (insn, op, &len); break;
    case ADJUST_LEN_COMPARE64: avr_out_compare64 (insn, op, &len); break;

    case ADJUST_LEN_LSHRQI: lshrqi3_out (insn, op, &len); break;
    case ADJUST_LEN_LSHRHI: lshrhi3_out (insn, op, &len); break;
    case ADJUST_LEN_LSHRSI: lshrsi3_out (insn, op, &len); break;

    case ADJUST_LEN_ASHRQI: ashrqi3_out (insn, op, &len); break;
    case ADJUST_LEN_ASHRHI: ashrhi3_out (insn, op, &len); break;
    case ADJUST_LEN_ASHRSI: ashrsi3_out (insn, op, &len); break;

    case ADJUST_LEN_ASHLQI: ashlqi3_out (insn, op, &len); break;
    case ADJUST_LEN_ASHLHI: ashlhi3_out (insn, op, &len); break;
    case ADJUST_LEN_ASHLSI: ashlsi3_out (insn, op, &len); break;

    case ADJUST_LEN_ASHLPSI: avr_out_ashlpsi3 (insn, op, &len); break;
    case ADJUST_LEN_ASHRPSI: avr_out_ashrpsi3 (insn, op, &len); break;
    case ADJUST_LEN_LSHRPSI: avr_out_lshrpsi3 (insn, op, &len); break;

    case ADJUST_LEN_CALL: len = AVR_HAVE_JMP_CALL ? 2 : 1; break;

    case ADJUST_LEN_INSERT_BITS: avr_out_insert_bits (op, &len); break;

    case ADJUST_LEN_INSV_NOTBIT:
      avr_out_insert_notbit (insn, op, NULL_RTX, &len);
      break;
    case ADJUST_LEN_INSV_NOTBIT_0:
      avr_out_insert_notbit (insn, op, const0_rtx, &len);
      break;
    case ADJUST_LEN_INSV_NOTBIT_7:
      avr_out_insert_notbit (insn, op, GEN_INT (7), &len);
      break;

    default:
      gcc_unreachable();
    }

  return len;
}

/* Return nonzero if register REG dead after INSN.  */

int
reg_unused_after (rtx_insn *insn, rtx reg)
{
  return (dead_or_set_p (insn, reg)
	  || (REG_P (reg) && _reg_unused_after (insn, reg)));
}

/* Return nonzero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels.  It may live past calls or jumps though.  */

int
_reg_unused_after (rtx_insn *insn, rtx reg)
{
  enum rtx_code code;
  rtx set;

  /* If the reg is set by this instruction, then it is safe for our
     case.  Disregard the case where this is a store to memory, since
     we are checking a register used in the store address.  */
  set = single_set (insn);
  if (set && !MEM_P (SET_DEST (set))
      && reg_overlap_mentioned_p (reg, SET_DEST (set)))
    return 1;

  while ((insn = NEXT_INSN (insn)))
    {
      rtx set;
      code = GET_CODE (insn);

#if 0
      /* If this is a label that existed before reload, then the register
	 if dead here.  However, if this is a label added by reorg, then
	 the register may still be live here.  We can't tell the difference,
	 so we just ignore labels completely.  */
      if (code == CODE_LABEL)
	return 1;
      /* else */
#endif

      if (!INSN_P (insn))
	continue;

      if (code == JUMP_INSN)
	return 0;

      /* If this is a sequence, we must handle them all at once.
	 We could have for instance a call that sets the target register,
	 and an insn in a delay slot that uses the register.  In this case,
	 we must return 0.  */
      else if (code == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  rtx_sequence *seq = as_a <rtx_sequence *> (PATTERN (insn));
	  int retval = 0;

	  for (int i = 0; i < seq->len (); i++)
	    {
	      rtx_insn *this_insn = seq->insn (i);
	      rtx set = single_set (this_insn);

	      if (CALL_P (this_insn))
		code = CALL_INSN;
	      else if (JUMP_P (this_insn))
		{
		  if (INSN_ANNULLED_BRANCH_P (this_insn))
		    return 0;
		  code = JUMP_INSN;
		}

	      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
		return 0;
	      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
		{
		  if (!MEM_P (SET_DEST (set)))
		    retval = 1;
		  else
		    return 0;
		}
	      if (set == 0
		  && reg_overlap_mentioned_p (reg, PATTERN (this_insn)))
		return 0;
	    }
	  if (retval == 1)
	    return 1;
	  else if (code == JUMP_INSN)
	    return 0;
	}

      if (code == CALL_INSN)
	{
	  rtx tem;
	  for (tem = CALL_INSN_FUNCTION_USAGE (insn); tem; tem = XEXP (tem, 1))
	    if (GET_CODE (XEXP (tem, 0)) == USE
		&& REG_P (XEXP (XEXP (tem, 0), 0))
		&& reg_overlap_mentioned_p (reg, XEXP (XEXP (tem, 0), 0)))
	      return 0;
	  if (call_used_regs[REGNO (reg)])
	    return 1;
	}

      set = single_set (insn);

      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
	return 0;
      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	return !MEM_P (SET_DEST (set));
      if (set == 0 && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	return 0;
    }
  return 1;
}


/* Implement `TARGET_ASM_INTEGER'.  */
/* Target hook for assembling integer objects.  The AVR version needs
   special handling for references to certain labels.  */

static bool
avr_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  if (size == POINTER_SIZE / BITS_PER_UNIT && aligned_p
      && text_segment_operand (x, VOIDmode))
    {
      fputs ("\t.word\tgs(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")\n", asm_out_file);

      return true;
    }
  else if (GET_MODE (x) == PSImode)
    {
      /* This needs binutils 2.23+, see PR binutils/13503  */

      fputs ("\t.byte\tlo8(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")" ASM_COMMENT_START "need binutils PR13503\n", asm_out_file);

      fputs ("\t.byte\thi8(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")" ASM_COMMENT_START "need binutils PR13503\n", asm_out_file);

      fputs ("\t.byte\thh8(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")" ASM_COMMENT_START "need binutils PR13503\n", asm_out_file);

      return true;
    }
  else if (CONST_FIXED_P (x))
    {
      /* varasm fails to handle big fixed modes that don't fit in hwi.  */

      for (unsigned n = 0; n < size; n++)
        {
          rtx xn = simplify_gen_subreg (QImode, x, GET_MODE (x), n);
          default_assemble_integer (xn, 1, aligned_p);
        }

      return true;
    }

  if (AVR_TINY
      && avr_address_tiny_pm_p (x))
    {
      x = plus_constant (Pmode, x, avr_arch->flash_pm_offset);
    }

  return default_assemble_integer (x, size, aligned_p);
}


/* Implement `TARGET_CLASS_LIKELY_SPILLED_P'.  */
/* Return value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.  */

static bool
avr_class_likely_spilled_p (reg_class_t c)
{
  return (c != ALL_REGS &&
           (AVR_TINY ? 1 : c != ADDW_REGS));
}


/* Valid attributes:
   progmem   -  Put data to program memory.
   signal    -  Make a function to be hardware interrupt.
                After function prologue interrupts remain disabled.
   interrupt -  Make a function to be hardware interrupt. Before function
                prologue interrupts are enabled by means of SEI.
   naked     -  Don't generate function prologue/epilogue and RET
                instruction.  */

/* Handle a "progmem" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
avr_handle_progmem_attribute (tree *node, tree name,
			      tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED,
			      bool *no_add_attrs)
{
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) == TYPE_DECL)
	{
	  /* This is really a decl attribute, not a type attribute,
	     but try to handle it for GCC 3.0 backwards compatibility.  */

	  tree type = TREE_TYPE (*node);
	  tree attr = tree_cons (name, args, TYPE_ATTRIBUTES (type));
	  tree newtype = build_type_attribute_variant (type, attr);

	  TYPE_MAIN_VARIANT (newtype) = TYPE_MAIN_VARIANT (type);
	  TREE_TYPE (*node) = newtype;
	  *no_add_attrs = true;
	}
      else if (TREE_STATIC (*node) || DECL_EXTERNAL (*node))
	{
          *no_add_attrs = false;
	}
      else
	{
	  warning (OPT_Wattributes, "%qE attribute ignored",
		   name);
	  *no_add_attrs = true;
	}
    }

  return NULL_TREE;
}

/* Handle an attribute requiring a FUNCTION_DECL; arguments as in
   struct attribute_spec.handler.  */

static tree
avr_handle_fndecl_attribute (tree *node, tree name,
			     tree args ATTRIBUTE_UNUSED,
			     int flags ATTRIBUTE_UNUSED,
			     bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static tree
avr_handle_fntype_attribute (tree *node, tree name,
                             tree args ATTRIBUTE_UNUSED,
                             int flags ATTRIBUTE_UNUSED,
                             bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static tree
avr_handle_absdata_attribute (tree *node, tree name, tree /* args */,
                              int /* flags */, bool *no_add)
{
  location_t loc = DECL_SOURCE_LOCATION (*node);

  if (AVR_TINY)
    {
      if (TREE_CODE (*node) != VAR_DECL
          || (!TREE_STATIC (*node) && !DECL_EXTERNAL (*node)))
        {
          warning_at (loc, OPT_Wattributes, "%qE attribute only applies to"
                      " variables in static storage", name);
          *no_add = true;
        }
    }
  else
    {
      warning_at (loc, OPT_Wattributes, "%qE attribute only supported"
                  " for reduced Tiny cores", name);
      *no_add = true;
    }

  return NULL_TREE;
}

static tree
avr_handle_addr_attribute (tree *node, tree name, tree args,
			   int flags ATTRIBUTE_UNUSED, bool *no_add)
{
  bool io_p = (strncmp (IDENTIFIER_POINTER (name), "io", 2) == 0);
  location_t loc = DECL_SOURCE_LOCATION (*node);

  if (!VAR_P (*node))
    {
      warning_at (loc, OPT_Wattributes, "%qE attribute only applies to "
		  "variables", name);
      *no_add = true;
      return NULL_TREE;
    }

  if (args != NULL_TREE)
    {
      if (TREE_CODE (TREE_VALUE (args)) == NON_LVALUE_EXPR)
	TREE_VALUE (args) = TREE_OPERAND (TREE_VALUE (args), 0);
      tree arg = TREE_VALUE (args);
      if (TREE_CODE (arg) != INTEGER_CST)
	{
	  warning_at (loc, OPT_Wattributes, "%qE attribute allows only an "
		      "integer constant argument", name);
	  *no_add = true;
	}
      else if (io_p
	       && (!tree_fits_shwi_p (arg)
		   || !(strcmp (IDENTIFIER_POINTER (name), "io_low") == 0
			? low_io_address_operand : io_address_operand)
			 (GEN_INT (TREE_INT_CST_LOW (arg)), QImode)))
	{
	  warning_at (loc, OPT_Wattributes, "%qE attribute address "
		      "out of range", name);
	  *no_add = true;
	}
      else
	{
	  tree attribs = DECL_ATTRIBUTES (*node);
	  const char *names[] = { "io", "io_low", "address", NULL };
	  for (const char **p = names; *p; p++)
	    {
	      tree other = lookup_attribute (*p, attribs);
	      if (other && TREE_VALUE (other))
		{
		  warning_at (loc, OPT_Wattributes,
			      "both %s and %qE attribute provide address",
			      *p, name);
		  *no_add = true;
		  break;
		}
	    }
	}
    }

  if (*no_add == false && io_p && !TREE_THIS_VOLATILE (*node))
    warning_at (loc, OPT_Wattributes, "%qE attribute on non-volatile variable",
		name);

  return NULL_TREE;
}

rtx
avr_eval_addr_attrib (rtx x)
{
  if (SYMBOL_REF_P (x)
      && (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_ADDRESS))
    {
      tree decl = SYMBOL_REF_DECL (x);
      tree attr = NULL_TREE;

      if (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_IO)
	{
	  attr = lookup_attribute ("io", DECL_ATTRIBUTES (decl));
          if (!attr || !TREE_VALUE (attr))
            attr = lookup_attribute ("io_low", DECL_ATTRIBUTES (decl));
	  gcc_assert (attr);
	}
      if (!attr || !TREE_VALUE (attr))
	attr = lookup_attribute ("address", DECL_ATTRIBUTES (decl));
      gcc_assert (attr && TREE_VALUE (attr) && TREE_VALUE (TREE_VALUE (attr)));
      return GEN_INT (TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr))));
    }
  return x;
}


/* AVR attributes.  */
static const struct attribute_spec
avr_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  { "progmem",   0, 0, false, false, false,  avr_handle_progmem_attribute,
    false },
  { "signal",    0, 0, true,  false, false,  avr_handle_fndecl_attribute,
    false },
  { "interrupt", 0, 0, true,  false, false,  avr_handle_fndecl_attribute,
    false },
  { "no_gccisr", 0, 0, true,  false, false,  avr_handle_fndecl_attribute,
    false },
  { "naked",     0, 0, false, true,  true,   avr_handle_fntype_attribute,
    false },
  { "OS_task",   0, 0, false, true,  true,   avr_handle_fntype_attribute,
    false },
  { "OS_main",   0, 0, false, true,  true,   avr_handle_fntype_attribute,
    false },
  { "io",        0, 1, true, false, false,  avr_handle_addr_attribute,
    false },
  { "io_low",    0, 1, true, false, false,  avr_handle_addr_attribute,
    false },
  { "address",   1, 1, true, false, false,  avr_handle_addr_attribute,
    false },
  { "absdata",   0, 0, true, false, false,  avr_handle_absdata_attribute,
    false },
  { NULL,        0, 0, false, false, false, NULL, false }
};


/* Return true if we support address space AS for the architecture in effect
   and false, otherwise.  If LOC is not UNKNOWN_LOCATION then also issue
   a respective error.  */

bool
avr_addr_space_supported_p (addr_space_t as, location_t loc)
{
  if (AVR_TINY)
    {
      if (loc != UNKNOWN_LOCATION)
        error_at (loc, "address spaces are not supported for reduced "
                  "Tiny devices");
      return false;
    }
  else if (avr_addrspace[as].segment >= avr_n_flash)
    {
      if (loc != UNKNOWN_LOCATION)
        error_at (loc, "address space %qs not supported for devices with "
                  "flash size up to %d KiB", avr_addrspace[as].name,
                  64 * avr_n_flash);
      return false;
    }

  return true;
}


/* Implement `TARGET_ADDR_SPACE_DIAGNOSE_USAGE'.  */

static void
avr_addr_space_diagnose_usage (addr_space_t as, location_t loc)
{
  (void) avr_addr_space_supported_p (as, loc);
}


/* Look if DECL shall be placed in program memory space by
   means of attribute `progmem' or some address-space qualifier.
   Return non-zero if DECL is data that must end up in Flash and
   zero if the data lives in RAM (.bss, .data, .rodata, ...).

   Return 2   if DECL is located in 24-bit flash address-space
   Return 1   if DECL is located in 16-bit flash address-space
   Return -1  if attribute `progmem' occurs in DECL or ATTRIBUTES
   Return 0   otherwise  */

int
avr_progmem_p (tree decl, tree attributes)
{
  tree a;

  if (TREE_CODE (decl) != VAR_DECL)
    return 0;

  if (avr_decl_memx_p (decl))
    return 2;

  if (avr_decl_flash_p (decl))
    return 1;

  if (NULL_TREE
      != lookup_attribute ("progmem", attributes))
    return -1;

  a = decl;

  do
    a = TREE_TYPE(a);
  while (TREE_CODE (a) == ARRAY_TYPE);

  if (a == error_mark_node)
    return 0;

  if (NULL_TREE != lookup_attribute ("progmem", TYPE_ATTRIBUTES (a)))
    return -1;

  return 0;
}


/* Return true if DECL has attribute `absdata' set.  This function should
   only be used for AVR_TINY.  */

static bool
avr_decl_absdata_p (tree decl, tree attributes)
{
  return (TREE_CODE (decl) == VAR_DECL
          && NULL_TREE != lookup_attribute ("absdata", attributes));
}


/* Scan type TYP for pointer references to address space ASn.
   Return ADDR_SPACE_GENERIC (i.e. 0) if all pointers targeting
   the AS are also declared to be CONST.
   Otherwise, return the respective address space, i.e. a value != 0.  */

static addr_space_t
avr_nonconst_pointer_addrspace (tree typ)
{
  while (ARRAY_TYPE == TREE_CODE (typ))
    typ = TREE_TYPE (typ);

  if (POINTER_TYPE_P (typ))
    {
      addr_space_t as;
      tree target = TREE_TYPE (typ);

      /* Pointer to function: Test the function's return type.  */

      if (FUNCTION_TYPE == TREE_CODE (target))
        return avr_nonconst_pointer_addrspace (TREE_TYPE (target));

      /* "Ordinary" pointers... */

      while (TREE_CODE (target) == ARRAY_TYPE)
        target = TREE_TYPE (target);

      /* Pointers to non-generic address space must be const.  */

      as = TYPE_ADDR_SPACE (target);

      if (!ADDR_SPACE_GENERIC_P (as)
          && !TYPE_READONLY (target)
          && avr_addr_space_supported_p (as))
        {
          return as;
        }

      /* Scan pointer's target type.  */

      return avr_nonconst_pointer_addrspace (target);
    }

  return ADDR_SPACE_GENERIC;
}


/* Sanity check NODE so that all pointers targeting non-generic address spaces
   go along with CONST qualifier.  Writing to these address spaces should
   be detected and complained about as early as possible.  */

static bool
avr_pgm_check_var_decl (tree node)
{
  const char *reason = NULL;

  addr_space_t as = ADDR_SPACE_GENERIC;

  gcc_assert (as == 0);

  if (avr_log.progmem)
    avr_edump ("%?: %t\n", node);

  switch (TREE_CODE (node))
    {
    default:
      break;

    case VAR_DECL:
      if (as = avr_nonconst_pointer_addrspace (TREE_TYPE (node)), as)
        reason = _("variable");
      break;

    case PARM_DECL:
      if (as = avr_nonconst_pointer_addrspace (TREE_TYPE (node)), as)
        reason = _("function parameter");
      break;

    case FIELD_DECL:
      if (as = avr_nonconst_pointer_addrspace (TREE_TYPE (node)), as)
        reason = _("structure field");
      break;

    case FUNCTION_DECL:
      if (as = avr_nonconst_pointer_addrspace (TREE_TYPE (TREE_TYPE (node))),
          as)
        reason = _("return type of function");
      break;

    case POINTER_TYPE:
      if (as = avr_nonconst_pointer_addrspace (node), as)
        reason = _("pointer");
      break;
    }

  if (reason)
    {
      if (TYPE_P (node))
        error ("pointer targeting address space %qs must be const in %qT",
               avr_addrspace[as].name, node);
      else
        error ("pointer targeting address space %qs must be const"
               " in %s %q+D",
               avr_addrspace[as].name, reason, node);
    }

  return reason == NULL;
}


/* Add the section attribute if the variable is in progmem.  */

static void
avr_insert_attributes (tree node, tree *attributes)
{
  avr_pgm_check_var_decl (node);

  if (TREE_CODE (node) == VAR_DECL
      && (TREE_STATIC (node) || DECL_EXTERNAL (node))
      && avr_progmem_p (node, *attributes))
    {
      addr_space_t as;
      tree node0 = node;

      /* For C++, we have to peel arrays in order to get correct
         determination of readonlyness.  */

      do
        node0 = TREE_TYPE (node0);
      while (TREE_CODE (node0) == ARRAY_TYPE);

      if (error_mark_node == node0)
        return;

      as = TYPE_ADDR_SPACE (TREE_TYPE (node));

      if (!TYPE_READONLY (node0)
          && !TREE_READONLY (node))
        {
          const char *reason = "__attribute__((progmem))";

          if (!ADDR_SPACE_GENERIC_P (as))
            reason = avr_addrspace[as].name;

          if (avr_log.progmem)
            avr_edump ("\n%?: %t\n%t\n", node, node0);

          error ("variable %q+D must be const in order to be put into"
                 " read-only section by means of %qs", node, reason);
        }
    }
}


/* Implement `ASM_OUTPUT_ALIGNED_DECL_LOCAL'.  */
/* Implement `ASM_OUTPUT_ALIGNED_DECL_COMMON'.  */
/* Track need of __do_clear_bss.  */

void
avr_asm_output_aligned_decl_common (FILE * stream,
                                    tree decl,
                                    const char *name,
                                    unsigned HOST_WIDE_INT size,
                                    unsigned int align, bool local_p)
{
  rtx mem = decl == NULL_TREE ? NULL_RTX : DECL_RTL (decl);
  rtx symbol;

  if (mem != NULL_RTX && MEM_P (mem)
      && SYMBOL_REF_P ((symbol = XEXP (mem, 0)))
      && (SYMBOL_REF_FLAGS (symbol) & (SYMBOL_FLAG_IO | SYMBOL_FLAG_ADDRESS)))
    {
      if (!local_p)
	{
	  fprintf (stream, "\t.globl\t");
	  assemble_name (stream, name);
	  fprintf (stream, "\n");
	}
      if (SYMBOL_REF_FLAGS (symbol) & SYMBOL_FLAG_ADDRESS)
	{
	  assemble_name (stream, name);
	  fprintf (stream, " = %ld\n",
		   (long) INTVAL (avr_eval_addr_attrib (symbol)));
	}
      else if (local_p)
	error_at (DECL_SOURCE_LOCATION (decl),
		  "static IO declaration for %q+D needs an address", decl);
      return;
    }

  /* __gnu_lto_v1 etc. are just markers for the linker injected by toplev.c.
     There is no need to trigger __do_clear_bss code for them.  */

  if (!STR_PREFIX_P (name, "__gnu_lto"))
    avr_need_clear_bss_p = true;

  if (local_p)
    ASM_OUTPUT_ALIGNED_LOCAL (stream, name, size, align);
  else
    ASM_OUTPUT_ALIGNED_COMMON (stream, name, size, align);
}

void
avr_asm_asm_output_aligned_bss (FILE *file, tree decl, const char *name,
				unsigned HOST_WIDE_INT size, int align,
				void (*default_func)
				  (FILE *, tree, const char *,
				   unsigned HOST_WIDE_INT, int))
{
  rtx mem = decl == NULL_TREE ? NULL_RTX : DECL_RTL (decl);
  rtx symbol;

  if (mem != NULL_RTX && MEM_P (mem)
      && SYMBOL_REF_P ((symbol = XEXP (mem, 0)))
      && (SYMBOL_REF_FLAGS (symbol) & (SYMBOL_FLAG_IO | SYMBOL_FLAG_ADDRESS)))
    {
      if (!(SYMBOL_REF_FLAGS (symbol) & SYMBOL_FLAG_ADDRESS))
	error_at (DECL_SOURCE_LOCATION (decl),
		  "IO definition for %q+D needs an address", decl);
      avr_asm_output_aligned_decl_common (file, decl, name, size, align, false);
    }
  else
    default_func (file, decl, name, size, align);
}


/* Unnamed section callback for data_section
   to track need of __do_copy_data.  */

static void
avr_output_data_section_asm_op (const void *data)
{
  avr_need_copy_data_p = true;

  /* Dispatch to default.  */
  output_section_asm_op (data);
}


/* Unnamed section callback for bss_section
   to track need of __do_clear_bss.  */

static void
avr_output_bss_section_asm_op (const void *data)
{
  avr_need_clear_bss_p = true;

  /* Dispatch to default.  */
  output_section_asm_op (data);
}


/* Unnamed section callback for progmem*.data sections.  */

static void
avr_output_progmem_section_asm_op (const void *data)
{
  fprintf (asm_out_file, "\t.section\t%s,\"a\",@progbits\n",
           (const char*) data);
}


/* Implement `TARGET_ASM_INIT_SECTIONS'.  */

static void
avr_asm_init_sections (void)
{
  /* Override section callbacks to keep track of `avr_need_clear_bss_p'
     resp. `avr_need_copy_data_p'.  If flash is not mapped to RAM then
     we have also to track .rodata because it is located in RAM then.  */

#if defined HAVE_LD_AVR_AVRXMEGA3_RODATA_IN_FLASH
  if (0 == avr_arch->flash_pm_offset)
#endif
    readonly_data_section->unnamed.callback = avr_output_data_section_asm_op;
  data_section->unnamed.callback = avr_output_data_section_asm_op;
  bss_section->unnamed.callback = avr_output_bss_section_asm_op;
}


/* Implement `TARGET_ASM_NAMED_SECTION'.  */
/* Track need of __do_clear_bss, __do_copy_data for named sections.  */

static void
avr_asm_named_section (const char *name, unsigned int flags, tree decl)
{
  if (flags & AVR_SECTION_PROGMEM)
    {
      addr_space_t as = (flags & AVR_SECTION_PROGMEM) / SECTION_MACH_DEP;
      const char *old_prefix = ".rodata";
      const char *new_prefix = avr_addrspace[as].section_name;

      if (STR_PREFIX_P (name, old_prefix))
        {
          const char *sname = ACONCAT ((new_prefix,
                                        name + strlen (old_prefix), NULL));
          default_elf_asm_named_section (sname, flags, decl);
          return;
        }

      default_elf_asm_named_section (new_prefix, flags, decl);
      return;
    }

  if (!avr_need_copy_data_p)
    avr_need_copy_data_p = (STR_PREFIX_P (name, ".data")
                            || STR_PREFIX_P (name, ".gnu.linkonce.d"));

  if (!avr_need_copy_data_p
#if defined HAVE_LD_AVR_AVRXMEGA3_RODATA_IN_FLASH
      && 0 == avr_arch->flash_pm_offset
#endif
      )
    avr_need_copy_data_p = (STR_PREFIX_P (name, ".rodata")
                            || STR_PREFIX_P (name, ".gnu.linkonce.r"));

  if (!avr_need_clear_bss_p)
    avr_need_clear_bss_p = STR_PREFIX_P (name, ".bss");

  default_elf_asm_named_section (name, flags, decl);
}


/* Implement `TARGET_SECTION_TYPE_FLAGS'.  */

static unsigned int
avr_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags = default_section_type_flags (decl, name, reloc);

  if (STR_PREFIX_P (name, ".noinit"))
    {
      if (decl && TREE_CODE (decl) == VAR_DECL
	  && DECL_INITIAL (decl) == NULL_TREE)
	flags |= SECTION_BSS;  /* @nobits */
      else
	warning (0, "only uninitialized variables can be placed in the "
		 ".noinit section");
    }

  if (decl && DECL_P (decl)
      && avr_progmem_p (decl, DECL_ATTRIBUTES (decl)))
    {
      addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (decl));

      /* Attribute progmem puts data in generic address space.
         Set section flags as if it was in __flash to get the right
         section prefix in the remainder.  */

      if (ADDR_SPACE_GENERIC_P (as))
        as = ADDR_SPACE_FLASH;

      flags |= as * SECTION_MACH_DEP;
      flags &= ~SECTION_WRITE;
      flags &= ~SECTION_BSS;
    }

  return flags;
}


/* A helper for the next function.  NODE is a decl that is associated with
   a symbol.  Return TRUE if the respective object may be accessed by LDS.
   There might still be other reasons for why LDS is not appropriate.
   This function is only appropriate for AVR_TINY.  */

static bool
avr_decl_maybe_lds_p (tree node)
{
  if (!node
      || TREE_CODE (node) != VAR_DECL
      || DECL_SECTION_NAME (node) != NULL)
    return false;

  /* Don't use LDS for objects that go to .rodata.  The current default
     linker description file still locates .rodata in RAM, but this is not
     a must.  A better linker script would just keep .rodata in flash and
     add an offset of 0x4000 to the VMA.  Hence avoid LDS for such data.  */

  if (TREE_READONLY (node))
    return false;

  // C++ requires peeling arrays.

  do
    node = TREE_TYPE (node);
  while (ARRAY_TYPE == TREE_CODE (node));

  return (node != error_mark_node
          && !TYPE_READONLY (node));
}


/* Implement `TARGET_ENCODE_SECTION_INFO'.  */

static void
avr_encode_section_info (tree decl, rtx rtl, int new_decl_p)
{
  tree addr_attr = NULL_TREE;

  /* In avr_handle_progmem_attribute, DECL_INITIAL is not yet
     readily available, see PR34734.  So we postpone the warning
     about uninitialized data in program memory section until here.  */

  if (new_decl_p
      && decl && DECL_P (decl)
      && !DECL_EXTERNAL (decl)
      && avr_progmem_p (decl, DECL_ATTRIBUTES (decl)))
    {
      if (!TREE_READONLY (decl))
        {
          // This might happen with C++ if stuff needs constructing.
          error ("variable %q+D with dynamic initialization put "
                 "into program memory area", decl);
        }
      else if (NULL_TREE == DECL_INITIAL (decl))
        {
          // Don't warn for (implicit) aliases like in PR80462.
          tree asmname = DECL_ASSEMBLER_NAME (decl);
          varpool_node *node = varpool_node::get_for_asmname (asmname);
          bool alias_p = node && node->alias;

          if (!alias_p)
            warning (OPT_Wuninitialized, "uninitialized variable %q+D put "
                     "into program memory area", decl);
        }
    }

  default_encode_section_info (decl, rtl, new_decl_p);

  if (decl && DECL_P (decl)
      && TREE_CODE (decl) != FUNCTION_DECL
      && MEM_P (rtl)
      && SYMBOL_REF_P (XEXP (rtl, 0)))
    {
      rtx sym = XEXP (rtl, 0);
      tree type = TREE_TYPE (decl);
      tree attr = DECL_ATTRIBUTES (decl);
      if (type == error_mark_node)
	return;

      addr_space_t as = TYPE_ADDR_SPACE (type);

      /* PSTR strings are in generic space but located in flash:
         patch address space.  */

      if (!AVR_TINY
          && -1 == avr_progmem_p (decl, attr))
        as = ADDR_SPACE_FLASH;

      AVR_SYMBOL_SET_ADDR_SPACE (sym, as);

      tree io_low_attr = lookup_attribute ("io_low", attr);
      tree io_attr = lookup_attribute ("io", attr);

      if (io_low_attr
	  && TREE_VALUE (io_low_attr) && TREE_VALUE (TREE_VALUE (io_low_attr)))
	addr_attr = io_attr;
      else if (io_attr
	       && TREE_VALUE (io_attr) && TREE_VALUE (TREE_VALUE (io_attr)))
	addr_attr = io_attr;
      else
	addr_attr = lookup_attribute ("address", attr);
      if (io_low_attr
	  || (io_attr && addr_attr
              && low_io_address_operand
                  (GEN_INT (TREE_INT_CST_LOW
                            (TREE_VALUE (TREE_VALUE (addr_attr)))), QImode)))
	SYMBOL_REF_FLAGS (sym) |= SYMBOL_FLAG_IO_LOW;
      if (io_attr || io_low_attr)
	SYMBOL_REF_FLAGS (sym) |= SYMBOL_FLAG_IO;
      /* If we have an (io) address attribute specification, but the variable
	 is external, treat the address as only a tentative definition
	 to be used to determine if an io port is in the lower range, but
	 don't use the exact value for constant propagation.  */
      if (addr_attr && !DECL_EXTERNAL (decl))
	SYMBOL_REF_FLAGS (sym) |= SYMBOL_FLAG_ADDRESS;
    }

  if (AVR_TINY
      && decl
      && VAR_DECL == TREE_CODE (decl)
      && MEM_P (rtl)
      && SYMBOL_REF_P (XEXP (rtl, 0)))
    {
      rtx sym = XEXP (rtl, 0);
      bool progmem_p = -1 == avr_progmem_p (decl, DECL_ATTRIBUTES (decl));

      if (progmem_p)
        {
          // Tag symbols for addition of 0x4000 (avr_arch->flash_pm_offset).
          SYMBOL_REF_FLAGS (sym) |= AVR_SYMBOL_FLAG_TINY_PM;
        }

      if (avr_decl_absdata_p (decl, DECL_ATTRIBUTES (decl))
          || (TARGET_ABSDATA
              && !progmem_p
              && !addr_attr
              && avr_decl_maybe_lds_p (decl))
          || (addr_attr
              // If addr_attr is non-null, it has an argument.  Peek into it.
              && TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (addr_attr))) < 0xc0))
        {
          // May be accessed by LDS / STS.
          SYMBOL_REF_FLAGS (sym) |= AVR_SYMBOL_FLAG_TINY_ABSDATA;
        }

      if (progmem_p
          && avr_decl_absdata_p (decl, DECL_ATTRIBUTES (decl)))
        {
          error ("%q+D has incompatible attributes %qs and %qs",
                 decl, "progmem", "absdata");
        }
    }
}


/* Implement `TARGET_ASM_SELECT_SECTION' */

static section *
avr_asm_select_section (tree decl, int reloc, unsigned HOST_WIDE_INT align)
{
  section * sect = default_elf_select_section (decl, reloc, align);

  if (decl && DECL_P (decl)
      && avr_progmem_p (decl, DECL_ATTRIBUTES (decl)))
    {
      addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (decl));

      /* __progmem__ goes in generic space but shall be allocated to
         .progmem.data  */

      if (ADDR_SPACE_GENERIC_P (as))
        as = ADDR_SPACE_FLASH;

      if (sect->common.flags & SECTION_NAMED)
        {
          const char * name = sect->named.name;
          const char * old_prefix = ".rodata";
          const char * new_prefix = avr_addrspace[as].section_name;

          if (STR_PREFIX_P (name, old_prefix))
            {
              const char *sname = ACONCAT ((new_prefix,
                                            name + strlen (old_prefix), NULL));
              return get_section (sname,
                                  sect->common.flags & ~SECTION_DECLARED,
                                  sect->named.decl);
            }
        }

      if (!progmem_section[as])
        {
          progmem_section[as]
            = get_unnamed_section (0, avr_output_progmem_section_asm_op,
                                   avr_addrspace[as].section_name);
        }

      return progmem_section[as];
    }

  return sect;
}

/* Implement `TARGET_ASM_FILE_START'.  */
/* Outputs some text at the start of each assembler file.  */

static void
avr_file_start (void)
{
  int sfr_offset = avr_arch->sfr_offset;

  if (avr_arch->asm_only)
    error ("architecture %qs supported for assembler only", avr_mmcu);

  default_file_start ();

  /* Print I/O addresses of some SFRs used with IN and OUT.  */

  if (AVR_HAVE_SPH)
    fprintf (asm_out_file, "__SP_H__ = 0x%02x\n", avr_addr.sp_h - sfr_offset);

  fprintf (asm_out_file, "__SP_L__ = 0x%02x\n", avr_addr.sp_l - sfr_offset);
  fprintf (asm_out_file, "__SREG__ = 0x%02x\n", avr_addr.sreg - sfr_offset);
  if (AVR_HAVE_RAMPZ)
    fprintf (asm_out_file, "__RAMPZ__ = 0x%02x\n", avr_addr.rampz - sfr_offset);
  if (AVR_HAVE_RAMPY)
    fprintf (asm_out_file, "__RAMPY__ = 0x%02x\n", avr_addr.rampy - sfr_offset);
  if (AVR_HAVE_RAMPX)
    fprintf (asm_out_file, "__RAMPX__ = 0x%02x\n", avr_addr.rampx - sfr_offset);
  if (AVR_HAVE_RAMPD)
    fprintf (asm_out_file, "__RAMPD__ = 0x%02x\n", avr_addr.rampd - sfr_offset);
  if (AVR_XMEGA || AVR_TINY)
    fprintf (asm_out_file, "__CCP__ = 0x%02x\n", avr_addr.ccp - sfr_offset);
  fprintf (asm_out_file, "__tmp_reg__ = %d\n", AVR_TMP_REGNO);
  fprintf (asm_out_file, "__zero_reg__ = %d\n", AVR_ZERO_REGNO);
}


/* Implement `TARGET_ASM_FILE_END'.  */
/* Outputs to the stdio stream FILE some
   appropriate text to go at the end of an assembler file.  */

static void
avr_file_end (void)
{
  /* Output these only if there is anything in the
     .data* / .rodata* / .gnu.linkonce.* resp. .bss* or COMMON
     input section(s) - some code size can be saved by not
     linking in the initialization code from libgcc if resp.
     sections are empty, see PR18145.  */

  if (avr_need_copy_data_p)
    fputs (".global __do_copy_data\n", asm_out_file);

  if (avr_need_clear_bss_p)
    fputs (".global __do_clear_bss\n", asm_out_file);
}


/* Worker function for `ADJUST_REG_ALLOC_ORDER'.  */
/* Choose the order in which to allocate hard registers for
   pseudo-registers local to a basic block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.  */

void
avr_adjust_reg_alloc_order (void)
{
  static const int order_0[] =
    {
      24, 25,
      18, 19, 20, 21, 22, 23,
      30, 31,
      26, 27, 28, 29,
      17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2,
      0, 1,
      32, 33, 34, 35
    };
  static const int tiny_order_0[] = {
    20, 21,
    22, 23,
    24, 25,
    30, 31,
    26, 27,
    28, 29,
    19, 18,
    16, 17,
    32, 33, 34, 35,
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
  };
  static const int order_1[] =
    {
      18, 19, 20, 21, 22, 23, 24, 25,
      30, 31,
      26, 27, 28, 29,
      17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2,
      0, 1,
      32, 33, 34, 35
    };
  static const int tiny_order_1[] = {
    22, 23,
    24, 25,
    30, 31,
    26, 27,
    28, 29,
    21, 20, 19, 18,
    16, 17,
    32, 33, 34, 35,
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
  };
  static const int order_2[] =
    {
      25, 24, 23, 22, 21, 20, 19, 18,
      30, 31,
      26, 27, 28, 29,
      17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2,
      1, 0,
      32, 33, 34, 35
    };

  /* Select specific register allocation order.
     Tiny Core (ATtiny4/5/9/10/20/40) devices have only 16 registers,
     so different allocation order should be used.  */

  const int *order = (TARGET_ORDER_1 ? (AVR_TINY ? tiny_order_1 : order_1)
                      : TARGET_ORDER_2 ? (AVR_TINY ? tiny_order_0 : order_2)
                      : (AVR_TINY ? tiny_order_0 : order_0));

  for (size_t i = 0; i < ARRAY_SIZE (order_0); ++i)
    reg_alloc_order[i] = order[i];
}


/* Implement `TARGET_REGISTER_MOVE_COST' */

static int
avr_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
                        reg_class_t from, reg_class_t to)
{
  return (from == STACK_REG ? 6
          : to == STACK_REG ? 12
          : 2);
}


/* Implement `TARGET_MEMORY_MOVE_COST' */

static int
avr_memory_move_cost (machine_mode mode,
                      reg_class_t rclass ATTRIBUTE_UNUSED,
                      bool in ATTRIBUTE_UNUSED)
{
  return (mode == QImode ? 2
          : mode == HImode ? 4
          : mode == SImode ? 8
          : mode == SFmode ? 8
          : 16);
}


/* Cost for mul highpart.  X is a LSHIFTRT, i.e. the outer TRUNCATE is
   already stripped off.  */

static int
avr_mul_highpart_cost (rtx x, int)
{
  if (AVR_HAVE_MUL
      && LSHIFTRT == GET_CODE (x)
      && MULT == GET_CODE (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1)))
    {
      // This is the wider mode.
      machine_mode mode = GET_MODE (x);
  
      // The middle-end might still have PR81444, i.e. it is calling the cost
      // functions with strange modes.  Fix this now by also considering
      // PSImode (should actually be SImode instead).
      if (HImode == mode || PSImode == mode || SImode == mode)
        {
          return COSTS_N_INSNS (2);
        }
    }

  return 10000;
}


/* Mutually recursive subroutine of avr_rtx_cost for calculating the
   cost of an RTX operand given its context.  X is the rtx of the
   operand, MODE is its mode, and OUTER is the rtx_code of this
   operand's parent operator.  */

static int
avr_operand_rtx_cost (rtx x, machine_mode mode, enum rtx_code outer,
		      int opno, bool speed)
{
  enum rtx_code code = GET_CODE (x);
  int total;

  switch (code)
    {
    case REG:
    case SUBREG:
      return 0;

    case CONST_INT:
    case CONST_FIXED:
    case CONST_DOUBLE:
      return COSTS_N_INSNS (GET_MODE_SIZE (mode));

    default:
      break;
    }

  total = 0;
  avr_rtx_costs (x, mode, outer, opno, &total, speed);
  return total;
}

/* Worker function for AVR backend's rtx_cost function.
   X is rtx expression whose cost is to be calculated.
   Return true if the complete cost has been computed.
   Return false if subexpressions should be scanned.
   In either case, *TOTAL contains the cost result.  */

static bool
avr_rtx_costs_1 (rtx x, machine_mode mode, int outer_code,
                 int opno ATTRIBUTE_UNUSED, int *total, bool speed)
{
  enum rtx_code code = GET_CODE (x);
  HOST_WIDE_INT val;

  switch (code)
    {
    case CONST_INT:
    case CONST_FIXED:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      /* Immediate constants are as cheap as registers.  */
      *total = 0;
      return true;

    case MEM:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode));
      return true;

    case NEG:
      switch (mode)
	{
	case E_QImode:
	case E_SFmode:
	  *total = COSTS_N_INSNS (1);
	  break;

        case E_HImode:
        case E_PSImode:
        case E_SImode:
          *total = COSTS_N_INSNS (2 * GET_MODE_SIZE (mode) - 1);
          break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case ABS:
      switch (mode)
	{
	case E_QImode:
	case E_SFmode:
	  *total = COSTS_N_INSNS (1);
	  break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case NOT:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode));
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case ZERO_EXTEND:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode)
			      - GET_MODE_SIZE (GET_MODE (XEXP (x, 0))));
      *total += avr_operand_rtx_cost (XEXP (x, 0), GET_MODE (XEXP (x, 0)),
				      code, 0, speed);
      return true;

    case SIGN_EXTEND:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode) + 2
			      - GET_MODE_SIZE (GET_MODE (XEXP (x, 0))));
      *total += avr_operand_rtx_cost (XEXP (x, 0), GET_MODE (XEXP (x, 0)),
				      code, 0, speed);
      return true;

    case PLUS:
      switch (mode)
	{
	case E_QImode:
          if (AVR_HAVE_MUL
              && MULT == GET_CODE (XEXP (x, 0))
              && register_operand (XEXP (x, 1), QImode))
            {
              /* multiply-add */
              *total = COSTS_N_INSNS (speed ? 4 : 3);
              /* multiply-add with constant: will be split and load constant. */
              if (CONST_INT_P (XEXP (XEXP (x, 0), 1)))
                *total = COSTS_N_INSNS (1) + *total;
              return true;
            }
	  *total = COSTS_N_INSNS (1);
	  if (!CONST_INT_P (XEXP (x, 1)))
	    *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	  break;

	case E_HImode:
          if (AVR_HAVE_MUL
              && (MULT == GET_CODE (XEXP (x, 0))
                  || ASHIFT == GET_CODE (XEXP (x, 0)))
              && register_operand (XEXP (x, 1), HImode)
              && (ZERO_EXTEND == GET_CODE (XEXP (XEXP (x, 0), 0))
                  || SIGN_EXTEND == GET_CODE (XEXP (XEXP (x, 0), 0))))
            {
              /* multiply-add */
              *total = COSTS_N_INSNS (speed ? 5 : 4);
              /* multiply-add with constant: will be split and load constant. */
              if (CONST_INT_P (XEXP (XEXP (x, 0), 1)))
                *total = COSTS_N_INSNS (1) + *total;
              return true;
            }
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (2);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else if (IN_RANGE (INTVAL (XEXP (x, 1)), -63, 63))
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (2);
	  break;

        case E_PSImode:
          if (!CONST_INT_P (XEXP (x, 1)))
            {
              *total = COSTS_N_INSNS (3);
              *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
                                              speed);
            }
          else if (IN_RANGE (INTVAL (XEXP (x, 1)), -63, 63))
            *total = COSTS_N_INSNS (2);
          else
            *total = COSTS_N_INSNS (3);
          break;

	case E_SImode:
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (4);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else if (IN_RANGE (INTVAL (XEXP (x, 1)), -63, 63))
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (4);
	  break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case MINUS:
      if (AVR_HAVE_MUL
          && QImode == mode
          && register_operand (XEXP (x, 0), QImode)
          && MULT == GET_CODE (XEXP (x, 1)))
        {
          /* multiply-sub */
          *total = COSTS_N_INSNS (speed ? 4 : 3);
          /* multiply-sub with constant: will be split and load constant. */
          if (CONST_INT_P (XEXP (XEXP (x, 1), 1)))
            *total = COSTS_N_INSNS (1) + *total;
          return true;
        }
      if (AVR_HAVE_MUL
          && HImode == mode
          && register_operand (XEXP (x, 0), HImode)
          && (MULT == GET_CODE (XEXP (x, 1))
              || ASHIFT == GET_CODE (XEXP (x, 1)))
          && (ZERO_EXTEND == GET_CODE (XEXP (XEXP (x, 1), 0))
              || SIGN_EXTEND == GET_CODE (XEXP (XEXP (x, 1), 0))))
        {
          /* multiply-sub */
          *total = COSTS_N_INSNS (speed ? 5 : 4);
          /* multiply-sub with constant: will be split and load constant. */
          if (CONST_INT_P (XEXP (XEXP (x, 1), 1)))
            *total = COSTS_N_INSNS (1) + *total;
          return true;
        }
      /* FALLTHRU */
    case AND:
    case IOR:
      if (IOR == code
          && HImode == mode
          && ASHIFT == GET_CODE (XEXP (x, 0)))
        {
          *total = COSTS_N_INSNS (2);
          // Just a rough estimate.  If we see no sign- or zero-extend,
          // then increase the cost a little bit.
          if (REG_P (XEXP (XEXP (x, 0), 0)))
            *total += COSTS_N_INSNS (1);
          if (REG_P (XEXP (x, 1)))
            *total += COSTS_N_INSNS (1);
          return true;
        }
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode));
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      if (!CONST_INT_P (XEXP (x, 1)))
	*total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
      return true;

    case XOR:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode));
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
      return true;

    case MULT:
      switch (mode)
	{
	case E_QImode:
	  if (AVR_HAVE_MUL)
	    *total = COSTS_N_INSNS (!speed ? 3 : 4);
	  else if (!speed)
	    *total = COSTS_N_INSNS (AVR_HAVE_JMP_CALL ? 2 : 1);
	  else
	    return false;
	  break;

	case E_HImode:
	  if (AVR_HAVE_MUL)
            {
              rtx op0 = XEXP (x, 0);
              rtx op1 = XEXP (x, 1);
              enum rtx_code code0 = GET_CODE (op0);
              enum rtx_code code1 = GET_CODE (op1);
              bool ex0 = SIGN_EXTEND == code0 || ZERO_EXTEND == code0;
              bool ex1 = SIGN_EXTEND == code1 || ZERO_EXTEND == code1;

              if (ex0
                  && (u8_operand (op1, HImode)
                      || s8_operand (op1, HImode)))
                {
                  *total = COSTS_N_INSNS (!speed ? 4 : 6);
                  return true;
                }
              if (ex0
                  && register_operand (op1, HImode))
                {
                  *total = COSTS_N_INSNS (!speed ? 5 : 8);
                  return true;
                }
              else if (ex0 || ex1)
                {
                  *total = COSTS_N_INSNS (!speed ? 3 : 5);
                  return true;
                }
              else if (register_operand (op0, HImode)
                       && (u8_operand (op1, HImode)
                           || s8_operand (op1, HImode)))
                {
                  *total = COSTS_N_INSNS (!speed ? 6 : 9);
                  return true;
                }
              else
                *total = COSTS_N_INSNS (!speed ? 7 : 10);
            }
	  else if (!speed)
	    *total = COSTS_N_INSNS (AVR_HAVE_JMP_CALL ? 2 : 1);
	  else
	    return false;
	  break;

        case E_PSImode:
          if (!speed)
            *total = COSTS_N_INSNS (AVR_HAVE_JMP_CALL ? 2 : 1);
          else
            *total = 10;
          break;

	case E_SImode:
	case E_DImode:
	  if (AVR_HAVE_MUL)
            {
              if (!speed)
                {
                  /* Add some additional costs besides CALL like moves etc.  */

                  *total = COSTS_N_INSNS (AVR_HAVE_JMP_CALL ? 5 : 4);
                }
              else
                {
                  /* Just a rough estimate.  Even with -O2 we don't want bulky
                     code expanded inline.  */

                  *total = COSTS_N_INSNS (25);
                }
            }
          else
            {
              if (speed)
                *total = COSTS_N_INSNS (300);
              else
                /* Add some additional costs besides CALL like moves etc.  */
                *total = COSTS_N_INSNS (AVR_HAVE_JMP_CALL ? 5 : 4);
            }

	  if (mode == DImode)
	    *total *= 2;

	  return true;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
      return true;

    case DIV:
    case MOD:
    case UDIV:
    case UMOD:
      if (!speed)
        *total = COSTS_N_INSNS (AVR_HAVE_JMP_CALL ? 2 : 1);
      else
        *total = COSTS_N_INSNS (15 * GET_MODE_SIZE (mode));
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      /* For div/mod with const-int divisor we have at least the cost of
         loading the divisor. */
      if (CONST_INT_P (XEXP (x, 1)))
        *total += COSTS_N_INSNS (GET_MODE_SIZE (mode));
      /* Add some overall penaly for clobbering and moving around registers */
      *total += COSTS_N_INSNS (2);
      return true;

    case ROTATE:
      switch (mode)
	{
	case E_QImode:
	  if (CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) == 4)
	    *total = COSTS_N_INSNS (1);

	  break;

	case E_HImode:
	  if (CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) == 8)
	    *total = COSTS_N_INSNS (3);

	  break;

	case E_SImode:
	  if (CONST_INT_P (XEXP (x, 1)))
	    switch (INTVAL (XEXP (x, 1)))
	      {
	      case 8:
	      case 24:
		*total = COSTS_N_INSNS (5);
		break;
	      case 16:
		*total = COSTS_N_INSNS (AVR_HAVE_MOVW ? 4 : 6);
		break;
	      }
	  break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case ASHIFT:
      switch (mode)
	{
	case E_QImode:
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 4 : 17);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    {
	      val = INTVAL (XEXP (x, 1));
	      if (val == 7)
		*total = COSTS_N_INSNS (3);
	      else if (val >= 0 && val <= 7)
		*total = COSTS_N_INSNS (val);
	      else
		*total = COSTS_N_INSNS (1);
	    }
	  break;

	case E_HImode:
          if (AVR_HAVE_MUL)
            {
              if (const_2_to_7_operand (XEXP (x, 1), HImode)
                  && (SIGN_EXTEND == GET_CODE (XEXP (x, 0))
                      || ZERO_EXTEND == GET_CODE (XEXP (x, 0))))
                {
                  *total = COSTS_N_INSNS (!speed ? 4 : 6);
                  return true;
                }
            }

          if (const1_rtx == (XEXP (x, 1))
              && SIGN_EXTEND == GET_CODE (XEXP (x, 0)))
            {
              *total = COSTS_N_INSNS (2);
              return true;
            }

	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 5 : 41);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (INTVAL (XEXP (x, 1)))
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
	      case 8:
		*total = COSTS_N_INSNS (2);
		break;
	      case 9:
		*total = COSTS_N_INSNS (3);
		break;
	      case 2:
	      case 3:
	      case 10:
	      case 15:
		*total = COSTS_N_INSNS (4);
		break;
	      case 7:
	      case 11:
	      case 12:
		*total = COSTS_N_INSNS (5);
		break;
	      case 4:
		*total = COSTS_N_INSNS (!speed ? 5 : 8);
		break;
	      case 6:
		*total = COSTS_N_INSNS (!speed ? 5 : 9);
		break;
	      case 5:
		*total = COSTS_N_INSNS (!speed ? 5 : 10);
		break;
	      default:
	        *total = COSTS_N_INSNS (!speed ? 5 : 41);
	        *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
						speed);
	      }
	  break;

        case E_PSImode:
          if (!CONST_INT_P (XEXP (x, 1)))
            {
              *total = COSTS_N_INSNS (!speed ? 6 : 73);
            }
          else
            switch (INTVAL (XEXP (x, 1)))
              {
              case 0:
                *total = 0;
                break;
              case 1:
              case 8:
              case 16:
                *total = COSTS_N_INSNS (3);
                break;
              case 23:
                *total = COSTS_N_INSNS (5);
                break;
              default:
                *total = COSTS_N_INSNS (!speed ? 5 : 3 * INTVAL (XEXP (x, 1)));
                break;
              }
          break;

	case E_SImode:
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 7 : 113);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (INTVAL (XEXP (x, 1)))
	      {
	      case 0:
		*total = 0;
		break;
	      case 24:
		*total = COSTS_N_INSNS (3);
		break;
	      case 1:
	      case 8:
	      case 16:
		*total = COSTS_N_INSNS (4);
		break;
	      case 31:
		*total = COSTS_N_INSNS (6);
		break;
	      case 2:
		*total = COSTS_N_INSNS (!speed ? 7 : 8);
		break;
	      default:
		*total = COSTS_N_INSNS (!speed ? 7 : 113);
		*total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
						speed);
	      }
	  break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case ASHIFTRT:
      switch (mode)
	{
	case E_QImode:
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 4 : 17);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    {
	      val = INTVAL (XEXP (x, 1));
	      if (val == 6)
		*total = COSTS_N_INSNS (4);
	      else if (val == 7)
		*total = COSTS_N_INSNS (2);
	      else if (val >= 0 && val <= 7)
		*total = COSTS_N_INSNS (val);
	      else
		*total = COSTS_N_INSNS (1);
	    }
	  break;

	case E_HImode:
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 5 : 41);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (INTVAL (XEXP (x, 1)))
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
		*total = COSTS_N_INSNS (2);
		break;
	      case 15:
		*total = COSTS_N_INSNS (3);
		break;
	      case 2:
	      case 7:
              case 8:
              case 9:
		*total = COSTS_N_INSNS (4);
		break;
              case 10:
	      case 14:
		*total = COSTS_N_INSNS (5);
		break;
              case 11:
                *total = COSTS_N_INSNS (!speed ? 5 : 6);
		break;
              case 12:
                *total = COSTS_N_INSNS (!speed ? 5 : 7);
		break;
              case 6:
	      case 13:
                *total = COSTS_N_INSNS (!speed ? 5 : 8);
		break;
	      default:
	        *total = COSTS_N_INSNS (!speed ? 5 : 41);
	        *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
						speed);
	      }
	  break;

        case E_PSImode:
          if (!CONST_INT_P (XEXP (x, 1)))
            {
              *total = COSTS_N_INSNS (!speed ? 6 : 73);
            }
          else
            switch (INTVAL (XEXP (x, 1)))
              {
              case 0:
                *total = 0;
                break;
              case 1:
                *total = COSTS_N_INSNS (3);
                break;
              case 16:
              case 8:
                *total = COSTS_N_INSNS (5);
                break;
              case 23:
                *total = COSTS_N_INSNS (4);
                break;
              default:
                *total = COSTS_N_INSNS (!speed ? 5 : 3 * INTVAL (XEXP (x, 1)));
                break;
              }
          break;

	case E_SImode:
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 7 : 113);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (INTVAL (XEXP (x, 1)))
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
		*total = COSTS_N_INSNS (4);
		break;
	      case 8:
	      case 16:
	      case 24:
		*total = COSTS_N_INSNS (6);
		break;
	      case 2:
		*total = COSTS_N_INSNS (!speed ? 7 : 8);
		break;
	      case 31:
		*total = COSTS_N_INSNS (AVR_HAVE_MOVW ? 4 : 5);
		break;
	      default:
		*total = COSTS_N_INSNS (!speed ? 7 : 113);
		*total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
						speed);
	      }
	  break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case LSHIFTRT:
      if (outer_code == TRUNCATE)
        {
          *total = avr_mul_highpart_cost (x, speed);
          return true;
        }

      switch (mode)
	{
	case E_QImode:
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 4 : 17);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    {
	      val = INTVAL (XEXP (x, 1));
	      if (val == 7)
		*total = COSTS_N_INSNS (3);
	      else if (val >= 0 && val <= 7)
		*total = COSTS_N_INSNS (val);
	      else
		*total = COSTS_N_INSNS (1);
	    }
	  break;

	case E_HImode:
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 5 : 41);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (INTVAL (XEXP (x, 1)))
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
	      case 8:
		*total = COSTS_N_INSNS (2);
		break;
	      case 9:
		*total = COSTS_N_INSNS (3);
		break;
	      case 2:
	      case 10:
	      case 15:
		*total = COSTS_N_INSNS (4);
		break;
	      case 7:
              case 11:
		*total = COSTS_N_INSNS (5);
		break;
	      case 3:
	      case 12:
	      case 13:
	      case 14:
		*total = COSTS_N_INSNS (!speed ? 5 : 6);
		break;
	      case 4:
		*total = COSTS_N_INSNS (!speed ? 5 : 7);
		break;
	      case 5:
	      case 6:
		*total = COSTS_N_INSNS (!speed ? 5 : 9);
		break;
	      default:
	        *total = COSTS_N_INSNS (!speed ? 5 : 41);
	        *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
						speed);
	      }
	  break;

        case E_PSImode:
          if (!CONST_INT_P (XEXP (x, 1)))
            {
              *total = COSTS_N_INSNS (!speed ? 6 : 73);
            }
          else
            switch (INTVAL (XEXP (x, 1)))
              {
              case 0:
                *total = 0;
                break;
              case 1:
              case 8:
              case 16:
                *total = COSTS_N_INSNS (3);
                break;
              case 23:
                *total = COSTS_N_INSNS (5);
                break;
              default:
                *total = COSTS_N_INSNS (!speed ? 5 : 3 * INTVAL (XEXP (x, 1)));
                break;
              }
          break;

	case E_SImode:
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 7 : 113);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (INTVAL (XEXP (x, 1)))
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
		*total = COSTS_N_INSNS (4);
		break;
	      case 2:
		*total = COSTS_N_INSNS (!speed ? 7 : 8);
		break;
	      case 8:
	      case 16:
	      case 24:
		*total = COSTS_N_INSNS (4);
		break;
	      case 31:
		*total = COSTS_N_INSNS (6);
		break;
	      default:
		*total = COSTS_N_INSNS (!speed ? 7 : 113);
		*total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
						speed);
	      }
	  break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case COMPARE:
      switch (GET_MODE (XEXP (x, 0)))
	{
	case E_QImode:
	  *total = COSTS_N_INSNS (1);
	  if (!CONST_INT_P (XEXP (x, 1)))
	    *total += avr_operand_rtx_cost (XEXP (x, 1), QImode, code,
					    1, speed);
	  break;

        case E_HImode:
	  *total = COSTS_N_INSNS (2);
	  if (!CONST_INT_P (XEXP (x, 1)))
            *total += avr_operand_rtx_cost (XEXP (x, 1), HImode, code,
					    1, speed);
	  else if (INTVAL (XEXP (x, 1)) != 0)
	    *total += COSTS_N_INSNS (1);
          break;

        case E_PSImode:
          *total = COSTS_N_INSNS (3);
          if (CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) != 0)
            *total += COSTS_N_INSNS (2);
          break;

        case E_SImode:
          *total = COSTS_N_INSNS (4);
          if (!CONST_INT_P (XEXP (x, 1)))
            *total += avr_operand_rtx_cost (XEXP (x, 1), SImode, code,
					    1, speed);
	  else if (INTVAL (XEXP (x, 1)) != 0)
	    *total += COSTS_N_INSNS (3);
          break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), GET_MODE (XEXP (x, 0)),
				      code, 0, speed);
      return true;

    case TRUNCATE:
      if (LSHIFTRT == GET_CODE (XEXP (x, 0)))
        {
          *total = avr_mul_highpart_cost (XEXP (x, 0), speed);
          return true;
        }
      break;

    default:
      break;
    }
  return false;
}


/* Implement `TARGET_RTX_COSTS'.  */

static bool
avr_rtx_costs (rtx x, machine_mode mode, int outer_code,
	       int opno, int *total, bool speed)
{
  bool done = avr_rtx_costs_1 (x, mode, outer_code, opno, total, speed);

  if (avr_log.rtx_costs)
    {
      avr_edump ("\n%?=%b (%s) total=%d, outer=%C:\n%r\n",
                 done, speed ? "speed" : "size", *total, outer_code, x);
    }

  return done;
}


/* Implement `TARGET_ADDRESS_COST'.  */

static int
avr_address_cost (rtx x, machine_mode mode ATTRIBUTE_UNUSED,
                  addr_space_t as ATTRIBUTE_UNUSED,
                  bool speed ATTRIBUTE_UNUSED)
{
  int cost = 4;

  if (GET_CODE (x) == PLUS
      && CONST_INT_P (XEXP (x, 1))
      && (REG_P (XEXP (x, 0))
          || SUBREG_P (XEXP (x, 0))))
    {
      if (INTVAL (XEXP (x, 1)) > MAX_LD_OFFSET(mode))
        cost = 18;
    }
  else if (CONSTANT_ADDRESS_P (x))
    {
      if (io_address_operand (x, QImode))
        cost = 2;

      if (AVR_TINY
          && avr_address_tiny_absdata_p (x, QImode))
        cost = 2;
    }

  if (avr_log.address_cost)
    avr_edump ("\n%?: %d = %r\n", cost, x);

  return cost;
}

/* Test for extra memory constraint 'Q'.
   It's a memory address based on Y or Z pointer with valid displacement.  */

int
extra_constraint_Q (rtx x)
{
  int ok = 0;
  rtx plus = XEXP (x, 0);

  if (GET_CODE (plus) == PLUS
      && REG_P (XEXP (plus, 0))
      && CONST_INT_P (XEXP (plus, 1))
      && (INTVAL (XEXP (plus, 1))
	  <= MAX_LD_OFFSET (GET_MODE (x))))
    {
      rtx xx = XEXP (plus, 0);
      int regno = REGNO (xx);

      ok = (/* allocate pseudos */
            regno >= FIRST_PSEUDO_REGISTER
            /* strictly check */
            || regno == REG_Z || regno == REG_Y
            /* XXX frame & arg pointer checks */
            || xx == frame_pointer_rtx
            || xx == arg_pointer_rtx);

      if (avr_log.constraints)
        avr_edump ("\n%?=%d reload_completed=%d reload_in_progress=%d\n %r\n",
                   ok, reload_completed, reload_in_progress, x);
    }

  return ok;
}

/* Convert condition code CONDITION to the valid AVR condition code.  */

RTX_CODE
avr_normalize_condition (RTX_CODE condition)
{
  switch (condition)
    {
    case GT:
      return GE;
    case GTU:
      return GEU;
    case LE:
      return LT;
    case LEU:
      return LTU;
    default:
      gcc_unreachable ();
    }
}

/* Helper function for `avr_reorg'.  */

static rtx
avr_compare_pattern (rtx_insn *insn)
{
  rtx pattern = single_set (insn);

  if (pattern
      && NONJUMP_INSN_P (insn)
      && SET_DEST (pattern) == cc0_rtx
      && GET_CODE (SET_SRC (pattern)) == COMPARE)
    {
      machine_mode mode0 = GET_MODE (XEXP (SET_SRC (pattern), 0));
      machine_mode mode1 = GET_MODE (XEXP (SET_SRC (pattern), 1));

      /* The 64-bit comparisons have fixed operands ACC_A and ACC_B.
         They must not be swapped, thus skip them.  */

      if ((mode0 == VOIDmode || GET_MODE_SIZE (mode0) <= 4)
          && (mode1 == VOIDmode || GET_MODE_SIZE (mode1) <= 4))
        return pattern;
    }

  return NULL_RTX;
}

/* Helper function for `avr_reorg'.  */

/* Expansion of switch/case decision trees leads to code like

       cc0 = compare (Reg, Num)
       if (cc0 == 0)
         goto L1

       cc0 = compare (Reg, Num)
       if (cc0 > 0)
         goto L2

   The second comparison is superfluous and can be deleted.
   The second jump condition can be transformed from a
   "difficult" one to a "simple" one because "cc0 > 0" and
   "cc0 >= 0" will have the same effect here.

   This function relies on the way switch/case is being expaned
   as binary decision tree.  For example code see PR 49903.

   Return TRUE if optimization performed.
   Return FALSE if nothing changed.

   INSN1 is a comparison, i.e. avr_compare_pattern != 0.

   We don't want to do this in text peephole because it is
   tedious to work out jump offsets there and the second comparison
   might have been transormed by `avr_reorg'.

   RTL peephole won't do because peephole2 does not scan across
   basic blocks.  */

static bool
avr_reorg_remove_redundant_compare (rtx_insn *insn1)
{
  rtx comp1, ifelse1, xcond1;
  rtx_insn *branch1;
  rtx comp2, ifelse2, xcond2;
  rtx_insn *branch2, *insn2;
  enum rtx_code code;
  rtx_insn *jump;
  rtx target, cond;

  /* Look out for:  compare1 - branch1 - compare2 - branch2  */

  branch1 = next_nonnote_nondebug_insn (insn1);
  if (!branch1 || !JUMP_P (branch1))
    return false;

  insn2 = next_nonnote_nondebug_insn (branch1);
  if (!insn2 || !avr_compare_pattern (insn2))
    return false;

  branch2 = next_nonnote_nondebug_insn (insn2);
  if (!branch2 || !JUMP_P (branch2))
    return false;

  comp1 = avr_compare_pattern (insn1);
  comp2 = avr_compare_pattern (insn2);
  xcond1 = single_set (branch1);
  xcond2 = single_set (branch2);

  if (!comp1 || !comp2
      || !rtx_equal_p (comp1, comp2)
      || !xcond1 || SET_DEST (xcond1) != pc_rtx
      || !xcond2 || SET_DEST (xcond2) != pc_rtx
      || IF_THEN_ELSE != GET_CODE (SET_SRC (xcond1))
      || IF_THEN_ELSE != GET_CODE (SET_SRC (xcond2)))
    {
      return false;
    }

  comp1 = SET_SRC (comp1);
  ifelse1 = SET_SRC (xcond1);
  ifelse2 = SET_SRC (xcond2);

  /* comp<n> is COMPARE now and ifelse<n> is IF_THEN_ELSE.  */

  if (EQ != GET_CODE (XEXP (ifelse1, 0))
      || !REG_P (XEXP (comp1, 0))
      || !CONST_INT_P (XEXP (comp1, 1))
      || XEXP (ifelse1, 2) != pc_rtx
      || XEXP (ifelse2, 2) != pc_rtx
      || LABEL_REF != GET_CODE (XEXP (ifelse1, 1))
      || LABEL_REF != GET_CODE (XEXP (ifelse2, 1))
      || !COMPARISON_P (XEXP (ifelse2, 0))
      || cc0_rtx != XEXP (XEXP (ifelse1, 0), 0)
      || cc0_rtx != XEXP (XEXP (ifelse2, 0), 0)
      || const0_rtx != XEXP (XEXP (ifelse1, 0), 1)
      || const0_rtx != XEXP (XEXP (ifelse2, 0), 1))
    {
      return false;
    }

  /* We filtered the insn sequence to look like

        (set (cc0)
             (compare (reg:M N)
                      (const_int VAL)))
        (set (pc)
             (if_then_else (eq (cc0)
                               (const_int 0))
                           (label_ref L1)
                           (pc)))

        (set (cc0)
             (compare (reg:M N)
                      (const_int VAL)))
        (set (pc)
             (if_then_else (CODE (cc0)
                                 (const_int 0))
                           (label_ref L2)
                           (pc)))
  */

  code = GET_CODE (XEXP (ifelse2, 0));

  /* Map GT/GTU to GE/GEU which is easier for AVR.
     The first two instructions compare/branch on EQ
     so we may replace the difficult

        if (x == VAL)   goto L1;
        if (x > VAL)    goto L2;

     with easy

         if (x == VAL)   goto L1;
         if (x >= VAL)   goto L2;

     Similarly, replace LE/LEU by LT/LTU.  */

  switch (code)
    {
    case EQ:
    case LT:  case LTU:
    case GE:  case GEU:
      break;

    case LE:  case LEU:
    case GT:  case GTU:
      code = avr_normalize_condition (code);
      break;

    default:
      return false;
    }

  /* Wrap the branches into UNSPECs so they won't be changed or
     optimized in the remainder.  */

  target = XEXP (XEXP (ifelse1, 1), 0);
  cond = XEXP (ifelse1, 0);
  jump = emit_jump_insn_after (gen_branch_unspec (target, cond), insn1);

  JUMP_LABEL (jump) = JUMP_LABEL (branch1);

  target = XEXP (XEXP (ifelse2, 1), 0);
  cond = gen_rtx_fmt_ee (code, VOIDmode, cc0_rtx, const0_rtx);
  jump = emit_jump_insn_after (gen_branch_unspec (target, cond), insn2);

  JUMP_LABEL (jump) = JUMP_LABEL (branch2);

  /* The comparisons in insn1 and insn2 are exactly the same;
     insn2 is superfluous so delete it.  */

  delete_insn (insn2);
  delete_insn (branch1);
  delete_insn (branch2);

  return true;
}


/* Implement `TARGET_MACHINE_DEPENDENT_REORG'.  */
/* Optimize conditional jumps.  */

static void
avr_reorg (void)
{
  rtx_insn *insn = get_insns();

  for (insn = next_real_insn (insn); insn; insn = next_real_insn (insn))
    {
      rtx pattern = avr_compare_pattern (insn);

      if (!pattern)
        continue;

      if (optimize
          && avr_reorg_remove_redundant_compare (insn))
        {
          continue;
        }

      if (compare_diff_p (insn))
	{
          /* Now we work under compare insn with difficult branch.  */

	  rtx_insn *next = next_real_insn (insn);
          rtx pat = PATTERN (next);

          pattern = SET_SRC (pattern);

          if (true_regnum (XEXP (pattern, 0)) >= 0
              && true_regnum (XEXP (pattern, 1)) >= 0)
            {
              rtx x = XEXP (pattern, 0);
              rtx src = SET_SRC (pat);
              rtx t = XEXP (src, 0);
              PUT_CODE (t, swap_condition (GET_CODE (t)));
              XEXP (pattern, 0) = XEXP (pattern, 1);
              XEXP (pattern, 1) = x;
              INSN_CODE (next) = -1;
            }
          else if (true_regnum (XEXP (pattern, 0)) >= 0
                   && XEXP (pattern, 1) == const0_rtx)
            {
              /* This is a tst insn, we can reverse it.  */
              rtx src = SET_SRC (pat);
              rtx t = XEXP (src, 0);

              PUT_CODE (t, swap_condition (GET_CODE (t)));
              XEXP (pattern, 1) = XEXP (pattern, 0);
              XEXP (pattern, 0) = const0_rtx;
              INSN_CODE (next) = -1;
              INSN_CODE (insn) = -1;
            }
          else if (true_regnum (XEXP (pattern, 0)) >= 0
                   && CONST_INT_P (XEXP (pattern, 1)))
            {
              rtx x = XEXP (pattern, 1);
              rtx src = SET_SRC (pat);
              rtx t = XEXP (src, 0);
              machine_mode mode = GET_MODE (XEXP (pattern, 0));

              if (avr_simplify_comparison_p (mode, GET_CODE (t), x))
                {
                  XEXP (pattern, 1) = gen_int_mode (INTVAL (x) + 1, mode);
                  PUT_CODE (t, avr_normalize_condition (GET_CODE (t)));
                  INSN_CODE (next) = -1;
                  INSN_CODE (insn) = -1;
                }
            }
        }
    }
}

/* Returns register number for function return value.*/

static inline unsigned int
avr_ret_register (void)
{
  return 24;
}


/* Implement `TARGET_FUNCTION_VALUE_REGNO_P'.  */

static bool
avr_function_value_regno_p (const unsigned int regno)
{
  return (regno == avr_ret_register ());
}


/* Implement `TARGET_LIBCALL_VALUE'.  */
/* Create an RTX representing the place where a
   library function returns a value of mode MODE.  */

static rtx
avr_libcall_value (machine_mode mode,
		   const_rtx func ATTRIBUTE_UNUSED)
{
  int offs = GET_MODE_SIZE (mode);

  if (offs <= 4)
    offs = (offs + 1) & ~1;

  return gen_rtx_REG (mode, avr_ret_register () + 2 - offs);
}


/* Implement `TARGET_FUNCTION_VALUE'.  */
/* Create an RTX representing the place where a
   function returns a value of data type VALTYPE.  */

static rtx
avr_function_value (const_tree type,
                    const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
                    bool outgoing ATTRIBUTE_UNUSED)
{
  unsigned int offs;

  if (TYPE_MODE (type) != BLKmode)
    return avr_libcall_value (TYPE_MODE (type), NULL_RTX);

  offs = int_size_in_bytes (type);
  if (offs < 2)
    offs = 2;
  if (offs > 2 && offs < GET_MODE_SIZE (SImode))
    offs = GET_MODE_SIZE (SImode);
  else if (offs > GET_MODE_SIZE (SImode) && offs < GET_MODE_SIZE (DImode))
    offs = GET_MODE_SIZE (DImode);

  return gen_rtx_REG (BLKmode, avr_ret_register () + 2 - offs);
}

int
test_hard_reg_class (enum reg_class rclass, rtx x)
{
  int regno = true_regnum (x);
  if (regno < 0)
    return 0;

  if (TEST_HARD_REG_CLASS (rclass, regno))
    return 1;

  return 0;
}


/* Helper for jump_over_one_insn_p:  Test if INSN is a 2-word instruction
   and thus is suitable to be skipped by CPSE, SBRC, etc.  */

static bool
avr_2word_insn_p (rtx_insn *insn)
{
  if (TARGET_SKIP_BUG
      || !insn
      || 2 != get_attr_length (insn))
    {
      return false;
    }

  switch (INSN_CODE (insn))
    {
    default:
      return false;

    case CODE_FOR_movqi_insn:
    case CODE_FOR_movuqq_insn:
    case CODE_FOR_movqq_insn:
      {
        rtx set  = single_set (insn);
        rtx src  = SET_SRC (set);
        rtx dest = SET_DEST (set);

        /* Factor out LDS and STS from movqi_insn.  */

        if (MEM_P (dest)
            && (REG_P (src) || src == CONST0_RTX (GET_MODE (dest))))
          {
            return CONSTANT_ADDRESS_P (XEXP (dest, 0));
          }
        else if (REG_P (dest)
                 && MEM_P (src))
          {
            return CONSTANT_ADDRESS_P (XEXP (src, 0));
          }

        return false;
      }

    case CODE_FOR_call_insn:
    case CODE_FOR_call_value_insn:
      return true;
    }
}


int
jump_over_one_insn_p (rtx_insn *insn, rtx dest)
{
  int uid = INSN_UID (GET_CODE (dest) == LABEL_REF
		      ? XEXP (dest, 0)
		      : dest);
  int jump_addr = INSN_ADDRESSES (INSN_UID (insn));
  int dest_addr = INSN_ADDRESSES (uid);
  int jump_offset = dest_addr - jump_addr - get_attr_length (insn);

  return (jump_offset == 1
          || (jump_offset == 2
              && avr_2word_insn_p (next_active_insn (insn))));
}


/* Implement TARGET_HARD_REGNO_MODE_OK.  On the enhanced core, anything
   larger than 1 byte must start in even numbered register for "movw" to
   work (this way we don't have to check for odd registers everywhere).  */

static bool
avr_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  /* NOTE: 8-bit values must not be disallowed for R28 or R29.
        Disallowing QI et al. in these regs might lead to code like
            (set (subreg:QI (reg:HI 28) n) ...)
        which will result in wrong code because reload does not
        handle SUBREGs of hard regsisters like this.
        This could be fixed in reload.  However, it appears
        that fixing reload is not wanted by reload people.  */

  /* Any GENERAL_REGS register can hold 8-bit values.  */

  if (GET_MODE_SIZE (mode) == 1)
    return true;

  /* FIXME: Ideally, the following test is not needed.
        However, it turned out that it can reduce the number
        of spill fails.  AVR and it's poor endowment with
        address registers is extreme stress test for reload.  */

  if (GET_MODE_SIZE (mode) >= 4
      && regno >= REG_X)
    return false;

  /* All modes larger than 8 bits should start in an even register.  */

  return !(regno & 1);
}


/* Implement TARGET_HARD_REGNO_CALL_PART_CLOBBERED.  */

static bool
avr_hard_regno_call_part_clobbered (unsigned regno, machine_mode mode)
{
  /* FIXME: This hook gets called with MODE:REGNO combinations that don't
        represent valid hard registers like, e.g. HI:29.  Returning TRUE
        for such registers can lead to performance degradation as mentioned
        in PR53595.  Thus, report invalid hard registers as FALSE.  */

  if (!avr_hard_regno_mode_ok (regno, mode))
    return 0;

  /* Return true if any of the following boundaries is crossed:
     17/18 or 19/20 (if AVR_TINY), 27/28 and 29/30.  */

  return ((regno <= LAST_CALLEE_SAVED_REG
           && regno + GET_MODE_SIZE (mode) > 1 + LAST_CALLEE_SAVED_REG)
          || (regno < REG_Y && regno + GET_MODE_SIZE (mode) > REG_Y)
          || (regno < REG_Z && regno + GET_MODE_SIZE (mode) > REG_Z));
}


/* Implement `MODE_CODE_BASE_REG_CLASS'.  */

enum reg_class
avr_mode_code_base_reg_class (machine_mode mode ATTRIBUTE_UNUSED,
                              addr_space_t as, RTX_CODE outer_code,
                              RTX_CODE index_code ATTRIBUTE_UNUSED)
{
  if (!ADDR_SPACE_GENERIC_P (as))
    {
      return POINTER_Z_REGS;
    }

  if (!avr_strict_X)
    return reload_completed ? BASE_POINTER_REGS : POINTER_REGS;

  return PLUS == outer_code ? BASE_POINTER_REGS : POINTER_REGS;
}


/* Implement `REGNO_MODE_CODE_OK_FOR_BASE_P'.  */

bool
avr_regno_mode_code_ok_for_base_p (int regno,
                                   machine_mode mode ATTRIBUTE_UNUSED,
                                   addr_space_t as ATTRIBUTE_UNUSED,
                                   RTX_CODE outer_code,
                                   RTX_CODE index_code ATTRIBUTE_UNUSED)
{
  bool ok = false;

  if (!ADDR_SPACE_GENERIC_P (as))
    {
      if (regno < FIRST_PSEUDO_REGISTER
          && regno == REG_Z)
        {
          return true;
        }

      if (reg_renumber)
        {
          regno = reg_renumber[regno];

          if (regno == REG_Z)
            {
              return true;
            }
        }

      return false;
    }

  if (regno < FIRST_PSEUDO_REGISTER
      && (regno == REG_X
          || regno == REG_Y
          || regno == REG_Z
          || regno == ARG_POINTER_REGNUM))
    {
      ok = true;
    }
  else if (reg_renumber)
    {
      regno = reg_renumber[regno];

      if (regno == REG_X
          || regno == REG_Y
          || regno == REG_Z
          || regno == ARG_POINTER_REGNUM)
        {
          ok = true;
        }
    }

  if (avr_strict_X
      && PLUS == outer_code
      && regno == REG_X)
    {
      ok = false;
    }

  return ok;
}


/* A helper for `output_reload_insisf' and `output_reload_inhi'.  */
/* Set 32-bit register OP[0] to compile-time constant OP[1].
   CLOBBER_REG is a QI clobber register or NULL_RTX.
   LEN == NULL: output instructions.
   LEN != NULL: set *LEN to the length of the instruction sequence
                (in words) printed with LEN = NULL.
   If CLEAR_P is true, OP[0] had been cleard to Zero already.
   If CLEAR_P is false, nothing is known about OP[0].

   The effect on cc0 is as follows:

   Load 0 to any register except ZERO_REG : NONE
   Load ld register with any value        : NONE
   Anything else:                         : CLOBBER  */

static void
output_reload_in_const (rtx *op, rtx clobber_reg, int *len, bool clear_p)
{
  rtx src = op[1];
  rtx dest = op[0];
  rtx xval, xdest[4];
  int ival[4];
  int clobber_val = 1234;
  bool cooked_clobber_p = false;
  bool set_p = false;
  machine_mode mode = GET_MODE (dest);
  int n_bytes = GET_MODE_SIZE (mode);

  gcc_assert (REG_P (dest)
              && CONSTANT_P (src));

  if (len)
    *len = 0;

  /* (REG:SI 14) is special: It's neither in LD_REGS nor in NO_LD_REGS
     but has some subregs that are in LD_REGS.  Use the MSB (REG:QI 17).  */

  if (REGNO (dest) < 16
      && REGNO (dest) + GET_MODE_SIZE (mode) > 16)
    {
      clobber_reg = all_regs_rtx[REGNO (dest) + n_bytes - 1];
    }

  /* We might need a clobber reg but don't have one.  Look at the value to
     be loaded more closely.  A clobber is only needed if it is a symbol
     or contains a byte that is neither 0, -1 or a power of 2.  */

  if (NULL_RTX == clobber_reg
      && !test_hard_reg_class (LD_REGS, dest)
      && (! (CONST_INT_P (src) || CONST_FIXED_P (src) || CONST_DOUBLE_P (src))
          || !avr_popcount_each_byte (src, n_bytes,
                                      (1 << 0) | (1 << 1) | (1 << 8))))
    {
      /* We have no clobber register but need one.  Cook one up.
         That's cheaper than loading from constant pool.  */

      cooked_clobber_p = true;
      clobber_reg = all_regs_rtx[REG_Z + 1];
      avr_asm_len ("mov __tmp_reg__,%0", &clobber_reg, len, 1);
    }

  /* Now start filling DEST from LSB to MSB.  */

  for (int n = 0; n < n_bytes; n++)
    {
      int ldreg_p;
      bool done_byte = false;
      rtx xop[3];

      /* Crop the n-th destination byte.  */

      xdest[n] = simplify_gen_subreg (QImode, dest, mode, n);
      ldreg_p = test_hard_reg_class (LD_REGS, xdest[n]);

      if (!CONST_INT_P (src)
          && !CONST_FIXED_P (src)
          && !CONST_DOUBLE_P (src))
        {
          static const char* const asm_code[][2] =
            {
              { "ldi %2,lo8(%1)"  CR_TAB "mov %0,%2",    "ldi %0,lo8(%1)"  },
              { "ldi %2,hi8(%1)"  CR_TAB "mov %0,%2",    "ldi %0,hi8(%1)"  },
              { "ldi %2,hlo8(%1)" CR_TAB "mov %0,%2",    "ldi %0,hlo8(%1)" },
              { "ldi %2,hhi8(%1)" CR_TAB "mov %0,%2",    "ldi %0,hhi8(%1)" }
            };

          xop[0] = xdest[n];
          xop[1] = src;
          xop[2] = clobber_reg;

          avr_asm_len (asm_code[n][ldreg_p], xop, len, ldreg_p ? 1 : 2);

          continue;
        }

      /* Crop the n-th source byte.  */

      xval = simplify_gen_subreg (QImode, src, mode, n);
      ival[n] = INTVAL (xval);

      /* Look if we can reuse the low word by means of MOVW.  */

      if (n == 2
          && n_bytes >= 4
          && AVR_HAVE_MOVW)
        {
          rtx lo16 = simplify_gen_subreg (HImode, src, mode, 0);
          rtx hi16 = simplify_gen_subreg (HImode, src, mode, 2);

          if (INTVAL (lo16) == INTVAL (hi16))
            {
              if (0 != INTVAL (lo16)
                  || !clear_p)
                {
                  avr_asm_len ("movw %C0,%A0", &op[0], len, 1);
                }

              break;
            }
        }

      /* Don't use CLR so that cc0 is set as expected.  */

      if (ival[n] == 0)
        {
          if (!clear_p)
            avr_asm_len (ldreg_p ? "ldi %0,0"
                         : AVR_ZERO_REGNO == REGNO (xdest[n]) ? "clr %0"
                         : "mov %0,__zero_reg__",
                         &xdest[n], len, 1);
          continue;
        }

      if (clobber_val == ival[n]
          && REGNO (clobber_reg) == REGNO (xdest[n]))
        {
          continue;
        }

      /* LD_REGS can use LDI to move a constant value */

      if (ldreg_p)
        {
          xop[0] = xdest[n];
          xop[1] = xval;
          avr_asm_len ("ldi %0,lo8(%1)", xop, len, 1);
          continue;
        }

      /* Try to reuse value already loaded in some lower byte. */

      for (int j = 0; j < n; j++)
        if (ival[j] == ival[n])
          {
            xop[0] = xdest[n];
            xop[1] = xdest[j];

            avr_asm_len ("mov %0,%1", xop, len, 1);
            done_byte = true;
            break;
          }

      if (done_byte)
        continue;

      /* Need no clobber reg for -1: Use CLR/DEC */

      if (-1 == ival[n])
        {
          if (!clear_p)
            avr_asm_len ("clr %0", &xdest[n], len, 1);

          avr_asm_len ("dec %0", &xdest[n], len, 1);
          continue;
        }
      else if (1 == ival[n])
        {
          if (!clear_p)
            avr_asm_len ("clr %0", &xdest[n], len, 1);

          avr_asm_len ("inc %0", &xdest[n], len, 1);
          continue;
        }

      /* Use T flag or INC to manage powers of 2 if we have
         no clobber reg.  */

      if (NULL_RTX == clobber_reg
          && single_one_operand (xval, QImode))
        {
          xop[0] = xdest[n];
          xop[1] = GEN_INT (exact_log2 (ival[n] & GET_MODE_MASK (QImode)));

          gcc_assert (constm1_rtx != xop[1]);

          if (!set_p)
            {
              set_p = true;
              avr_asm_len ("set", xop, len, 1);
            }

          if (!clear_p)
            avr_asm_len ("clr %0", xop, len, 1);

          avr_asm_len ("bld %0,%1", xop, len, 1);
          continue;
        }

      /* We actually need the LD_REGS clobber reg.  */

      gcc_assert (NULL_RTX != clobber_reg);

      xop[0] = xdest[n];
      xop[1] = xval;
      xop[2] = clobber_reg;
      clobber_val = ival[n];

      avr_asm_len ("ldi %2,lo8(%1)" CR_TAB
                   "mov %0,%2", xop, len, 2);
    }

  /* If we cooked up a clobber reg above, restore it.  */

  if (cooked_clobber_p)
    {
      avr_asm_len ("mov %0,__tmp_reg__", &clobber_reg, len, 1);
    }
}


/* Reload the constant OP[1] into the HI register OP[0].
   CLOBBER_REG is a QI clobber reg needed to move vast majority of consts
   into a NO_LD_REGS register.  If CLOBBER_REG is NULL_RTX we either don't
   need a clobber reg or have to cook one up.

   PLEN == NULL: Output instructions.
   PLEN != NULL: Output nothing.  Set *PLEN to number of words occupied
                 by the insns printed.

   Return "".  */

const char*
output_reload_inhi (rtx *op, rtx clobber_reg, int *plen)
{
  output_reload_in_const (op, clobber_reg, plen, false);
  return "";
}


/* Reload a SI or SF compile time constant OP[1] into the register OP[0].
   CLOBBER_REG is a QI clobber reg needed to move vast majority of consts
   into a NO_LD_REGS register.  If CLOBBER_REG is NULL_RTX we either don't
   need a clobber reg or have to cook one up.

   LEN == NULL: Output instructions.

   LEN != NULL: Output nothing.  Set *LEN to number of words occupied
                by the insns printed.

   Return "".  */

const char *
output_reload_insisf (rtx *op, rtx clobber_reg, int *len)
{
  if (AVR_HAVE_MOVW
      && !test_hard_reg_class (LD_REGS, op[0])
      && (CONST_INT_P (op[1])
          || CONST_FIXED_P (op[1])
          || CONST_DOUBLE_P (op[1])))
    {
      int len_clr, len_noclr;

      /* In some cases it is better to clear the destination beforehand, e.g.

             CLR R2   CLR R3   MOVW R4,R2   INC R2

         is shorther than

             CLR R2   INC R2   CLR  R3      CLR R4   CLR R5

         We find it too tedious to work that out in the print function.
         Instead, we call the print function twice to get the lengths of
         both methods and use the shortest one.  */

      output_reload_in_const (op, clobber_reg, &len_clr, true);
      output_reload_in_const (op, clobber_reg, &len_noclr, false);

      if (len_noclr - len_clr == 4)
        {
          /* Default needs 4 CLR instructions: clear register beforehand.  */

          avr_asm_len ("mov %A0,__zero_reg__" CR_TAB
                       "mov %B0,__zero_reg__" CR_TAB
                       "movw %C0,%A0", &op[0], len, 3);

          output_reload_in_const (op, clobber_reg, len, true);

          if (len)
            *len += 3;

          return "";
        }
    }

  /* Default: destination not pre-cleared.  */

  output_reload_in_const (op, clobber_reg, len, false);
  return "";
}

const char*
avr_out_reload_inpsi (rtx *op, rtx clobber_reg, int *len)
{
  output_reload_in_const (op, clobber_reg, len, false);
  return "";
}


/* Worker function for `ASM_OUTPUT_ADDR_VEC'.  */
/* Emit jump tables out-of-line so that branches crossing the table
   get shorter offsets.  If we have JUMP + CALL, then put the tables
   in a dedicated non-.text section so that CALLs get better chance to
   be relaxed to RCALLs.

   We emit the tables by hand because `function_rodata_section' does not
   work as expected, cf. PR71151, and we do *NOT* want the table to be
   in .rodata, hence setting JUMP_TABLES_IN_TEXT_SECTION = 0 is of limited
   use; and setting it to 1 attributes table lengths to branch offsets...
   Moreover, fincal.c keeps switching section before each table entry
   which we find too fragile as to rely on section caching.  */

void
avr_output_addr_vec (rtx_insn *labl, rtx table)
{
  FILE *stream = asm_out_file;

  app_disable();

  // Switch to appropriate (sub)section.

  if (DECL_SECTION_NAME (current_function_decl)
      && symtab_node::get (current_function_decl)
      && ! symtab_node::get (current_function_decl)->implicit_section)
    {
      // .subsection will emit the code after the function and in the
      // section as chosen by the user.

      switch_to_section (current_function_section ());
      fprintf (stream, "\t.subsection\t1\n");
    }
  else
    {
      // Since PR63223 there is no restriction where to put the table; it
      // may even reside above 128 KiB.  We put it in a section as high as
      // possible and avoid progmem in order not to waste flash <= 64 KiB.

      const char *sec_name = ".jumptables.gcc";

      // The table belongs to its host function, therefore use fine
      // grained sections so that, if that function is removed by
      // --gc-sections, the child table(s) may also be removed.  */

      tree asm_name = DECL_ASSEMBLER_NAME (current_function_decl);
      const char *fname = IDENTIFIER_POINTER (asm_name);
      fname = targetm.strip_name_encoding (fname);
      sec_name = ACONCAT ((sec_name, ".", fname, NULL));

      fprintf (stream, "\t.section\t%s,\"%s\",@progbits\n", sec_name,
               AVR_HAVE_JMP_CALL ? "a" : "ax");
    }

  // Output the label that preceeds the table.

  ASM_OUTPUT_ALIGN (stream, 1);
  targetm.asm_out.internal_label (stream, "L", CODE_LABEL_NUMBER (labl));

  // Output the table's content.

  int vlen = XVECLEN (table, 0);

  for (int idx = 0; idx < vlen; idx++)
    {
      int value = CODE_LABEL_NUMBER (XEXP (XVECEXP (table, 0, idx), 0));

      if (AVR_HAVE_JMP_CALL)
        fprintf (stream, "\t.word gs(.L%d)\n", value);
      else
        fprintf (stream, "\trjmp .L%d\n", value);
    }

  // Switch back to original section.  As we clobbered the section above,
  // forget the current section before switching back.

  in_section = NULL;
  switch_to_section (current_function_section ());
}


/* Implement `TARGET_CONDITIONAL_REGISTER_USAGE'.  */

static void
avr_conditional_register_usage (void)
{
  if (AVR_TINY)
    {
      const int tiny_reg_alloc_order[] = {
        24, 25,
        22, 23,
        30, 31,
        26, 27,
        28, 29,
        21, 20, 19, 18,
        16, 17,
        32, 33, 34, 35,
        15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
      };

      /* Set R0-R17 as fixed registers. Reset R0-R17 in call used register list
         - R0-R15 are not available in Tiny Core devices
         - R16 and R17 are fixed registers.  */

      for (size_t i = 0; i <= 17;  i++)
        {
          fixed_regs[i] = 1;
          call_used_regs[i] = 1;
        }

      /* Set R18 to R21 as callee saved registers
         - R18, R19, R20 and R21 are the callee saved registers in
           Tiny Core devices  */

      for (size_t i = 18; i <= LAST_CALLEE_SAVED_REG; i++)
        {
          call_used_regs[i] = 0;
        }

      /* Update register allocation order for Tiny Core devices */

      for (size_t i = 0; i < ARRAY_SIZE (tiny_reg_alloc_order); i++)
        {
          reg_alloc_order[i] = tiny_reg_alloc_order[i];
        }

      CLEAR_HARD_REG_SET (reg_class_contents[(int) ADDW_REGS]);
      CLEAR_HARD_REG_SET (reg_class_contents[(int) NO_LD_REGS]);
    }
}

/* Implement `TARGET_HARD_REGNO_SCRATCH_OK'.  */
/* Returns true if SCRATCH are safe to be allocated as a scratch
   registers (for a define_peephole2) in the current function.  */

static bool
avr_hard_regno_scratch_ok (unsigned int regno)
{
  /* Interrupt functions can only use registers that have already been saved
     by the prologue, even if they would normally be call-clobbered.  */

  if ((cfun->machine->is_interrupt || cfun->machine->is_signal)
      && !df_regs_ever_live_p (regno))
    return false;

  /* Don't allow hard registers that might be part of the frame pointer.
     Some places in the compiler just test for [HARD_]FRAME_POINTER_REGNUM
     and don't care for a frame pointer that spans more than one register.  */

  if ((!reload_completed || frame_pointer_needed)
      && (regno == REG_Y || regno == REG_Y + 1))
    {
      return false;
    }

  return true;
}


/* Worker function for `HARD_REGNO_RENAME_OK'.  */
/* Return nonzero if register OLD_REG can be renamed to register NEW_REG.  */

int
avr_hard_regno_rename_ok (unsigned int old_reg,
			  unsigned int new_reg)
{
  /* Interrupt functions can only use registers that have already been
     saved by the prologue, even if they would normally be
     call-clobbered.  */

  if ((cfun->machine->is_interrupt || cfun->machine->is_signal)
      && !df_regs_ever_live_p (new_reg))
    return 0;

  /* Don't allow hard registers that might be part of the frame pointer.
     Some places in the compiler just test for [HARD_]FRAME_POINTER_REGNUM
     and don't care for a frame pointer that spans more than one register.  */

  if ((!reload_completed || frame_pointer_needed)
      && (old_reg == REG_Y || old_reg == REG_Y + 1
          || new_reg == REG_Y || new_reg == REG_Y + 1))
    {
      return 0;
    }

  return 1;
}

/* Output a branch that tests a single bit of a register (QI, HI, SI or DImode)
   or memory location in the I/O space (QImode only).

   Operand 0: comparison operator (must be EQ or NE, compare bit to zero).
   Operand 1: register operand to test, or CONST_INT memory address.
   Operand 2: bit number.
   Operand 3: label to jump to if the test is true.  */

const char*
avr_out_sbxx_branch (rtx_insn *insn, rtx operands[])
{
  enum rtx_code comp = GET_CODE (operands[0]);
  bool long_jump = get_attr_length (insn) >= 4;
  bool reverse = long_jump || jump_over_one_insn_p (insn, operands[3]);

  if (comp == GE)
    comp = EQ;
  else if (comp == LT)
    comp = NE;

  if (reverse)
    comp = reverse_condition (comp);

  switch (GET_CODE (operands[1]))
    {
    default:
      gcc_unreachable();

    case CONST_INT:
    case CONST:
    case SYMBOL_REF:

      if (low_io_address_operand (operands[1], QImode))
        {
          if (comp == EQ)
            output_asm_insn ("sbis %i1,%2", operands);
          else
            output_asm_insn ("sbic %i1,%2", operands);
        }
      else
        {
	  gcc_assert (io_address_operand (operands[1], QImode));
          output_asm_insn ("in __tmp_reg__,%i1", operands);
          if (comp == EQ)
            output_asm_insn ("sbrs __tmp_reg__,%2", operands);
          else
            output_asm_insn ("sbrc __tmp_reg__,%2", operands);
        }

      break; /* CONST_INT */

    case REG:

      if (comp == EQ)
        output_asm_insn ("sbrs %T1%T2", operands);
      else
        output_asm_insn ("sbrc %T1%T2", operands);

      break; /* REG */
    }        /* switch */

  if (long_jump)
    return ("rjmp .+4" CR_TAB
            "jmp %x3");

  if (!reverse)
    return "rjmp %x3";

  return "";
}

/* Worker function for `TARGET_ASM_CONSTRUCTOR'.  */

static void
avr_asm_out_ctor (rtx symbol, int priority)
{
  fputs ("\t.global __do_global_ctors\n", asm_out_file);
  default_ctor_section_asm_out_constructor (symbol, priority);
}


/* Worker function for `TARGET_ASM_DESTRUCTOR'.  */

static void
avr_asm_out_dtor (rtx symbol, int priority)
{
  fputs ("\t.global __do_global_dtors\n", asm_out_file);
  default_dtor_section_asm_out_destructor (symbol, priority);
}


/* Worker function for `TARGET_RETURN_IN_MEMORY'.  */

static bool
avr_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT size = int_size_in_bytes (type);
  HOST_WIDE_INT ret_size_limit = AVR_TINY ? 4 : 8;

  /* In avr, there are 8 return registers. But, for Tiny Core
     (ATtiny4/5/9/10/20/40) devices, only 4 registers are available.
     Return true if size is unknown or greater than the limit.  */

  if (size == -1 || size > ret_size_limit)
    {
      return true;
    }
  else
    {
      return false;
    }
}


/* Implement `CASE_VALUES_THRESHOLD'.  */
/* Supply the default for --param case-values-threshold=0  */

static unsigned int
avr_case_values_threshold (void)
{
  /* The exact break-even point between a jump table and an if-else tree
     depends on several factors not available here like, e.g. if 8-bit
     comparisons can be used in the if-else tree or not, on the
     range of the case values, if the case value can be reused, on the
     register allocation, etc.  '7' appears to be a good choice.  */

  return 7;
}


/* Implement `TARGET_ADDR_SPACE_ADDRESS_MODE'.  */

static scalar_int_mode
avr_addr_space_address_mode (addr_space_t as)
{
  return avr_addrspace[as].pointer_size == 3 ? PSImode : HImode;
}


/* Implement `TARGET_ADDR_SPACE_POINTER_MODE'.  */

static scalar_int_mode
avr_addr_space_pointer_mode (addr_space_t as)
{
  return avr_addr_space_address_mode (as);
}


/* Helper for following function.  */

static bool
avr_reg_ok_for_pgm_addr (rtx reg, bool strict)
{
  gcc_assert (REG_P (reg));

  if (strict)
    {
      return REGNO (reg) == REG_Z;
    }

  /* Avoid combine to propagate hard regs.  */

  if (can_create_pseudo_p()
      && REGNO (reg) < REG_Z)
    {
      return false;
    }

  return true;
}


/* Implement `TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P'.  */

static bool
avr_addr_space_legitimate_address_p (machine_mode mode, rtx x,
                                     bool strict, addr_space_t as)
{
  bool ok = false;

  switch (as)
    {
    default:
      gcc_unreachable();

    case ADDR_SPACE_GENERIC:
      return avr_legitimate_address_p (mode, x, strict);

    case ADDR_SPACE_FLASH:
    case ADDR_SPACE_FLASH1:
    case ADDR_SPACE_FLASH2:
    case ADDR_SPACE_FLASH3:
    case ADDR_SPACE_FLASH4:
    case ADDR_SPACE_FLASH5:

      switch (GET_CODE (x))
        {
        case REG:
          ok = avr_reg_ok_for_pgm_addr (x, strict);
          break;

        case POST_INC:
          ok = avr_reg_ok_for_pgm_addr (XEXP (x, 0), strict);
          break;

        default:
          break;
        }

      break; /* FLASH */

    case ADDR_SPACE_MEMX:
      if (REG_P (x))
        ok = (!strict
              && can_create_pseudo_p());

      if (LO_SUM == GET_CODE (x))
        {
          rtx hi = XEXP (x, 0);
          rtx lo = XEXP (x, 1);

          ok = (REG_P (hi)
                && (!strict || REGNO (hi) < FIRST_PSEUDO_REGISTER)
                && REG_P (lo)
                && REGNO (lo) == REG_Z);
        }

      break; /* MEMX */
    }

  if (avr_log.legitimate_address_p)
    {
      avr_edump ("\n%?: ret=%b, mode=%m strict=%d "
                 "reload_completed=%d reload_in_progress=%d %s:",
                 ok, mode, strict, reload_completed, reload_in_progress,
                 reg_renumber ? "(reg_renumber)" : "");

      if (GET_CODE (x) == PLUS
          && REG_P (XEXP (x, 0))
          && CONST_INT_P (XEXP (x, 1))
          && IN_RANGE (INTVAL (XEXP (x, 1)), 0, MAX_LD_OFFSET (mode))
          && reg_renumber)
        {
          avr_edump ("(r%d ---> r%d)", REGNO (XEXP (x, 0)),
                     true_regnum (XEXP (x, 0)));
        }

      avr_edump ("\n%r\n", x);
    }

  return ok;
}


/* Implement `TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS'.  */

static rtx
avr_addr_space_legitimize_address (rtx x, rtx old_x,
                                   machine_mode mode, addr_space_t as)
{
  if (ADDR_SPACE_GENERIC_P (as))
    return avr_legitimize_address (x, old_x, mode);

  if (avr_log.legitimize_address)
    {
      avr_edump ("\n%?: mode=%m\n %r\n", mode, old_x);
    }

  return old_x;
}


/* Implement `TARGET_ADDR_SPACE_CONVERT'.  */

static rtx
avr_addr_space_convert (rtx src, tree type_from, tree type_to)
{
  addr_space_t as_from = TYPE_ADDR_SPACE (TREE_TYPE (type_from));
  addr_space_t as_to = TYPE_ADDR_SPACE (TREE_TYPE (type_to));

  if (avr_log.progmem)
    avr_edump ("\n%!: op = %r\nfrom = %t\nto = %t\n",
               src, type_from, type_to);

  /* Up-casting from 16-bit to 24-bit pointer.  */

  if (as_from != ADDR_SPACE_MEMX
      && as_to == ADDR_SPACE_MEMX)
    {
      int msb;
      rtx sym = src;
      rtx reg = gen_reg_rtx (PSImode);

      while (CONST == GET_CODE (sym) || PLUS == GET_CODE (sym))
        sym = XEXP (sym, 0);

      /* Look at symbol flags:  avr_encode_section_info set the flags
         also if attribute progmem was seen so that we get the right
         promotion for, e.g. PSTR-like strings that reside in generic space
         but are located in flash.  In that case we patch the incoming
         address space.  */

      if (SYMBOL_REF_P (sym)
          && ADDR_SPACE_FLASH == AVR_SYMBOL_GET_ADDR_SPACE (sym))
        {
          as_from = ADDR_SPACE_FLASH;
        }

      /* Linearize memory: RAM has bit 23 set.  */

      msb = ADDR_SPACE_GENERIC_P (as_from)
        ? 0x80
        : avr_addrspace[as_from].segment;

      src = force_reg (Pmode, src);

      emit_insn (msb == 0
                 ? gen_zero_extendhipsi2 (reg, src)
                 : gen_n_extendhipsi2 (reg, gen_int_mode (msb, QImode), src));

      return reg;
    }

  /* Down-casting from 24-bit to 16-bit throws away the high byte.  */

  if (as_from == ADDR_SPACE_MEMX
      && as_to != ADDR_SPACE_MEMX)
    {
      rtx new_src = gen_reg_rtx (Pmode);

      src = force_reg (PSImode, src);

      emit_move_insn (new_src,
                      simplify_gen_subreg (Pmode, src, PSImode, 0));
      return new_src;
    }

  return src;
}


/* Implement `TARGET_ADDR_SPACE_SUBSET_P'.  */

static bool
avr_addr_space_subset_p (addr_space_t subset ATTRIBUTE_UNUSED,
                         addr_space_t superset ATTRIBUTE_UNUSED)
{
  /* Allow any kind of pointer mess.  */

  return true;
}


/* Implement `TARGET_CONVERT_TO_TYPE'.  */

static tree
avr_convert_to_type (tree type, tree expr)
{
  /* Print a diagnose for pointer conversion that changes the address
     space of the pointer target to a non-enclosing address space,
     provided -Waddr-space-convert is on.

     FIXME: Filter out cases where the target object is known to
            be located in the right memory, like in

                (const __flash*) PSTR ("text")

            Also try to distinguish between explicit casts requested by
            the user and implicit casts like

                void f (const __flash char*);

                void g (const char *p)
                {
                    f ((const __flash*) p);
                }

            under the assumption that an explicit casts means that the user
            knows what he is doing, e.g. interface with PSTR or old style
            code with progmem and pgm_read_xxx.
  */

  if (avr_warn_addr_space_convert
      && expr != error_mark_node
      && POINTER_TYPE_P (type)
      && POINTER_TYPE_P (TREE_TYPE (expr)))
    {
      addr_space_t as_old = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (expr)));
      addr_space_t as_new = TYPE_ADDR_SPACE (TREE_TYPE (type));

      if (avr_log.progmem)
        avr_edump ("%?: type = %t\nexpr = %t\n\n", type, expr);

      if (as_new != ADDR_SPACE_MEMX
          && as_new != as_old)
        {
          location_t loc = EXPR_LOCATION (expr);
          const char *name_old = avr_addrspace[as_old].name;
          const char *name_new = avr_addrspace[as_new].name;

          warning (OPT_Waddr_space_convert,
                   "conversion from address space %qs to address space %qs",
                   ADDR_SPACE_GENERIC_P (as_old) ? "generic" : name_old,
                   ADDR_SPACE_GENERIC_P (as_new) ? "generic" : name_new);

          return fold_build1_loc (loc, ADDR_SPACE_CONVERT_EXPR, type, expr);
        }
    }

  return NULL_TREE;
}


/* Implement `TARGET_LEGITIMATE_COMBINED_INSN'.  */

/* PR78883: Filter out paradoxical SUBREGs of MEM which are not handled
   properly by following passes.  As INSN_SCHEDULING is off and hence
   general_operand accepts such expressions, ditch them now.  */

static bool
avr_legitimate_combined_insn (rtx_insn *insn)
{
  subrtx_iterator::array_type array;

  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
    {
      const_rtx op = *iter;

      if (SUBREG_P (op)
          && MEM_P (SUBREG_REG (op))
          && (GET_MODE_SIZE (GET_MODE (op))
              > GET_MODE_SIZE (GET_MODE (SUBREG_REG (op)))))
        {
          return false;
        }
    }

  return true;
}


/* PR63633: The middle-end might come up with hard regs as input operands.

   RMASK is a bit mask representing a subset of hard registers R0...R31:
   Rn is an element of that set iff bit n of RMASK is set.
   OPMASK describes a subset of OP[]:  If bit n of OPMASK is 1 then
   OP[n] has to be fixed; otherwise OP[n] is left alone.

   For each element of OPMASK which is a hard register overlapping RMASK,
   replace OP[n] with a newly created pseudo register

   HREG == 0:  Also emit a move insn that copies the contents of that
               hard register into the new pseudo.

   HREG != 0:  Also set HREG[n] to the hard register.  */

static void
avr_fix_operands (rtx *op, rtx *hreg, unsigned opmask, unsigned rmask)
{
  for (; opmask; opmask >>= 1, op++)
    {
      rtx reg = *op;

      if (hreg)
        *hreg = NULL_RTX;

      if ((opmask & 1)
          && REG_P (reg)
          && REGNO (reg) < FIRST_PSEUDO_REGISTER
          // This hard-reg overlaps other prohibited hard regs?
          && (rmask & regmask (GET_MODE (reg), REGNO (reg))))
        {
          *op = gen_reg_rtx (GET_MODE (reg));
          if (hreg == NULL)
            emit_move_insn (*op, reg);
          else
            *hreg = reg;
        }

      if (hreg)
        hreg++;
    }
}


void
avr_fix_inputs (rtx *op, unsigned opmask, unsigned rmask)
{
  avr_fix_operands (op, NULL, opmask, rmask);
}


/* Helper for the function below:  If bit n of MASK is set and
   HREG[n] != NULL, then emit a move insn to copy OP[n] to HREG[n].
   Otherwise do nothing for that n.  Return TRUE.  */

static bool
avr_move_fixed_operands (rtx *op, rtx *hreg, unsigned mask)
{
  for (; mask; mask >>= 1, op++, hreg++)
    if ((mask & 1)
        && *hreg)
      emit_move_insn (*hreg, *op);

  return true;
}


/* PR63633: The middle-end might come up with hard regs as output operands.

   GEN is a sequence generating function like gen_mulsi3 with 3 operands OP[].
   RMASK is a bit mask representing a subset of hard registers R0...R31:
   Rn is an element of that set iff bit n of RMASK is set.
   OPMASK describes a subset of OP[]:  If bit n of OPMASK is 1 then
   OP[n] has to be fixed; otherwise OP[n] is left alone.

   Emit the insn sequence as generated by GEN() with all elements of OPMASK
   which are hard registers overlapping RMASK replaced by newly created
   pseudo registers.  After the sequence has been emitted, emit insns that
   move the contents of respective pseudos to their hard regs.  */

bool
avr_emit3_fix_outputs (rtx (*gen)(rtx,rtx,rtx), rtx *op,
                       unsigned opmask, unsigned rmask)
{
  const int n = 3;
  rtx hreg[n];

  /* It is letigimate for GEN to call this function, and in order not to
     get self-recursive we use the following static kludge.  This is the
     only way not to duplicate all expanders and to avoid ugly and
     hard-to-maintain C-code instead of the much more appreciated RTL
     representation as supplied by define_expand.  */
  static bool lock = false;

  gcc_assert (opmask < (1u << n));

  if (lock)
    return false;

  avr_fix_operands (op, hreg, opmask, rmask);

  lock = true;
  emit_insn (gen (op[0], op[1], op[2]));
  lock = false;

  return avr_move_fixed_operands (op, hreg, opmask);
}


/* Worker function for movmemhi expander.
   XOP[0]  Destination as MEM:BLK
   XOP[1]  Source      "     "
   XOP[2]  # Bytes to copy

   Return TRUE  if the expansion is accomplished.
   Return FALSE if the operand compination is not supported.  */

bool
avr_emit_movmemhi (rtx *xop)
{
  HOST_WIDE_INT count;
  machine_mode loop_mode;
  addr_space_t as = MEM_ADDR_SPACE (xop[1]);
  rtx loop_reg, addr1, a_src, a_dest, insn, xas;
  rtx a_hi8 = NULL_RTX;

  if (avr_mem_flash_p (xop[0]))
    return false;

  if (!CONST_INT_P (xop[2]))
    return false;

  count = INTVAL (xop[2]);
  if (count <= 0)
    return false;

  a_src  = XEXP (xop[1], 0);
  a_dest = XEXP (xop[0], 0);

  if (PSImode == GET_MODE (a_src))
    {
      gcc_assert (as == ADDR_SPACE_MEMX);

      loop_mode = (count < 0x100) ? QImode : HImode;
      loop_reg = gen_rtx_REG (loop_mode, 24);
      emit_move_insn (loop_reg, gen_int_mode (count, loop_mode));

      addr1 = simplify_gen_subreg (HImode, a_src, PSImode, 0);
      a_hi8 = simplify_gen_subreg (QImode, a_src, PSImode, 2);
    }
  else
    {
      int segment = avr_addrspace[as].segment;

      if (segment
          && avr_n_flash > 1)
        {
          a_hi8 = GEN_INT (segment);
          emit_move_insn (rampz_rtx, a_hi8 = copy_to_mode_reg (QImode, a_hi8));
        }
      else if (!ADDR_SPACE_GENERIC_P (as))
        {
          as = ADDR_SPACE_FLASH;
        }

      addr1 = a_src;

      loop_mode = (count <= 0x100) ? QImode : HImode;
      loop_reg = copy_to_mode_reg (loop_mode, gen_int_mode (count, loop_mode));
    }

  xas = GEN_INT (as);

  /* FIXME: Register allocator might come up with spill fails if it is left
        on its own.  Thus, we allocate the pointer registers by hand:
        Z = source address
        X = destination address  */

  emit_move_insn (lpm_addr_reg_rtx, addr1);
  emit_move_insn (gen_rtx_REG (HImode, REG_X), a_dest);

  /* FIXME: Register allocator does a bad job and might spill address
        register(s) inside the loop leading to additional move instruction
        to/from stack which could clobber tmp_reg.  Thus, do *not* emit
        load and store as separate insns.  Instead, we perform the copy
        by means of one monolithic insn.  */

  gcc_assert (TMP_REGNO == LPM_REGNO);

  if (as != ADDR_SPACE_MEMX)
    {
      /* Load instruction ([E]LPM or LD) is known at compile time:
         Do the copy-loop inline.  */

      rtx (*fun) (rtx, rtx, rtx)
        = QImode == loop_mode ? gen_movmem_qi : gen_movmem_hi;

      insn = fun (xas, loop_reg, loop_reg);
    }
  else
    {
      rtx (*fun) (rtx, rtx)
        = QImode == loop_mode ? gen_movmemx_qi : gen_movmemx_hi;

      emit_move_insn (gen_rtx_REG (QImode, 23), a_hi8);

      insn = fun (xas, GEN_INT (avr_addr.rampz));
    }

  set_mem_addr_space (SET_SRC (XVECEXP (insn, 0, 0)), as);
  emit_insn (insn);

  return true;
}


/* Print assembler for movmem_qi, movmem_hi insns...
       $0     : Address Space
       $1, $2 : Loop register
       Z      : Source address
       X      : Destination address
*/

const char*
avr_out_movmem (rtx_insn *insn ATTRIBUTE_UNUSED, rtx *op, int *plen)
{
  addr_space_t as = (addr_space_t) INTVAL (op[0]);
  machine_mode loop_mode = GET_MODE (op[1]);
  bool sbiw_p = test_hard_reg_class (ADDW_REGS, op[1]);
  rtx xop[3];

  if (plen)
    *plen = 0;

  xop[0] = op[0];
  xop[1] = op[1];
  xop[2] = tmp_reg_rtx;

  /* Loop label */

  avr_asm_len ("0:", xop, plen, 0);

  /* Load with post-increment */

  switch (as)
    {
    default:
      gcc_unreachable();

    case ADDR_SPACE_GENERIC:

      avr_asm_len ("ld %2,Z+", xop, plen, 1);
      break;

    case ADDR_SPACE_FLASH:

      if (AVR_HAVE_LPMX)
        avr_asm_len ("lpm %2,Z+", xop, plen, 1);
      else
        avr_asm_len ("lpm" CR_TAB
                     "adiw r30,1", xop, plen, 2);
      break;

    case ADDR_SPACE_FLASH1:
    case ADDR_SPACE_FLASH2:
    case ADDR_SPACE_FLASH3:
    case ADDR_SPACE_FLASH4:
    case ADDR_SPACE_FLASH5:

      if (AVR_HAVE_ELPMX)
        avr_asm_len ("elpm %2,Z+", xop, plen, 1);
      else
        avr_asm_len ("elpm" CR_TAB
                     "adiw r30,1", xop, plen, 2);
      break;
    }

  /* Store with post-increment */

  avr_asm_len ("st X+,%2", xop, plen, 1);

  /* Decrement loop-counter and set Z-flag */

  if (QImode == loop_mode)
    {
      avr_asm_len ("dec %1", xop, plen, 1);
    }
  else if (sbiw_p)
    {
      avr_asm_len ("sbiw %1,1", xop, plen, 1);
    }
  else
    {
      avr_asm_len ("subi %A1,1" CR_TAB
                   "sbci %B1,0", xop, plen, 2);
    }

  /* Loop until zero */

  return avr_asm_len ("brne 0b", xop, plen, 1);
}



/* Helper for __builtin_avr_delay_cycles */

static rtx
avr_mem_clobber (void)
{
  rtx mem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (mem) = 1;
  return mem;
}

static void
avr_expand_delay_cycles (rtx operands0)
{
  unsigned HOST_WIDE_INT cycles = UINTVAL (operands0) & GET_MODE_MASK (SImode);
  unsigned HOST_WIDE_INT cycles_used;
  unsigned HOST_WIDE_INT loop_count;

  if (IN_RANGE (cycles, 83886082, 0xFFFFFFFF))
    {
      loop_count = ((cycles - 9) / 6) + 1;
      cycles_used = ((loop_count - 1) * 6) + 9;
      emit_insn (gen_delay_cycles_4 (gen_int_mode (loop_count, SImode),
                                     avr_mem_clobber()));
      cycles -= cycles_used;
    }

  if (IN_RANGE (cycles, 262145, 83886081))
    {
      loop_count = ((cycles - 7) / 5) + 1;
      if (loop_count > 0xFFFFFF)
        loop_count = 0xFFFFFF;
      cycles_used = ((loop_count - 1) * 5) + 7;
      emit_insn (gen_delay_cycles_3 (gen_int_mode (loop_count, SImode),
                                     avr_mem_clobber()));
      cycles -= cycles_used;
    }

  if (IN_RANGE (cycles, 768, 262144))
    {
      loop_count = ((cycles - 5) / 4) + 1;
      if (loop_count > 0xFFFF)
        loop_count = 0xFFFF;
      cycles_used = ((loop_count - 1) * 4) + 5;
      emit_insn (gen_delay_cycles_2 (gen_int_mode (loop_count, HImode),
                                     avr_mem_clobber()));
      cycles -= cycles_used;
    }

  if (IN_RANGE (cycles, 6, 767))
    {
      loop_count = cycles / 3;
      if (loop_count > 255)
        loop_count = 255;
      cycles_used = loop_count * 3;
      emit_insn (gen_delay_cycles_1 (gen_int_mode (loop_count, QImode),
                                     avr_mem_clobber()));
      cycles -= cycles_used;
    }

  while (cycles >= 2)
    {
      emit_insn (gen_nopv (GEN_INT (2)));
      cycles -= 2;
    }

  if (cycles == 1)
    {
      emit_insn (gen_nopv (GEN_INT (1)));
      cycles--;
    }
}


static void
avr_expand_nops (rtx operands0)
{
  unsigned HOST_WIDE_INT n_nops = UINTVAL (operands0) & GET_MODE_MASK (HImode);

  while (n_nops--)
    {
      emit_insn (gen_nopv (const1_rtx));
    }
}


/* Compute the image of x under f, i.e. perform   x --> f(x)    */

static int
avr_map (unsigned int f, int x)
{
  return x < 8 ? (f >> (4 * x)) & 0xf : 0;
}


/* Return some metrics of map A.  */

enum
  {
    /* Number of fixed points in { 0 ... 7 } */
    MAP_FIXED_0_7,

    /* Size of preimage of non-fixed points in { 0 ... 7 } */
    MAP_NONFIXED_0_7,

    /* Mask representing the fixed points in { 0 ... 7 } */
    MAP_MASK_FIXED_0_7,

    /* Size of the preimage of { 0 ... 7 } */
    MAP_PREIMAGE_0_7,

    /* Mask that represents the preimage of { f } */
    MAP_MASK_PREIMAGE_F
  };

static unsigned
avr_map_metric (unsigned int a, int mode)
{
  unsigned metric = 0;

  for (unsigned i = 0; i < 8; i++)
    {
      unsigned ai = avr_map (a, i);

      if (mode == MAP_FIXED_0_7)
        metric += ai == i;
      else if (mode == MAP_NONFIXED_0_7)
        metric += ai < 8 && ai != i;
      else if (mode == MAP_MASK_FIXED_0_7)
        metric |= ((unsigned) (ai == i)) << i;
      else if (mode == MAP_PREIMAGE_0_7)
        metric += ai < 8;
      else if (mode == MAP_MASK_PREIMAGE_F)
        metric |= ((unsigned) (ai == 0xf)) << i;
      else
        gcc_unreachable();
    }

  return metric;
}


/* Return true if IVAL has a 0xf in its hexadecimal representation
   and false, otherwise.  Only nibbles 0..7 are taken into account.
   Used as constraint helper for C0f and Cxf.  */

bool
avr_has_nibble_0xf (rtx ival)
{
  unsigned int map = UINTVAL (ival) & GET_MODE_MASK (SImode);
  return 0 != avr_map_metric (map, MAP_MASK_PREIMAGE_F);
}


/* We have a set of bits that are mapped by a function F.
   Try to decompose F by means of a second function G so that

      F = F o G^-1 o G

   and

      cost (F o G^-1) + cost (G)  <  cost (F)

   Example:  Suppose builtin insert_bits supplies us with the map
   F = 0x3210ffff.  Instead of doing 4 bit insertions to get the high
   nibble of the result, we can just as well rotate the bits before inserting
   them and use the map 0x7654ffff which is cheaper than the original map.
   For this example G = G^-1 = 0x32107654 and F o G^-1 = 0x7654ffff.  */

typedef struct
{
  /* tree code of binary function G */
  enum tree_code code;

  /* The constant second argument of G */
  int arg;

  /* G^-1, the inverse of G (*, arg) */
  unsigned ginv;

  /* The cost of applying G (*, arg) */
  int cost;

  /* The composition F o G^-1 (*, arg) for some function F */
  unsigned int map;

  /* For debug purpose only */
  const char *str;
} avr_map_op_t;

static const avr_map_op_t avr_map_op[] =
  {
    { LROTATE_EXPR, 0, 0x76543210, 0, 0, "id" },
    { LROTATE_EXPR, 1, 0x07654321, 2, 0, "<<<" },
    { LROTATE_EXPR, 2, 0x10765432, 4, 0, "<<<" },
    { LROTATE_EXPR, 3, 0x21076543, 4, 0, "<<<" },
    { LROTATE_EXPR, 4, 0x32107654, 1, 0, "<<<" },
    { LROTATE_EXPR, 5, 0x43210765, 3, 0, "<<<" },
    { LROTATE_EXPR, 6, 0x54321076, 5, 0, "<<<" },
    { LROTATE_EXPR, 7, 0x65432107, 3, 0, "<<<" },
    { RSHIFT_EXPR, 1, 0x6543210c, 1, 0, ">>" },
    { RSHIFT_EXPR, 1, 0x7543210c, 1, 0, ">>" },
    { RSHIFT_EXPR, 2, 0x543210cc, 2, 0, ">>" },
    { RSHIFT_EXPR, 2, 0x643210cc, 2, 0, ">>" },
    { RSHIFT_EXPR, 2, 0x743210cc, 2, 0, ">>" },
    { LSHIFT_EXPR, 1, 0xc7654321, 1, 0, "<<" },
    { LSHIFT_EXPR, 2, 0xcc765432, 2, 0, "<<" }
  };


/* Try to decompose F as F = (F o G^-1) o G as described above.
   The result is a struct representing F o G^-1 and G.
   If result.cost < 0 then such a decomposition does not exist.  */

static avr_map_op_t
avr_map_decompose (unsigned int f, const avr_map_op_t *g, bool val_const_p)
{
  bool val_used_p = 0 != avr_map_metric (f, MAP_MASK_PREIMAGE_F);
  avr_map_op_t f_ginv = *g;
  unsigned int ginv = g->ginv;

  f_ginv.cost = -1;

  /* Step 1:  Computing F o G^-1  */

  for (int i = 7; i >= 0; i--)
    {
      int x = avr_map (f, i);

      if (x <= 7)
        {
          x = avr_map (ginv, x);

          /* The bit is no element of the image of G: no avail (cost = -1)  */

          if (x > 7)
            return f_ginv;
        }

      f_ginv.map = (f_ginv.map << 4) + x;
    }

  /* Step 2:  Compute the cost of the operations.
     The overall cost of doing an operation prior to the insertion is
      the cost of the insertion plus the cost of the operation.  */

  /* Step 2a:  Compute cost of F o G^-1  */

  if (0 == avr_map_metric (f_ginv.map, MAP_NONFIXED_0_7))
    {
      /* The mapping consists only of fixed points and can be folded
         to AND/OR logic in the remainder.  Reasonable cost is 3. */

      f_ginv.cost = 2 + (val_used_p && !val_const_p);
    }
  else
    {
      rtx xop[4];

      /* Get the cost of the insn by calling the output worker with some
         fake values.  Mimic effect of reloading xop[3]: Unused operands
         are mapped to 0 and used operands are reloaded to xop[0].  */

      xop[0] = all_regs_rtx[24];
      xop[1] = gen_int_mode (f_ginv.map, SImode);
      xop[2] = all_regs_rtx[25];
      xop[3] = val_used_p ? xop[0] : const0_rtx;

      avr_out_insert_bits (xop, &f_ginv.cost);

      f_ginv.cost += val_const_p && val_used_p ? 1 : 0;
    }

  /* Step 2b:  Add cost of G  */

  f_ginv.cost += g->cost;

  if (avr_log.builtin)
    avr_edump (" %s%d=%d", g->str, g->arg, f_ginv.cost);

  return f_ginv;
}


/* Insert bits from XOP[1] into XOP[0] according to MAP.
   XOP[0] and XOP[1] don't overlap.
   If FIXP_P = true:  Move all bits according to MAP using BLD/BST sequences.
   If FIXP_P = false: Just move the bit if its position in the destination
   is different to its source position.  */

static void
avr_move_bits (rtx *xop, unsigned int map, bool fixp_p, int *plen)
{
  /* T-flag contains this bit of the source, i.e. of XOP[1]  */
  int t_bit_src = -1;

  /* We order the operations according to the requested source bit b.  */

  for (int b = 0; b < 8; b++)
    for (int bit_dest = 0; bit_dest < 8; bit_dest++)
      {
        int bit_src = avr_map (map, bit_dest);

        if (b != bit_src
            || bit_src >= 8
            /* Same position: No need to copy as requested by FIXP_P.  */
            || (bit_dest == bit_src && !fixp_p))
          continue;

        if (t_bit_src != bit_src)
          {
            /* Source bit is not yet in T: Store it to T.  */

            t_bit_src = bit_src;

            xop[3] = GEN_INT (bit_src);
            avr_asm_len ("bst %T1%T3", xop, plen, 1);
          }

        /* Load destination bit with T.  */

        xop[3] = GEN_INT (bit_dest);
        avr_asm_len ("bld %T0%T3", xop, plen, 1);
      }
}


/* PLEN == 0: Print assembler code for `insert_bits'.
   PLEN != 0: Compute code length in bytes.

   OP[0]:  Result
   OP[1]:  The mapping composed of nibbles. If nibble no. N is
           0:   Bit N of result is copied from bit OP[2].0
           ...  ...
           7:   Bit N of result is copied from bit OP[2].7
           0xf: Bit N of result is copied from bit OP[3].N
   OP[2]:  Bits to be inserted
   OP[3]:  Target value  */

const char*
avr_out_insert_bits (rtx *op, int *plen)
{
  unsigned int map = UINTVAL (op[1]) & GET_MODE_MASK (SImode);
  unsigned mask_fixed;
  bool fixp_p = true;
  rtx xop[4];

  xop[0] = op[0];
  xop[1] = op[2];
  xop[2] = op[3];

  gcc_assert (REG_P (xop[2]) || CONST_INT_P (xop[2]));

  if (plen)
    *plen = 0;
  else if (flag_print_asm_name)
    fprintf (asm_out_file, ASM_COMMENT_START "map = 0x%08x\n", map);

  /* If MAP has fixed points it might be better to initialize the result
     with the bits to be inserted instead of moving all bits by hand.  */

  mask_fixed = avr_map_metric (map, MAP_MASK_FIXED_0_7);

  if (REGNO (xop[0]) == REGNO (xop[1]))
    {
      /* Avoid early-clobber conflicts */

      avr_asm_len ("mov __tmp_reg__,%1", xop, plen, 1);
      xop[1] = tmp_reg_rtx;
      fixp_p = false;
    }

  if (avr_map_metric (map, MAP_MASK_PREIMAGE_F))
    {
      /* XOP[2] is used and reloaded to XOP[0] already */

      int n_fix = 0, n_nofix = 0;

      gcc_assert (REG_P (xop[2]));

      /* Get the code size of the bit insertions; once with all bits
         moved and once with fixed points omitted.  */

      avr_move_bits (xop, map, true, &n_fix);
      avr_move_bits (xop, map, false, &n_nofix);

      if (fixp_p && n_fix - n_nofix > 3)
        {
          xop[3] = gen_int_mode (~mask_fixed, QImode);

          avr_asm_len ("eor %0,%1"   CR_TAB
                       "andi %0,%3"  CR_TAB
                       "eor %0,%1", xop, plen, 3);
          fixp_p = false;
        }
    }
  else
    {
      /* XOP[2] is unused */

      if (fixp_p && mask_fixed)
        {
          avr_asm_len ("mov %0,%1", xop, plen, 1);
          fixp_p = false;
        }
    }

  /* Move/insert remaining bits.  */

  avr_move_bits (xop, map, fixp_p, plen);

  return "";
}


/* IDs for all the AVR builtins.  */

enum avr_builtin_id
  {
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, CODE, LIBNAME)  \
    AVR_BUILTIN_ ## NAME,
#include "builtins.def"
#undef DEF_BUILTIN

    AVR_BUILTIN_COUNT
  };

struct GTY(()) avr_builtin_description
{
  enum insn_code icode;
  int n_args;
  tree fndecl;
};


/* Notice that avr_bdesc[] and avr_builtin_id are initialized in such a way
   that a built-in's ID can be used to access the built-in by means of
   avr_bdesc[ID]  */

static GTY(()) struct avr_builtin_description
avr_bdesc[AVR_BUILTIN_COUNT] =
  {
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, ICODE, LIBNAME)         \
    { (enum insn_code) CODE_FOR_ ## ICODE, N_ARGS, NULL_TREE },
#include "builtins.def"
#undef DEF_BUILTIN
  };


/* Implement `TARGET_BUILTIN_DECL'.  */

static tree
avr_builtin_decl (unsigned id, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (id < AVR_BUILTIN_COUNT)
    return avr_bdesc[id].fndecl;

  return error_mark_node;
}


static void
avr_init_builtin_int24 (void)
{
  tree int24_type  = make_signed_type (GET_MODE_BITSIZE (PSImode));
  tree uint24_type = make_unsigned_type (GET_MODE_BITSIZE (PSImode));

  lang_hooks.types.register_builtin_type (int24_type, "__int24");
  lang_hooks.types.register_builtin_type (uint24_type, "__uint24");
}


/* Implement `TARGET_INIT_BUILTINS' */
/* Set up all builtin functions for this target.  */

static void
avr_init_builtins (void)
{
  tree void_ftype_void
    = build_function_type_list (void_type_node, NULL_TREE);
  tree uchar_ftype_uchar
    = build_function_type_list (unsigned_char_type_node,
                                unsigned_char_type_node,
                                NULL_TREE);
  tree uint_ftype_uchar_uchar
    = build_function_type_list (unsigned_type_node,
                                unsigned_char_type_node,
                                unsigned_char_type_node,
                                NULL_TREE);
  tree int_ftype_char_char
    = build_function_type_list (integer_type_node,
                                char_type_node,
                                char_type_node,
                                NULL_TREE);
  tree int_ftype_char_uchar
    = build_function_type_list (integer_type_node,
                                char_type_node,
                                unsigned_char_type_node,
                                NULL_TREE);
  tree void_ftype_ulong
    = build_function_type_list (void_type_node,
                                long_unsigned_type_node,
                                NULL_TREE);

  tree uchar_ftype_ulong_uchar_uchar
    = build_function_type_list (unsigned_char_type_node,
                                long_unsigned_type_node,
                                unsigned_char_type_node,
                                unsigned_char_type_node,
                                NULL_TREE);

  tree const_memx_void_node
    = build_qualified_type (void_type_node,
                            TYPE_QUAL_CONST
                            | ENCODE_QUAL_ADDR_SPACE (ADDR_SPACE_MEMX));

  tree const_memx_ptr_type_node
    = build_pointer_type_for_mode (const_memx_void_node, PSImode, false);

  tree char_ftype_const_memx_ptr
    = build_function_type_list (char_type_node,
                                const_memx_ptr_type_node,
                                NULL);

#define ITYP(T)                                                         \
  lang_hooks.types.type_for_size (TYPE_PRECISION (T), TYPE_UNSIGNED (T))

#define FX_FTYPE_FX(fx)                                                 \
  tree fx##r_ftype_##fx##r                                              \
    = build_function_type_list (node_##fx##r, node_##fx##r, NULL);      \
  tree fx##k_ftype_##fx##k                                              \
    = build_function_type_list (node_##fx##k, node_##fx##k, NULL)

#define FX_FTYPE_FX_INT(fx)                                             \
  tree fx##r_ftype_##fx##r_int                                          \
    = build_function_type_list (node_##fx##r, node_##fx##r,             \
                                integer_type_node, NULL);               \
  tree fx##k_ftype_##fx##k_int                                          \
    = build_function_type_list (node_##fx##k, node_##fx##k,             \
                                integer_type_node, NULL)

#define INT_FTYPE_FX(fx)                                                \
  tree int_ftype_##fx##r                                                \
    = build_function_type_list (integer_type_node, node_##fx##r, NULL); \
  tree int_ftype_##fx##k                                                \
    = build_function_type_list (integer_type_node, node_##fx##k, NULL)

#define INTX_FTYPE_FX(fx)                                               \
  tree int##fx##r_ftype_##fx##r                                         \
    = build_function_type_list (ITYP (node_##fx##r), node_##fx##r, NULL); \
  tree int##fx##k_ftype_##fx##k                                         \
    = build_function_type_list (ITYP (node_##fx##k), node_##fx##k, NULL)

#define FX_FTYPE_INTX(fx)                                               \
  tree fx##r_ftype_int##fx##r                                           \
    = build_function_type_list (node_##fx##r, ITYP (node_##fx##r), NULL); \
  tree fx##k_ftype_int##fx##k                                           \
    = build_function_type_list (node_##fx##k, ITYP (node_##fx##k), NULL)

  tree node_hr = short_fract_type_node;
  tree node_nr = fract_type_node;
  tree node_lr = long_fract_type_node;
  tree node_llr = long_long_fract_type_node;

  tree node_uhr = unsigned_short_fract_type_node;
  tree node_unr = unsigned_fract_type_node;
  tree node_ulr = unsigned_long_fract_type_node;
  tree node_ullr = unsigned_long_long_fract_type_node;

  tree node_hk = short_accum_type_node;
  tree node_nk = accum_type_node;
  tree node_lk = long_accum_type_node;
  tree node_llk = long_long_accum_type_node;

  tree node_uhk = unsigned_short_accum_type_node;
  tree node_unk = unsigned_accum_type_node;
  tree node_ulk = unsigned_long_accum_type_node;
  tree node_ullk = unsigned_long_long_accum_type_node;


  /* For absfx builtins.  */

  FX_FTYPE_FX (h);
  FX_FTYPE_FX (n);
  FX_FTYPE_FX (l);
  FX_FTYPE_FX (ll);

  /* For roundfx builtins.  */

  FX_FTYPE_FX_INT (h);
  FX_FTYPE_FX_INT (n);
  FX_FTYPE_FX_INT (l);
  FX_FTYPE_FX_INT (ll);

  FX_FTYPE_FX_INT (uh);
  FX_FTYPE_FX_INT (un);
  FX_FTYPE_FX_INT (ul);
  FX_FTYPE_FX_INT (ull);

  /* For countlsfx builtins.  */

  INT_FTYPE_FX (h);
  INT_FTYPE_FX (n);
  INT_FTYPE_FX (l);
  INT_FTYPE_FX (ll);

  INT_FTYPE_FX (uh);
  INT_FTYPE_FX (un);
  INT_FTYPE_FX (ul);
  INT_FTYPE_FX (ull);

  /* For bitsfx builtins.  */

  INTX_FTYPE_FX (h);
  INTX_FTYPE_FX (n);
  INTX_FTYPE_FX (l);
  INTX_FTYPE_FX (ll);

  INTX_FTYPE_FX (uh);
  INTX_FTYPE_FX (un);
  INTX_FTYPE_FX (ul);
  INTX_FTYPE_FX (ull);

  /* For fxbits builtins.  */

  FX_FTYPE_INTX (h);
  FX_FTYPE_INTX (n);
  FX_FTYPE_INTX (l);
  FX_FTYPE_INTX (ll);

  FX_FTYPE_INTX (uh);
  FX_FTYPE_INTX (un);
  FX_FTYPE_INTX (ul);
  FX_FTYPE_INTX (ull);


#define DEF_BUILTIN(NAME, N_ARGS, TYPE, CODE, LIBNAME)                  \
  {                                                                     \
    int id = AVR_BUILTIN_ ## NAME;                                      \
    const char *Name = "__builtin_avr_" #NAME;                          \
    char *name = (char*) alloca (1 + strlen (Name));                    \
                                                                        \
    gcc_assert (id < AVR_BUILTIN_COUNT);                                \
    avr_bdesc[id].fndecl                                                \
      = add_builtin_function (avr_tolower (name, Name), TYPE, id,       \
                              BUILT_IN_MD, LIBNAME, NULL_TREE);         \
  }
#include "builtins.def"
#undef DEF_BUILTIN

  avr_init_builtin_int24 ();
}


/* Subroutine of avr_expand_builtin to expand vanilla builtins
   with non-void result and 1 ... 3 arguments.  */

static rtx
avr_default_expand_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, xop[3];
  int n_args = call_expr_nargs (exp);
  machine_mode tmode = insn_data[icode].operand[0].mode;

  gcc_assert (n_args >= 1 && n_args <= 3);

  if (target == NULL_RTX
      || GET_MODE (target) != tmode
      || !insn_data[icode].operand[0].predicate (target, tmode))
    {
      target = gen_reg_rtx (tmode);
    }

  for (int n = 0; n < n_args; n++)
    {
      tree arg = CALL_EXPR_ARG (exp, n);
      rtx op = expand_expr (arg, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      machine_mode opmode = GET_MODE (op);
      machine_mode mode = insn_data[icode].operand[n + 1].mode;

      if ((opmode == SImode || opmode == VOIDmode) && mode == HImode)
        {
          opmode = HImode;
          op = gen_lowpart (HImode, op);
        }

      /* In case the insn wants input operands in modes different from
         the result, abort.  */

      gcc_assert (opmode == mode || opmode == VOIDmode);

      if (!insn_data[icode].operand[n + 1].predicate (op, mode))
        op = copy_to_mode_reg (mode, op);

      xop[n] = op;
    }

  switch (n_args)
    {
    case 1: pat = GEN_FCN (icode) (target, xop[0]); break;
    case 2: pat = GEN_FCN (icode) (target, xop[0], xop[1]); break;
    case 3: pat = GEN_FCN (icode) (target, xop[0], xop[1], xop[2]); break;

    default:
      gcc_unreachable();
    }

  if (pat == NULL_RTX)
    return NULL_RTX;

  emit_insn (pat);

  return target;
}


/* Implement `TARGET_EXPAND_BUILTIN'.  */
/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
avr_expand_builtin (tree exp, rtx target,
                    rtx subtarget ATTRIBUTE_UNUSED,
                    machine_mode mode ATTRIBUTE_UNUSED,
                    int ignore)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  const char *bname = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  unsigned int id = DECL_FUNCTION_CODE (fndecl);
  const struct avr_builtin_description *d = &avr_bdesc[id];
  tree arg0;
  rtx op0;

  gcc_assert (id < AVR_BUILTIN_COUNT);

  switch (id)
    {
    case AVR_BUILTIN_NOP:
      emit_insn (gen_nopv (GEN_INT (1)));
      return 0;

    case AVR_BUILTIN_DELAY_CYCLES:
      {
        arg0 = CALL_EXPR_ARG (exp, 0);
        op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);

        if (!CONST_INT_P (op0))
          error ("%s expects a compile time integer constant", bname);
        else
          avr_expand_delay_cycles (op0);

        return NULL_RTX;
      }

    case AVR_BUILTIN_NOPS:
      {
        arg0 = CALL_EXPR_ARG (exp, 0);
        op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);

        if (!CONST_INT_P (op0))
          error ("%s expects a compile time integer constant", bname);
        else
          avr_expand_nops (op0);

        return NULL_RTX;
      }

    case AVR_BUILTIN_INSERT_BITS:
      {
        arg0 = CALL_EXPR_ARG (exp, 0);
        op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);

        if (!CONST_INT_P (op0))
          {
            error ("%s expects a compile time long integer constant"
                   " as first argument", bname);
            return target;
          }

        break;
      }

    case AVR_BUILTIN_ROUNDHR:   case AVR_BUILTIN_ROUNDUHR:
    case AVR_BUILTIN_ROUNDR:    case AVR_BUILTIN_ROUNDUR:
    case AVR_BUILTIN_ROUNDLR:   case AVR_BUILTIN_ROUNDULR:
    case AVR_BUILTIN_ROUNDLLR:  case AVR_BUILTIN_ROUNDULLR:

    case AVR_BUILTIN_ROUNDHK:   case AVR_BUILTIN_ROUNDUHK:
    case AVR_BUILTIN_ROUNDK:    case AVR_BUILTIN_ROUNDUK:
    case AVR_BUILTIN_ROUNDLK:   case AVR_BUILTIN_ROUNDULK:
    case AVR_BUILTIN_ROUNDLLK:  case AVR_BUILTIN_ROUNDULLK:

      /* Warn about odd rounding.  Rounding points >= FBIT will have
         no effect.  */

      if (TREE_CODE (CALL_EXPR_ARG (exp, 1)) != INTEGER_CST)
        break;

      int rbit = (int) TREE_INT_CST_LOW (CALL_EXPR_ARG (exp, 1));

      if (rbit >= (int) GET_MODE_FBIT (mode))
        {
          warning (OPT_Wextra, "rounding to %d bits has no effect for "
                   "fixed-point value with %d fractional bits",
                   rbit, GET_MODE_FBIT (mode));

          return expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX, mode,
                              EXPAND_NORMAL);
        }
      else if (rbit <= - (int) GET_MODE_IBIT (mode))
        {
          warning (0, "rounding result will always be 0");
          return CONST0_RTX (mode);
        }

      /* The rounding points RP satisfies now:  -IBIT < RP < FBIT.

         TR 18037 only specifies results for  RP > 0.  However, the
         remaining cases of  -IBIT < RP <= 0  can easily be supported
         without any additional overhead.  */

      break; /* round */
    }

  /* No fold found and no insn:  Call support function from libgcc.  */

  if (d->icode == CODE_FOR_nothing
      && DECL_ASSEMBLER_NAME (get_callee_fndecl (exp)) != NULL_TREE)
    {
      return expand_call (exp, target, ignore);
    }

  /* No special treatment needed: vanilla expand.  */

  gcc_assert (d->icode != CODE_FOR_nothing);
  gcc_assert (d->n_args == call_expr_nargs (exp));

  if (d->n_args == 0)
    {
      emit_insn ((GEN_FCN (d->icode)) (target));
      return NULL_RTX;
    }

  return avr_default_expand_builtin (d->icode, exp, target);
}


/* Helper for `avr_fold_builtin' that folds  absfx (FIXED_CST).  */

static tree
avr_fold_absfx (tree tval)
{
  if (FIXED_CST != TREE_CODE (tval))
    return NULL_TREE;

  /* Our fixed-points have no padding:  Use double_int payload directly.  */

  FIXED_VALUE_TYPE fval = TREE_FIXED_CST (tval);
  unsigned int bits = GET_MODE_BITSIZE (fval.mode);
  double_int ival = fval.data.sext (bits);

  if (!ival.is_negative())
    return tval;

  /* ISO/IEC TR 18037, 7.18a.6.2:  The absfx functions are saturating.  */

  fval.data = (ival == double_int::min_value (bits, false).sext (bits))
    ? double_int::max_value (bits, false)
    : -ival;

  return build_fixed (TREE_TYPE (tval), fval);
}


/* Implement `TARGET_FOLD_BUILTIN'.  */

static tree
avr_fold_builtin (tree fndecl, int n_args ATTRIBUTE_UNUSED, tree *arg,
                  bool ignore ATTRIBUTE_UNUSED)
{
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree val_type = TREE_TYPE (TREE_TYPE (fndecl));

  if (!optimize)
    return NULL_TREE;

  switch (fcode)
    {
    default:
      break;

    case AVR_BUILTIN_SWAP:
      {
        return fold_build2 (LROTATE_EXPR, val_type, arg[0],
                            build_int_cst (val_type, 4));
      }

    case AVR_BUILTIN_ABSHR:
    case AVR_BUILTIN_ABSR:
    case AVR_BUILTIN_ABSLR:
    case AVR_BUILTIN_ABSLLR:

    case AVR_BUILTIN_ABSHK:
    case AVR_BUILTIN_ABSK:
    case AVR_BUILTIN_ABSLK:
    case AVR_BUILTIN_ABSLLK:
      /* GCC is not good with folding ABS for fixed-point.  Do it by hand.  */

      return avr_fold_absfx (arg[0]);

    case AVR_BUILTIN_BITSHR:    case AVR_BUILTIN_HRBITS:
    case AVR_BUILTIN_BITSHK:    case AVR_BUILTIN_HKBITS:
    case AVR_BUILTIN_BITSUHR:   case AVR_BUILTIN_UHRBITS:
    case AVR_BUILTIN_BITSUHK:   case AVR_BUILTIN_UHKBITS:

    case AVR_BUILTIN_BITSR:     case AVR_BUILTIN_RBITS:
    case AVR_BUILTIN_BITSK:     case AVR_BUILTIN_KBITS:
    case AVR_BUILTIN_BITSUR:    case AVR_BUILTIN_URBITS:
    case AVR_BUILTIN_BITSUK:    case AVR_BUILTIN_UKBITS:

    case AVR_BUILTIN_BITSLR:    case AVR_BUILTIN_LRBITS:
    case AVR_BUILTIN_BITSLK:    case AVR_BUILTIN_LKBITS:
    case AVR_BUILTIN_BITSULR:   case AVR_BUILTIN_ULRBITS:
    case AVR_BUILTIN_BITSULK:   case AVR_BUILTIN_ULKBITS:

    case AVR_BUILTIN_BITSLLR:   case AVR_BUILTIN_LLRBITS:
    case AVR_BUILTIN_BITSLLK:   case AVR_BUILTIN_LLKBITS:
    case AVR_BUILTIN_BITSULLR:  case AVR_BUILTIN_ULLRBITS:
    case AVR_BUILTIN_BITSULLK:  case AVR_BUILTIN_ULLKBITS:

      gcc_assert (TYPE_PRECISION (val_type)
                  == TYPE_PRECISION (TREE_TYPE (arg[0])));

      return build1 (VIEW_CONVERT_EXPR, val_type, arg[0]);

    case AVR_BUILTIN_INSERT_BITS:
      {
        tree tbits = arg[1];
        tree tval = arg[2];
        tree tmap;
        tree map_type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
        unsigned int map;
        bool changed = false;
        avr_map_op_t best_g;

        if (TREE_CODE (arg[0]) != INTEGER_CST)
          {
            /* No constant as first argument: Don't fold this and run into
               error in avr_expand_builtin.  */

            break;
          }

        tmap = wide_int_to_tree (map_type, wi::to_wide (arg[0]));
        map = TREE_INT_CST_LOW (tmap);

        if (TREE_CODE (tval) != INTEGER_CST
            && 0 == avr_map_metric (map, MAP_MASK_PREIMAGE_F))
          {
            /* There are no F in the map, i.e. 3rd operand is unused.
               Replace that argument with some constant to render
               respective input unused.  */

            tval = build_int_cst (val_type, 0);
            changed = true;
          }

        if (TREE_CODE (tbits) != INTEGER_CST
            && 0 == avr_map_metric (map, MAP_PREIMAGE_0_7))
          {
            /* Similar for the bits to be inserted. If they are unused,
               we can just as well pass 0.  */

            tbits = build_int_cst (val_type, 0);
          }

        if (TREE_CODE (tbits) == INTEGER_CST)
          {
            /* Inserting bits known at compile time is easy and can be
               performed by AND and OR with appropriate masks.  */

            int bits = TREE_INT_CST_LOW (tbits);
            int mask_ior = 0, mask_and = 0xff;

            for (size_t i = 0; i < 8; i++)
              {
                int mi = avr_map (map, i);

                if (mi < 8)
                  {
                    if (bits & (1 << mi))     mask_ior |=  (1 << i);
                    else                      mask_and &= ~(1 << i);
                  }
              }

            tval = fold_build2 (BIT_IOR_EXPR, val_type, tval,
                                build_int_cst (val_type, mask_ior));
            return fold_build2 (BIT_AND_EXPR, val_type, tval,
                                build_int_cst (val_type, mask_and));
          }

        if (changed)
          return build_call_expr (fndecl, 3, tmap, tbits, tval);

        /* If bits don't change their position we can use vanilla logic
           to merge the two arguments.  */

        if (0 == avr_map_metric (map, MAP_NONFIXED_0_7))
          {
            int mask_f = avr_map_metric (map, MAP_MASK_PREIMAGE_F);
            tree tres, tmask = build_int_cst (val_type, mask_f ^ 0xff);

            tres = fold_build2 (BIT_XOR_EXPR, val_type, tbits, tval);
            tres = fold_build2 (BIT_AND_EXPR, val_type, tres, tmask);
            return fold_build2 (BIT_XOR_EXPR, val_type, tres, tval);
          }

        /* Try to decomposing map to reduce overall cost.  */

        if (avr_log.builtin)
          avr_edump ("\n%?: %x\n%?: ROL cost: ", map);

        best_g = avr_map_op[0];
        best_g.cost = 1000;

        for (size_t i = 0; i < ARRAY_SIZE (avr_map_op); i++)
          {
            avr_map_op_t g
              = avr_map_decompose (map, avr_map_op + i,
                                   TREE_CODE (tval) == INTEGER_CST);

            if (g.cost >= 0 && g.cost < best_g.cost)
              best_g = g;
          }

        if (avr_log.builtin)
          avr_edump ("\n");

        if (best_g.arg == 0)
          /* No optimization found */
          break;

        /* Apply operation G to the 2nd argument.  */

        if (avr_log.builtin)
          avr_edump ("%?: using OP(%s%d, %x) cost %d\n",
                     best_g.str, best_g.arg, best_g.map, best_g.cost);

        /* Do right-shifts arithmetically: They copy the MSB instead of
           shifting in a non-usable value (0) as with logic right-shift.  */

        tbits = fold_convert (signed_char_type_node, tbits);
        tbits = fold_build2 (best_g.code, signed_char_type_node, tbits,
                             build_int_cst (val_type, best_g.arg));
        tbits = fold_convert (val_type, tbits);

        /* Use map o G^-1 instead of original map to undo the effect of G.  */

        tmap = wide_int_to_tree (map_type, best_g.map);

        return build_call_expr (fndecl, 3, tmap, tbits, tval);
      } /* AVR_BUILTIN_INSERT_BITS */
    }

  return NULL_TREE;
}



/* Initialize the GCC target structure.  */

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.long\t"
#undef  TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.long\t"
#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER avr_assemble_integer
#undef  TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START avr_file_start
#undef  TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END avr_file_end

#undef  TARGET_ASM_FUNCTION_END_PROLOGUE
#define TARGET_ASM_FUNCTION_END_PROLOGUE avr_asm_function_end_prologue
#undef  TARGET_ASM_FUNCTION_BEGIN_EPILOGUE
#define TARGET_ASM_FUNCTION_BEGIN_EPILOGUE avr_asm_function_begin_epilogue

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE avr_function_value
#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE avr_libcall_value
#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P avr_function_value_regno_p

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE avr_attribute_table
#undef  TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES avr_insert_attributes
#undef  TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS avr_section_type_flags

#undef  TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION avr_asm_named_section
#undef  TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS avr_asm_init_sections
#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO avr_encode_section_info
#undef  TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION avr_asm_select_section

#undef  TARGET_ASM_FINAL_POSTSCAN_INSN
#define TARGET_ASM_FINAL_POSTSCAN_INSN avr_asm_final_postscan_insn

#undef  TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST avr_register_move_cost
#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST avr_memory_move_cost
#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS avr_rtx_costs
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST avr_address_cost
#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG avr_reorg
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG avr_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE avr_function_arg_advance

#undef  TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION avr_set_current_function

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY avr_return_in_memory

#undef  TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

#undef  TARGET_BUILTIN_SETJMP_FRAME_VALUE
#define TARGET_BUILTIN_SETJMP_FRAME_VALUE avr_builtin_setjmp_frame_value

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE avr_conditional_register_usage

#undef  TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK avr_hard_regno_mode_ok
#undef  TARGET_HARD_REGNO_SCRATCH_OK
#define TARGET_HARD_REGNO_SCRATCH_OK avr_hard_regno_scratch_ok
#undef  TARGET_HARD_REGNO_CALL_PART_CLOBBERED
#define TARGET_HARD_REGNO_CALL_PART_CLOBBERED \
  avr_hard_regno_call_part_clobbered

#undef  TARGET_CASE_VALUES_THRESHOLD
#define TARGET_CASE_VALUES_THRESHOLD avr_case_values_threshold

#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED avr_frame_pointer_required_p
#undef  TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE avr_can_eliminate

#undef  TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS avr_allocate_stack_slots_for_args

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN avr_warn_func_return

#undef  TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P avr_class_likely_spilled_p

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE avr_option_override

#undef  TARGET_CANNOT_MODIFY_JUMPS_P
#define TARGET_CANNOT_MODIFY_JUMPS_P avr_cannot_modify_jumps_p

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL avr_function_ok_for_sibcall

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS avr_init_builtins

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL avr_builtin_decl

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN avr_expand_builtin

#undef  TARGET_FOLD_BUILTIN
#define TARGET_FOLD_BUILTIN avr_fold_builtin

#undef  TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P avr_scalar_mode_supported_p

#undef  TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST avr_build_builtin_va_list

#undef  TARGET_FIXED_POINT_SUPPORTED_P
#define TARGET_FIXED_POINT_SUPPORTED_P hook_bool_void_true

#undef  TARGET_CONVERT_TO_TYPE
#define TARGET_CONVERT_TO_TYPE avr_convert_to_type

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef  TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P avr_addr_space_subset_p

#undef  TARGET_ADDR_SPACE_CONVERT
#define TARGET_ADDR_SPACE_CONVERT avr_addr_space_convert

#undef  TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE avr_addr_space_address_mode

#undef  TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE avr_addr_space_pointer_mode

#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P  \
  avr_addr_space_legitimate_address_p

#undef  TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS
#define TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS avr_addr_space_legitimize_address

#undef  TARGET_ADDR_SPACE_DIAGNOSE_USAGE
#define TARGET_ADDR_SPACE_DIAGNOSE_USAGE avr_addr_space_diagnose_usage

#undef  TARGET_MODE_DEPENDENT_ADDRESS_P
#define TARGET_MODE_DEPENDENT_ADDRESS_P avr_mode_dependent_address_p

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND avr_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS avr_print_operand_address
#undef  TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P avr_print_operand_punct_valid_p

#undef TARGET_USE_BY_PIECES_INFRASTRUCTURE_P
#define TARGET_USE_BY_PIECES_INFRASTRUCTURE_P \
  avr_use_by_pieces_infrastructure_p

#undef  TARGET_LEGITIMATE_COMBINED_INSN
#define TARGET_LEGITIMATE_COMBINED_INSN avr_legitimate_combined_insn

#undef  TARGET_STARTING_FRAME_OFFSET
#define TARGET_STARTING_FRAME_OFFSET avr_starting_frame_offset

struct gcc_target targetm = TARGET_INITIALIZER;


#include "gt-avr.h"
