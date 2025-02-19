/* Subroutines for insn-output.cc for AVR 8-bit microcontrollers
   Copyright (C) 1998-2025 Free Software Foundation, Inc.
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

#define IN_TARGET_CODE 1

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
#include "builtins.h"
#include "tree-pass.h"
#include "context.h"
#include "pass_manager.h"
#include "rtl-iter.h"

/* This file should be included last.  */
#include "target-def.h"

/* Maximal allowed offset for an address in the LD command */
#define MAX_LD_OFFSET(MODE) (64 - (signed)GET_MODE_SIZE (MODE))

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
  { ADDR_SPACE_RAM,  0, 2, "", 0, nullptr },
  { ADDR_SPACE_FLASH,  1, 2, "__flash",   0, ".progmem.data" },
  { ADDR_SPACE_FLASH1, 1, 2, "__flash1",  1, ".progmem1.data" },
  { ADDR_SPACE_FLASH2, 1, 2, "__flash2",  2, ".progmem2.data" },
  { ADDR_SPACE_FLASH3, 1, 2, "__flash3",  3, ".progmem3.data" },
  { ADDR_SPACE_FLASH4, 1, 2, "__flash4",  4, ".progmem4.data" },
  { ADDR_SPACE_FLASH5, 1, 2, "__flash5",  5, ".progmem5.data" },
  { ADDR_SPACE_FLASHX, 1, 3, "__flashx",  0, ".progmemx.data" },
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

static const char *out_movqi_r_mr (rtx_insn *, rtx[], int *);
static const char *out_movhi_r_mr (rtx_insn *, rtx[], int *);
static const char *out_movsi_r_mr (rtx_insn *, rtx[], int *);
static const char *out_movqi_mr_r (rtx_insn *, rtx[], int *);
static const char *out_movhi_mr_r (rtx_insn *, rtx[], int *);
static const char *out_movsi_mr_r (rtx_insn *, rtx[], int *);


/* Prototypes for hook implementors if needed before their implementation.  */

static bool avr_rtx_costs (rtx, machine_mode, int, int, int *, bool);


/* Allocate registers from r25 to r8 for parameters for function calls
   resp. r25 to r20 for reduced Tiny.  */
#define FIRST_CUM_REG REG_26

/* Last call saved register */
#define LAST_CALLEE_SAVED_REG (AVR_TINY ? REG_19 : REG_17)

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

/* Condition Code register RTX (reg:CC REG_CC) */
extern GTY(()) rtx cc_reg_rtx;
extern GTY(()) rtx ccn_reg_rtx;
extern GTY(()) rtx cczn_reg_rtx;
rtx cc_reg_rtx;
rtx ccn_reg_rtx;
rtx cczn_reg_rtx;

/* RTXs for all general purpose registers as QImode */
extern GTY(()) rtx all_regs_rtx[REG_32];
rtx all_regs_rtx[REG_32];

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
avr_arch_id avr_arch_index;

/* Unnamed sections associated to __attribute__((progmem)) aka. PROGMEM
   or to address space __flash* or __memx.  Only used as singletons inside
   avr_asm_select_section, but it must not be local there because of GTY.  */
static GTY(()) section *progmem_section[ADDR_SPACE_COUNT];

/* Condition for insns/expanders from avr-dimode.md.  */
bool avr_have_dimode = true;

/* To track if code will use .bss, .data, .rodata.  */
bool avr_need_clear_bss_p = false;
bool avr_need_copy_data_p = false;
bool avr_has_rodata_p = false;

/* To track if we satisfy __call_main from AVR-LibC.  */
bool avr_no_call_main_p = false;

/* Counts how often pass avr-fuse-add has been executed.  Is is kept in
   sync with cfun->machine->n_avr_fuse_add_executed and serves as an
   insn condition for shift insn splitters.  */
int n_avr_fuse_add_executed = 0;

static location_t avr_insn_location = UNKNOWN_LOCATION;

/* Similar to ctz_hwi etc, but as constexpr so we can use it in
   static_assert.  */
static constexpr int
avr_ctz (uint64_t x)
{
#define TSTB(n) (x & ((uint64_t) 1 << n)) ? n
  return
    TSTB (0) : TSTB (1) : TSTB (2) : TSTB (3) : TSTB (4)
    : TSTB (5) : TSTB (6) : TSTB (7) : TSTB (8) : TSTB (9)
    : TSTB (10) : TSTB (11) : TSTB (12) : TSTB (13) : TSTB (14)
    : TSTB (15) : TSTB (16) : TSTB (17) : TSTB (18) : TSTB (19)
    : TSTB (20) : TSTB (21) : TSTB (22) : TSTB (23) : TSTB (24)
    : TSTB (25) : TSTB (26) : TSTB (27) : TSTB (28) : TSTB (29)
    : TSTB (30) : TSTB (31) : TSTB (32) : TSTB (33) : TSTB (34)
    : TSTB (35) : TSTB (36) : TSTB (37) : TSTB (38) : TSTB (39)
    : TSTB (40) : TSTB (41) : TSTB (42) : TSTB (43) : TSTB (44)
    : TSTB (45) : TSTB (46) : TSTB (47) : TSTB (48) : TSTB (49)
    : TSTB (50) : TSTB (51) : TSTB (52) : TSTB (53) : TSTB (54)
    : TSTB (55) : TSTB (56) : TSTB (57) : TSTB (58) : TSTB (59)
    : TSTB (60) : TSTB (61) : TSTB (62) : TSTB (63)
    : 64;
#undef TSTB
}

/* Make sure that there are enough section flags bits.  avr allocates 4.  */
static_assert (8 * sizeof (decltype (section_common::flags))
	       >= 4u + avr_ctz (SECTION_MACH_DEP),
	       "section_common::flags is too narrow");


/* Transform UP into lowercase and write the result to LO.
   You must provide enough space for LO.  Return LO.  */

static char *
avr_tolower (char *lo, const char *up)
{
  char *lo0 = lo;

  for (; *up; up++, lo++)
    *lo = TOLOWER (*up);

  *lo = '\0';

  return lo0;
}


/* Return chunk of mode MODE of X as an rtx.  N specifies the subreg
   byte at which the chunk starts.  N must be an integral multiple
   of the mode size.  */

rtx
avr_chunk (machine_mode mode, rtx x, int n)
{
  gcc_assert (n % GET_MODE_SIZE (mode) == 0);
  machine_mode xmode = GET_MODE (x) == VOIDmode ? DImode : GET_MODE (x);
  return simplify_gen_subreg (mode, x, xmode, n);
}


/* Return the N-th byte of X as an rtx.  */

rtx
avr_byte (rtx x, int n)
{
  return avr_chunk (QImode, x, n);
}


/* Return the sub-word of X starting at byte number N.  */

rtx
avr_word (rtx x, int n)
{
  return avr_chunk (HImode, x, n);
}


/* Return the N-th byte of compile-time constant X as an int8_t.  */

int8_t
avr_int8 (rtx x, int n)
{
  gcc_assert (CONST_INT_P (x) || CONST_FIXED_P (x) || CONST_DOUBLE_P (x));

  return (int8_t) trunc_int_for_mode (INTVAL (avr_byte (x, n)), QImode);
}

/* Return the N-th byte of compile-time constant X as an uint8_t.  */

uint8_t
avr_uint8 (rtx x, int n)
{
  return (uint8_t) avr_int8 (x, n);
}


/* Return the sub-word of compile-time constant X that starts
   at byte N as an int16_t.  */

int16_t
avr_int16 (rtx x, int n)
{
  gcc_assert (CONST_INT_P (x) || CONST_FIXED_P (x) || CONST_DOUBLE_P (x));

  return (int16_t) trunc_int_for_mode (INTVAL (avr_word (x, n)), HImode);
}

/* Return the sub-word of compile-time constant X that starts
   at byte N as an uint16_t.  */

uint16_t
avr_uint16 (rtx x, int n)
{
  return (uint16_t) avr_int16 (x, n);
}


/* Constraint helper function.  XVAL is a CONST_INT or a CONST_DOUBLE.
   Return true if the least significant N_BYTES bytes of XVAL all have a
   popcount in POP_MASK and false, otherwise.  POP_MASK represents a subset
   of integers which contains an integer N iff bit N of POP_MASK is set.  */

bool
avr_popcount_each_byte (rtx xval, int n_bytes, int pop_mask)
{
  for (int i = 0; i < n_bytes; i++)
    {
      unsigned int val8 = avr_uint8 (xval, i);

      if ((pop_mask & (1 << popcount_hwi (val8))) == 0)
	return false;
    }

  return true;
}


/* Constraint helper function.  XVAL is a CONST_INT.  Return true if we
   can perform XOR without a clobber reg, provided the operation is on
   a d-register.  This means each byte is in { 0, 0xff, 0x80 }.  */

bool
avr_xor_noclobber_dconst (rtx xval, int n_bytes)
{
  for (int i = 0; i < n_bytes; ++i)
    {
      unsigned int val8 = avr_uint8 (xval, i);

      if (val8 != 0 && val8 != 0xff && val8 != 0x80)
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


/* Return true if hard register REG supports the ADIW and SBIW instructions.  */

bool
avr_adiw_reg_p (rtx reg)
{
  return (AVR_HAVE_ADIW
	  && test_hard_reg_class (ADDW_REGS, reg));
}


/* Return true iff REGNO is in R16...R31.  */

static bool
avr_ld_regno_p (int regno)
{
  return TEST_HARD_REG_CLASS (LD_REGS, regno);
}


static bool
ra_in_progress ()
{
  return avropt_lra_p ? lra_in_progress : reload_in_progress;
}


/* Set `avr_arch' as specified by `-mmcu='.
   Return true on success.  */

static bool
avr_set_core_architecture (void)
{
  /* Search for mcu core architecture.  */

  if (!avropt_mmcu)
    avropt_mmcu = AVR_MMCU_DEFAULT;

  avr_arch = &avr_arch_types[0];

  for (const avr_mcu_t *mcu = avr_mcu_types; ; mcu++)
    {
      if (!mcu->name)
	{
	  /* Reached the end of `avr_mcu_types'.  This should actually never
	     happen as options are provided by device-specs.  It could be a
	     typo in a device-specs or calling the compiler proper directly
	     with -mmcu=<device>. */

	  error ("unknown core architecture %qs specified with %qs",
		 avropt_mmcu, "-mmcu=");
	  avr_inform_core_architectures ();
	  break;
	}
      else if (strcmp (mcu->name, avropt_mmcu) == 0
	       // Is this a proper architecture ?
	       && !mcu->macro)
	{
	  avr_arch = &avr_arch_types[mcu->arch_id];
	  avr_arch_index = mcu->arch_id;
	  if (avropt_n_flash < 0)
	    avropt_n_flash = 1 + (mcu->flash_size - 1) / 0x10000;

	  return true;
	}
    }

  return false;
}


/* Implement `TARGET_OPTION_OVERRIDE'.  */

static void
avr_option_override (void)
{
  /* caller-save.cc looks for call-clobbered hard registers that are assigned
     to pseudos that cross calls and tries so save-restore them around calls
     in order to reduce the number of stack slots needed.

     This might lead to situations where reload is no more able to cope
     with the challenge of AVR's very few address registers and fails to
     perform the requested spills.  */

  if (avropt_strict_X)
    flag_caller_saves = 0;

  /* Unwind tables currently require a frame pointer for correctness,
     see toplev.cc:process_options().  */

  if ((flag_unwind_tables
       || flag_non_call_exceptions
       || flag_asynchronous_unwind_tables)
      && !ACCUMULATE_OUTGOING_ARGS)
    {
      flag_omit_frame_pointer = 0;
    }

  /* Disable flag_delete_null_pointer_checks if zero is a valid address. */
  if (targetm.addr_space.zero_address_valid (ADDR_SPACE_GENERIC))
    flag_delete_null_pointer_checks = 0;

  /* PR ipa/92606: Inter-procedural analysis optimizes data across
     address-spaces and PROGMEM.  As of v14, the PROGMEM part is
     still not fixed (and there is still no target hook as proposed
     in PR92932).  Just disable respective bogus optimization.  */
  flag_ipa_icf_variables = 0;

  if (flag_pic == 1)
    warning (OPT_fpic, "%<-fpic%> is not supported");
  if (flag_pic == 2)
    warning (OPT_fPIC, "%<-fPIC%> is not supported");
  if (flag_pie == 1)
    warning (OPT_fpie, "%<-fpie%> is not supported");
  if (flag_pie == 2)
    warning (OPT_fPIE, "%<-fPIE%> is not supported");

#if !defined (HAVE_AS_AVR_MGCCISR_OPTION)
  avropt_gasisr_prologues = 0;
#endif

  if (!avr_set_core_architecture ())
    return;

  /* Sould be set by avr-common.cc */
  gcc_assert (avropt_long_double >= avropt_double && avropt_double >= 32);

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

  init_machine_status = []()
  {
    return ggc_cleared_alloc<machine_function> ();
  };

  avr_log_set_avr_log ();

  /* As long as peep2_rescan is not implemented, see
     http://gcc.gnu.org/ml/gcc-patches/2011-10/msg02819.html
     we add a second peephole2 run to get best results.  */
  {
    opt_pass *extra_peephole2
      = g->get_passes ()->get_pass_peephole2 ()->clone ();
    register_pass_info peep2_2_info
      = { extra_peephole2, "avr-fuse-move", 1, PASS_POS_INSERT_AFTER };

    register_pass (&peep2_2_info);
  }
}


int
avr_optimize_size_level ()
{
  return cfun && cfun->decl
    ? opt_for_fn (cfun->decl, optimize_size)
    : optimize_size;
}


static bool
avr_optimize_size_max_p ()
{
  return avr_optimize_size_level () == OPTIMIZE_SIZE_MAX;
}


/* Implement `INIT_EXPANDERS'.  */
/* The function works like a singleton.  */

void
avr_init_expanders (void)
{
  for (int regno = REG_0; regno < REG_32; regno ++)
    all_regs_rtx[regno] = gen_rtx_REG (QImode, regno);

  lpm_reg_rtx  = all_regs_rtx[LPM_REGNO];
  tmp_reg_rtx  = all_regs_rtx[AVR_TMP_REGNO];
  zero_reg_rtx = all_regs_rtx[AVR_ZERO_REGNO];

  cc_reg_rtx = gen_rtx_REG (CCmode, REG_CC);
  ccn_reg_rtx = gen_rtx_REG (CCNmode, REG_CC);
  cczn_reg_rtx = gen_rtx_REG (CCZNmode, REG_CC);

  lpm_addr_reg_rtx = gen_rtx_REG (HImode, REG_Z);

  sreg_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.sreg));
  rampd_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.rampd));
  rampx_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.rampx));
  rampy_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.rampy));
  rampz_rtx = gen_rtx_MEM (QImode, GEN_INT (avr_addr.rampz));

  MEM_VOLATILE_P (sreg_rtx) = 1;
  MEM_VOLATILE_P (rampd_rtx) = 1;
  MEM_VOLATILE_P (rampx_rtx) = 1;
  MEM_VOLATILE_P (rampy_rtx) = 1;
  MEM_VOLATILE_P (rampz_rtx) = 1;

  xstring_empty = gen_rtx_CONST_STRING (VOIDmode, "");
  xstring_e = gen_rtx_CONST_STRING (VOIDmode, "e");

  /* TINY core does not have regs r10-r16, but avr-dimode.md expects them
     to be present */
  if (AVR_TINY)
    avr_have_dimode = false;
}


/* Implement `REGNO_REG_CLASS'.  */
/* Return register class for register R.  */

reg_class
avr_regno_reg_class (int r)
{
  static const reg_class reg_class_tab[] =
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

  if (r == REG_CC)
    return CC_REG;

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
   address space __memx and FALSE, otherwise.  */

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


/* Return TRUE if DECL is a VAR_DECL located in the 24-bit flash
   address space __flashx and FALSE, otherwise.  */

static bool
avr_decl_flashx_p (tree decl)
{
  if (TREE_CODE (decl) != VAR_DECL
      || TREE_TYPE (decl) == error_mark_node)
    {
      return false;
    }

  return ADDR_SPACE_FLASHX == TYPE_ADDR_SPACE (TREE_TYPE (decl));
}


/* Return TRUE if X is a MEM rtx located in flash and FALSE, otherwise.  */

bool
avr_mem_flash_p (rtx x)
{
  return (MEM_P (x)
	  && !ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (x)));
}


/* Return TRUE if X is a MEM rtx located in the 24-bit
   address space __memx and FALSE, otherwise.  */

bool
avr_mem_memx_p (rtx x)
{
  return (MEM_P (x)
	  && ADDR_SPACE_MEMX == MEM_ADDR_SPACE (x));
}


/* Return TRUE if X is a MEM rtx located in the 24-bit flash
   address space __flashx and FALSE, otherwise.  */

bool
avr_mem_flashx_p (rtx x)
{
  return (MEM_P (x)
	  && ADDR_SPACE_FLASHX == MEM_ADDR_SPACE (x));
}


/* A helper for the subsequent function attribute used to dig for
   attribute 'name' in a FUNCTION_DECL or FUNCTION_TYPE.  */

static inline bool
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

  gcc_assert (FUNC_OR_METHOD_TYPE_P (func));

  return NULL_TREE != lookup_attribute (name, TYPE_ATTRIBUTES (func));
}


/* Call WORKER on all NAME attributes of function FUNC.  */

static void
avr_foreach_function_attribute (tree func, const char *name,
				void (*worker) (tree, tree, void *),
				void *cookie)
{
  tree attrs = NULL_TREE;

  if (TREE_CODE (func) == FUNCTION_DECL)
    attrs = DECL_ATTRIBUTES (func);
  else if (FUNC_OR_METHOD_TYPE_P (func))
    attrs = TYPE_ATTRIBUTES (TREE_TYPE (func));

  while (attrs)
    {
      attrs = lookup_attribute (name, attrs);
      if (attrs)
	{
	  worker (func, attrs, cookie);
	  attrs = TREE_CHAIN (attrs);
	}
    }
}


/* Return nonzero if FUNC is a naked function.  */

static bool
avr_naked_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "naked");
}


/* Return nonzero if FUNC is a noblock function.  */

static bool
avr_noblock_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "noblock");
}

/* Return 1 if FUNC is a function that has a "ATTR_NAME" attribute
   (and perhaps also "ATTR_NAME(num)" attributes.  Return -1 if FUNC has
   "ATTR_NAME(num)" attribute(s) but no "ATTR_NAME" attribute.
   When no form of ATTR_NAME is present, return 0.  */

static int
avr_interrupt_signal_function (tree func, const char *attr_name)
{
  int res = 0;

  avr_foreach_function_attribute (func, attr_name,
    [] (tree, tree attr, void *cookie)
    {
      int *pcook = (int *) cookie;

      *pcook = TREE_VALUE (attr)
	? *pcook ? *pcook : -1
	: 1;
    }, &res);

  return res;
}


/* Return 1 if FUNC is an interrupt function that has an "interrupt" attribute
   (and perhaps also "interrupt(num)" attributes.  Return -1 if FUNC has
   "interrupt(num)" attribute(s) but no "interrupt" attribute.  */

static int
avr_interrupt_function (tree func)
{
  return avr_interrupt_signal_function (func, "interrupt");
}


/* Return 1 if FUNC is a signal function that has a "signal" attribute
   (and perhaps also "signal(num)" attributes.  Return -1 if FUNC has
   "signal(num)" attribute(s) but no "signal" attribute.  */

static int
avr_signal_function (tree func)
{
  return avr_interrupt_signal_function (func, "signal");
}


/* Return nonzero if FUNC is an OS_task function.  */

static bool
avr_OS_task_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "OS_task");
}


/* Return nonzero if FUNC is an OS_main function.  */

static bool
avr_OS_main_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "OS_main");
}


/* Return nonzero if FUNC is a no_gccisr function as specified
   by the "no_gccisr" attribute.  */

static bool
avr_no_gccisr_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "no_gccisr");
}


/* Implement `TARGET_CAN_INLINE_P'.  */
/* Some options like -mgas_isr_prologues depend on optimization level,
   and the inliner might think that due to different options, inlining
   is not permitted; see PR104327.  */

static bool
avr_can_inline_p (tree /* caller */, tree /* callee */)
{
  // No restrictions whatsoever.
  return true;
}


/* Implement `TARGET_SET_CURRENT_FUNCTION'.  */
/* Sanity cheching for above function attributes.  */

static void
avr_set_current_function (tree decl)
{
  if (decl == NULL_TREE
      || current_function_decl == NULL_TREE
      || current_function_decl == error_mark_node
      || ! cfun->machine)
    return;

  n_avr_fuse_add_executed = cfun->machine->n_avr_fuse_add_executed;

  if (cfun->machine->attributes_checked_p)
    return;

  location_t loc = DECL_SOURCE_LOCATION (decl);

  cfun->machine->is_naked = avr_naked_function_p (decl);
  cfun->machine->is_signal = avr_signal_function (decl);
  cfun->machine->is_interrupt = avr_interrupt_function (decl);
  cfun->machine->is_noblock = avr_noblock_function_p (decl);
  cfun->machine->is_OS_task = avr_OS_task_function_p (decl);
  cfun->machine->is_OS_main = avr_OS_main_function_p (decl);
  cfun->machine->is_no_gccisr = avr_no_gccisr_function_p (decl);

  const char *isr = cfun->machine->is_interrupt ? "interrupt" : "signal";

  /* Too much attributes make no sense as they request conflicting features. */

  if (cfun->machine->is_OS_task
      && (cfun->machine->is_signal || cfun->machine->is_interrupt))
    error_at (loc, "function attributes %qs and %qs are mutually exclusive",
	      "OS_task", isr);

  if (cfun->machine->is_OS_main
      && (cfun->machine->is_signal || cfun->machine->is_interrupt))
    error_at (loc, "function attributes %qs and %qs are mutually exclusive",
	      "OS_main", isr);

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
	{
	  error_at (loc, "%qs function cannot have arguments", isr);
	  if (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	    inform (loc, "method %qs has an implicit %<this%> argument", name);
	}

      if (TREE_CODE (ret) != VOID_TYPE)
	error_at (loc, "%qs function cannot return a value", isr);

#if defined WITH_AVRLIBC
      /* If the function has the 'signal' or 'interrupt' attribute, ensure
	 that the name of the function is "__vector_NN" so as to catch
	 when the user misspells the vector name.  This check is only
	 required when the "interrupt" resp. "signal" attribute does not
	 have an IRQ-number argument.  */

      if (!startswith (name, "__vector")
	  && (cfun->machine->is_interrupt == 1
	      || cfun->machine->is_signal == 1))
	warning_at (loc, OPT_Wmisspelled_isr, "%qs appears to be a misspelled "
		    "%qs handler, missing %<__vector%> prefix", name, isr);
#endif // AVR-LibC naming conventions
    }
  else if (cfun->machine->is_noblock)
    {
      warning (OPT_Wattributes, "%qs attribute ignored on non-ISR function",
	       "noblock");
    }

#if defined WITH_AVRLIBC
  // Common problem is using "ISR" without first including avr/interrupt.h.
  const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
  name = default_strip_name_encoding (name);
  if (strcmp ("ISR", name) == 0)
    {
      warning_at (loc, OPT_Wmisspelled_isr, "%qs is a reserved identifier"
		  " in AVR-LibC.  Consider %<#include <avr/interrupt.h>%>"
		  " before using the %qs macro", name, name);
    }
  if (strcmp ("INTERRUPT", name) == 0
      || strcmp ("SIGNAL", name) == 0)
    {
      warning_at (loc, OPT_Wmisspelled_isr, "%qs is a deprecated identifier"
		  " in AVR-LibC.  Consider %<#include <avr/interrupt.h>%>"
		  " or %<#include <compat/deprecated.h>%>"
		  " before using the %qs macro", name, name);
    }
#endif // AVR-LibC naming conventions

  /* Don't print the above diagnostics more than once.  */

  cfun->machine->attributes_checked_p = true;
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
  return (ACCUMULATE_OUTGOING_ARGS
	  ? (HOST_WIDE_INT) crtl->outgoing_args_size
	  : 0);
}


/* Implement `TARGET_STARTING_FRAME_OFFSET'.  */
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
  int count = 0;
  bool int_or_sig_p = cfun->machine->is_interrupt || cfun->machine->is_signal;

  if (set)
    CLEAR_HARD_REG_SET (*set);

  /* No need to save any registers if the function never returns or
     has the "OS_task" or "OS_main" attribute.  */

  if (TREE_THIS_VOLATILE (current_function_decl)
      || cfun->machine->is_OS_task
      || cfun->machine->is_OS_main)
    return 0;

  for (int reg = REG_0; reg < REG_32; reg++)
    {
      /* Do not push/pop __tmp_reg__, __zero_reg__, as well as
	 any global register variables.  */

      if (fixed_regs[reg])
	continue;

      if ((int_or_sig_p && !crtl->is_leaf && call_used_or_fixed_reg_p (reg))
	  || (df_regs_ever_live_p (reg)
	      && (int_or_sig_p || !call_used_or_fixed_reg_p (reg))
	      /* Don't record frame pointer registers here.  They are treated
		 individually in the prologue.  */
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


/* Implement `TARGET_CAN_ELIMINATE'.  */
/* Return true if register FROM can be eliminated via register TO.  */

static bool
avr_can_eliminate (const int /*from*/, const int to)
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


/* Worker function for `INITIAL_ELIMINATION_OFFSET'.  */
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
      offset += avr_regs_to_save (nullptr);
      return (get_frame_size () + avr_outgoing_args_size ()
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

     Fix this now, right after node setup in tree.cc:build_common_tree_nodes().
     This must run before c-cppbuiltin.cc:builtin_define_fixed_point_constants()
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


/* Worker function for `INCOMING_RETURN_ADDR_RTX'.  */
/* Return contents of MEM at frame pointer + stack size + 1 (+2 if 3-byte PC).
   This is the return address of the function.  */

rtx
avr_return_addr_rtx (int count, rtx tem)
{
  rtx r;

  /* Can only return this function's return address. Others not supported.  */
  if (count)
    return NULL_RTX;

  if (AVR_3_BYTE_PC)
    {
      r = gen_rtx_SYMBOL_REF (Pmode, ".L__stack_usage+2");
      warning (0, "%<builtin_return_address%> contains only 2 bytes"
	       " of address");
    }
  else
    r = gen_rtx_SYMBOL_REF (Pmode, ".L__stack_usage+1");

  cfun->machine->use_L__stack_usage = true;

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
	  && avr_outgoing_args_size () == 0
	  && avr_regs_to_save (nullptr) == 0
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

      if (!call_used_or_fixed_reg_p (reg))
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


/* Obtain the length sequence of insns.  */

static int
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
  for (int regno = REG_0; regno < REG_32; regno++)
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
  gcc_assert (MEM_P (sfr));

  /* IN treg, IO(SFR) */
  rtx_insn *insn = emit_move_insn (all_regs_rtx[treg], sfr);
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
	  || avr_outgoing_args_size () > 8
	  || (AVR_2_BYTE_PC && live_seq > 6)
	  || live_seq > 7))
    {
      rtx pattern;
      int reg, offset;

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

      int first_reg = (LAST_CALLEE_SAVED_REG + 1) - (live_seq - 2);

      for (reg = REG_29, offset = -live_seq + 1;
	   reg >= first_reg;
	   reg = (reg == REG_28 ? LAST_CALLEE_SAVED_REG : reg - 1), ++offset)
	{
	  rtx m = gen_rtx_MEM (QImode, plus_constant (Pmode, stack_pointer_rtx,
						      offset));
	  rtx r = gen_rtx_REG (QImode, reg);
	  add_reg_note (insn, REG_CFA_OFFSET, gen_rtx_SET (m, r));
	}

      cfun->machine->stack_usage += size + live_seq;
    }
  else /* !minimize */
    {
      for (int reg = REG_0; reg < REG_32; ++reg)
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
	      the optimal method depends on the function type, stack and
	      frame size.  To avoid a complex logic, both methods are
	      tested and the shortest one is selected.

	      There is also the case where SIZE != 0 and no frame pointer is
	      needed; this can occur if ACCUMULATE_OUTGOING_ARGS is on.
	      In that case, insn (*) is not needed.
	      We use the X register as scratch. This is save because in X
	      is call-clobbered.
		 In an interrupt routine, the case of SIZE != 0 together with
	      !frame_pointer_needed can only occur if the function is not a
	      leaf function and thus X has already been saved.  */

	  int irq_state = -1;
	  HOST_WIDE_INT size_cfa = size, neg_size;
	  rtx_insn *fp_plus_insns;

	  gcc_assert (frame_pointer_needed
		      || !isr_p
		      || !crtl->is_leaf);

	  rtx my_fp = (frame_pointer_needed
		       ? frame_pointer_rtx
		       : gen_rtx_REG (Pmode, REG_X));
	  rtx fp = my_fp;

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

	  if (cfun->machine->is_interrupt
	      || (cfun->machine->is_signal
		  && cfun->machine->is_noblock))
	    irq_state = 1;

	  if (TARGET_NO_INTERRUPTS
	      || (cfun->machine->is_signal
		  && ! cfun->machine->is_noblock)
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
  HOST_WIDE_INT size = get_frame_size () + avr_outgoing_args_size ();

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
      if (cfun->machine->is_interrupt
	  || cfun->machine->is_noblock)
	emit_insn (gen_enable_interrupt ());

      if (cfun->machine->gasisr.maybe)
	{
	  /* Let GAS PR21472 emit prologue preamble for us which handles SREG,
	     ZERO_REG and TMP_REG and one additional, optional register for
	     us in an optimal way.  This even scans through inline asm.  */

	  cfun->machine->gasisr.yes = true;

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


/* Turn TVAL into an integer that represents an ISR number.  When no such
   conversion is possible, then return 0.  Unfortunately, we don't know
   how many IRQs the device actually has.  */

static int
avr_isr_number (tree tval)
{
  return (TREE_CODE (tval) == INTEGER_CST
	  && tree_fits_shwi_p (tval)
	  && tree_to_shwi (tval) > 0)
    ? (int) tree_to_shwi (tval)
    : 0;
}


struct avr_fun_cookie
{
  FILE *file;
  const char *name;
};

/* A helper for `avr_declare_function_name' below.  When the function has
   attributes like signal(N) or interrupt(N), then define __vector_N as
   a global alias for the function name.  */

static void
avr_asm_isr_alias (tree /*func*/, tree attr, void *pv)
{
  avr_fun_cookie *cookie = (avr_fun_cookie *) pv;

  for (tree v = TREE_VALUE (attr); v; v = TREE_CHAIN (v))
    {
      int ival = avr_isr_number (TREE_VALUE (v));

      if (ival)
	{
	  fprintf (cookie->file, ".global __vector_%d\n", ival);
	  fprintf (cookie->file, "__vector_%d = ", ival);
	  assemble_name (cookie->file, cookie->name);
	  fprintf (cookie->file, "\n");
	}
    }
}


/* Worker for `ASM_DECLARE_FUNCTION_NAME'.  */

void
avr_declare_function_name (FILE *file, const char *name, tree decl)
{
  // Default action from elfos.h.
  ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");
  ASM_DECLARE_RESULT (file, DECL_RESULT (decl));
  ASM_OUTPUT_FUNCTION_LABEL (file, name, decl);

  avr_fun_cookie fc = { file, name };

  avr_foreach_function_attribute (decl, "signal", avr_asm_isr_alias, &fc);
  avr_foreach_function_attribute (decl, "interrupt", avr_asm_isr_alias, &fc);
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
      if (cfun->machine->is_interrupt
	  || (cfun->machine->is_signal
	      && cfun->machine->is_noblock))
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
	     avr_outgoing_args_size ());

  fprintf (file, "/* frame size = " HOST_WIDE_INT_PRINT_DEC " */\n",
	   (HOST_WIDE_INT) get_frame_size ());

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


/* Worker function for `EPILOGUE_USES'.  */

bool
avr_epilogue_uses (int /*regno*/)
{
  return (reload_completed
	  && cfun->machine
	  && (cfun->machine->is_interrupt || cfun->machine->is_signal));
}


/*  Helper for avr_expand_epilogue.  Emit a pop of a byte register.  */

static void
emit_pop_byte (unsigned regno)
{
  rtx mem = gen_rtx_PRE_INC (HImode, stack_pointer_rtx);
  mem = gen_frame_mem (QImode, mem);
  rtx reg = gen_rtx_REG (QImode, regno);

  emit_insn (gen_rtx_SET (reg, mem));
}


/*  Output RTL epilogue.  */

void
avr_expand_epilogue (bool sibcall_p)
{
  HARD_REG_SET set;
  bool isr_p = cfun->machine->is_interrupt || cfun->machine->is_signal;

  HOST_WIDE_INT size = get_frame_size () + avr_outgoing_args_size ();

  /* epilogue: naked  */
  if (cfun->machine->is_naked)
    {
      gcc_assert (!sibcall_p);

      emit_jump_insn (gen_return ());
      return;
    }

  avr_regs_to_save (&set);
  int live_seq = sequent_regs_live ();

  bool minimize = (TARGET_CALL_PROLOGUES
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

      gcc_assert (frame_pointer_needed
		  || !isr_p
		  || !crtl->is_leaf);

      rtx my_fp = (frame_pointer_needed
		   ? frame_pointer_rtx
		   : gen_rtx_REG (Pmode, REG_X));
      rtx fp = my_fp;

      if (AVR_HAVE_8BIT_SP)
	{
	  /* The high byte (r29) does not change:
	     Prefer SUBI (1 cycle) over SBIW (2 cycles).  */

	  my_fp = all_regs_rtx[FRAME_POINTER_REGNUM];
	}

      /* For rationale see comment in prologue generation.  */

      HOST_WIDE_INT size_max = (HOST_WIDE_INT) GET_MODE_MASK (GET_MODE (my_fp));
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

      rtx_insn *fp_plus_insns = get_insns ();
      end_sequence ();

      /********** Method 2: Adjust Stack pointer  **********/

      if (avr_sp_immediate_operand (gen_int_mode (size, HImode), HImode))
	{
	  start_sequence ();

	  emit_move_insn (stack_pointer_rtx,
			  plus_constant (Pmode, stack_pointer_rtx, size));

	  rtx_insn *sp_plus_insns = get_insns ();
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

  for (int reg = REG_31; reg >= REG_0; --reg)
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
  app_disable ();
  fprintf (file, "/* epilogue start */\n");
}


/* Implement `TARGET_CANNOT_MODITY_JUMPS_P'.  */

static bool
avr_cannot_modify_jumps_p (void)
{
  /* Naked functions must not have any instructions after
     their epilogue, see PR42240 */

  return (reload_completed
	  && cfun->machine
	  && cfun->machine->is_naked);
}


/* Implement `TARGET_MODE_DEPENDENT_ADDRESS_P'.  */

static bool
avr_mode_dependent_address_p (const_rtx /*addr*/, addr_space_t as)
{
  /* FIXME:  Non-generic addresses are not mode-dependent in themselves.
       This hook just serves to hack around PR rtl-optimization/52543 by
       claiming that non-generic addresses were mode-dependent so that
       lower-subreg.cc will skip these addresses.  lower-subreg.cc sets up fake
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
		       rtx_code outer_code, bool strict)
{
  return (REG_P (reg)
	  && (avr_regno_mode_code_ok_for_base_p (REGNO (reg), QImode,
						 as, outer_code, UNKNOWN)
	      || (!strict
		  && REGNO (reg) >= FIRST_PSEUDO_REGISTER)));
}


/* Return nonzero if rtx X is a legitimate memory address on the target
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
	    bool fit = (IN_RANGE (INTVAL (op1), 0, MAX_LD_OFFSET (mode))
			// Reduced Tiny does not support PLUS addressing
			// anyway, so we are not restricted to LD offset.
			|| AVR_TINY);

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
		 "reload_completed=%d ra_in_progress=%d %s:",
		 ok, mode, strict, reload_completed, ra_in_progress (),
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
avr_legitimize_reload_address (rtx *px, machine_mode mode, int opnum,
			       int type, int addr_type, int /*ind_levels*/,
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
      && reg_equiv_constant (REGNO (XEXP (x, 0))) == 0
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

static const char *
avr_asm_len (const char *tpl, rtx *operands, int *plen, int n_words)
{
  if (plen == nullptr)
    output_asm_insn (tpl, operands);
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

static const char *
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
  return nullptr;
}


/* Return the condition name as a string to be used in a BR** instruction.
   Used in conditional jump constructing.  */

static const char *
avr_cond_string (rtx_code code, bool cc_overflow_unusable)
{
  switch (code)
    {
    case NE:
      return "ne";
    case EQ:
      return "eq";
    case GE:
      return cc_overflow_unusable ? "pl" : "ge";
    case LT:
      return cc_overflow_unusable ? "mi" : "lt";
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
		{
		  location_t loc = avr_insn_location != UNKNOWN_LOCATION
		    ? avr_insn_location
		    : input_location;
		  if (warning_at (loc, 0, "pointer offset from symbol may be"
				  " incorrect"))
		    {
		      output_addr_const (stderr, addr);
		      fprintf (stderr, "\n");
		    }
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
  else if (code == 'i')
    {
      const int sfr0 = avr_arch->sfr_offset;
      bool lossage_p = false;

      switch (GET_CODE (x))
	{
	default:
	  lossage_p = true;
	  break;

	case CONST_INT:
	  {
	    const auto ival = INTVAL (x);

	    if (io_address_operand (x, VOIDmode))
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
		  fprintf (file, HOST_WIDE_INT_PRINT_HEX, ival - sfr0);
	      }
	    else
	      output_operand_lossage
		("bad I/O address 0x" HOST_WIDE_INT_PRINT_HEX_PURE
		 " outside of valid range [0x%x, 0x%x] for %%i operand",
		 ival, sfr0, sfr0 + 0x3f);
	  }
	  break; // CONST_INT

	case MEM:
	  if (io_address_operand (XEXP (x, 0), VOIDmode))
	    avr_print_operand (file, XEXP (x, 0), 'i');
	  else
	    lossage_p = true;
	  break;

	case SYMBOL_REF:
	  if (io_address_operand (x, VOIDmode))
	    {
	      rtx addr = plus_constant (HImode, x, -sfr0);
	      avr_print_operand_address (file, VOIDmode, addr);
	    }
	  else
	    lossage_p = true;
	  break;
	} // switch code

      if (lossage_p)
	output_operand_lossage ("%s operand cannot be used as %%i I/O "
				"address operand", rtx_name[GET_CODE (x)]);
    } // code = i
  else if (REG_P (x))
    {
      if (x == zero_reg_rtx)
	fprintf (file, "__zero_reg__");
      else if (code == 'r' && REGNO (x) < REG_32)
	fprintf (file, "%d", (int) REGNO (x));
      else
	fprintf (file, "%s", reg_names[REGNO (x) + abcd]);
    }
  else if (CONST_INT_P (x))
    {
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) + abcd);
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
	    {
	      location_t loc = avr_insn_location != UNKNOWN_LOCATION
		? avr_insn_location
		: input_location;
	      if (warning_at (loc, 0, "accessing data memory with"
			      " program memory address"))
		{
		  output_addr_const (stderr, addr);
		  fprintf (stderr,"\n");
		}
	    }
	  output_addr_const (file, addr);
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
	    fatal_insn ("internal compiler error.  Bad address:", addr);
	  fputc ('+', file);
	  avr_print_operand (file, XEXP (addr, 1), code);
	}
      else
	avr_print_operand_address (file, VOIDmode, addr);
    }
  else if (code == 'x')
    {
      /* Constant progmem address - like used in jmp or call */
      if (text_segment_operand (x, VOIDmode) == 0)
	{
	  location_t loc = avr_insn_location != UNKNOWN_LOCATION
	    ? avr_insn_location
	    : input_location;
	  if (warning_at (loc, 0, "accessing program memory"
			  " with data memory address"))
	    {
	      output_addr_const (stderr, x);
	      fprintf (stderr, "\n");
	    }
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
      if (GET_MODE (x) == SFmode)
	{
	  long val;
	  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), val);
	  fprintf (file, "0x%lx", val);
	}
      else if (GET_MODE (x) == DFmode)
	{
	  long l[2];
	  REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (x), l);
	  fprintf (file, "0x%lx%08lx", l[1] & 0xffffffff, l[0] & 0xffffffff);
	}
      else
	fatal_insn ("internal compiler error.  Unknown mode:", x);
    }
  else if (GET_CODE (x) == CONST_STRING)
    fputs (XSTR (x, 0), file);
  else if (code == 'j' || code == 'L')
    fputs (avr_cond_string (GET_CODE (x), code == 'L'), file);
  else if (code == 'k' || code == 'K')
    fputs (avr_cond_string (reverse_condition (GET_CODE (x)), code == 'K'),
	   file);
  else
    avr_print_operand_address (file, VOIDmode, x);
}


/* Implement `TARGET_USE_BY_PIECES_INFRASTRUCTURE_P'.  */
/* Prefer sequence of loads/stores for moves of size upto
   two - two pairs of load/store instructions are always better
   than the 5 instruction sequence for a loop (1 instruction
   for loop counter setup, and 4 for the body of the loop). */

static bool
avr_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT size,
				    unsigned int align,
				    by_pieces_operation op, bool speed_p)
{
  if (op != MOVE_BY_PIECES
      || (speed_p && size > MOVE_MAX_PIECES))
    return default_use_by_pieces_infrastructure_p (size, align, op, speed_p);

  return size <= MOVE_MAX_PIECES;
}


/* Choose mode for jump insn:
   1 - relative jump in range -63 <= x <= 62 ;
   2 - relative jump in range -2046 <= x <= 2045 ;
   3 - absolute jump (only when we have JMP / CALL).

   When jumping backwards, assume the jump offset is EXTRA words
   bigger than inferred from insn addresses.  */

int
avr_jump_mode (rtx x, rtx_insn *insn, int extra)
{
  int dest_addr = INSN_ADDRESSES (INSN_UID (GET_CODE (x) == LABEL_REF
					    ? XEXP (x, 0) : x));
  int cur_addr = INSN_ADDRESSES (INSN_UID (insn));
  int jump_distance = cur_addr - dest_addr;

  if (IN_RANGE (jump_distance, -63, 62 - extra))
    return 1;
  else if (IN_RANGE (jump_distance, -2046, 2045 - extra))
    return 2;
  else if (AVR_HAVE_JMP_CALL)
    return 3;

  return 2;
}


/* Return the asm code for conditional branch INSN, where XOP[0] is the jump
   target label and XOP[1] is a comparison operator of REG_CC against 0.  */

const char *
avr_cond_branch (rtx_insn *insn, rtx *xop)
{
  machine_mode ccmode = GET_MODE (XEXP (xop[1], 0));
  rtx_code cond = GET_CODE (xop[1]);
  bool cc_overflow_unusable = ccmode != CCmode;
  int len = avr_jump_mode (xop[0], insn);

  if (ccmode == CCNmode)
    // The N flag can only do < 0 and >= 0.
    gcc_assert (cond == GE || cond == LT);

  switch (cond)
    {
    case GT:
      if (cc_overflow_unusable)
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
      if (cc_overflow_unusable)
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
      switch (len)
	{
	case 1:
	  return cc_overflow_unusable
	    ? "br%L1 %0"
	    : "br%j1 %0";
	case 2:
	  return cc_overflow_unusable
	    ? "br%K1 .+2" CR_TAB "rjmp %0"
	    : "br%k1 .+2" CR_TAB "rjmp %0";
	default:
	  return cc_overflow_unusable
	    ? "br%K1 .+4" CR_TAB "jmp %0"
	    : "br%k1 .+4" CR_TAB "jmp %0";
	}
    }
  return "";
}


/* Worker function for `FINAL_PRESCAN_INSN'.  */
/* Output insn cost for next insn.  */

void
avr_final_prescan_insn (rtx_insn *insn, rtx * /*operands*/,
			int /*num_operands*/)
{
  avr_insn_location = LOCATION_LOCUS (INSN_LOCATION (insn));

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
			   optimize_insn_for_speed_p ()));
    }

  if (avr_log.insn_addresses)
    fprintf (asm_out_file, ";; ADDR = %d\n",
	     (int) INSN_ADDRESSES (INSN_UID (insn)));
}


/* Implement `TARGET_ASM_FINAL_POSTSCAN_INSN'.  */
/* When GAS generates (parts of) ISR prologue / epilogue for us, we must
   hint GAS about the end of the code to scan.  There might be code located
   after the last epilogue.  */

static void
avr_asm_final_postscan_insn (FILE *stream, rtx_insn *insn, rtx *, int)
{
  if (!next_real_insn (insn))
    avr_insn_location = UNKNOWN_LOCATION;

  if (cfun->machine->gasisr.yes
      && !next_real_insn (insn))
    {
      app_disable ();
      fprintf (stream, "\t__gcc_isr %d,r%d\n", GASISR_Done,
	       cfun->machine->gasisr.regno);
    }
}


/* Worker function for `FUNCTION_ARG_REGNO_P'.  */
/* Returns nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */

bool
avr_function_arg_regno_p (int r)
{
  return AVR_TINY
    ? IN_RANGE (r, REG_20, REG_25)
    : IN_RANGE (r, REG_8, REG_25);
}


/* Worker function for `INIT_CUMULATIVE_ARGS'.  */
/* Initializing the variable cum for the state at the beginning
   of the argument list.  */

void
avr_init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype, rtx libname,
			  tree /*fndecl*/)
{
  cum->nregs = AVR_TINY ? 1 + REG_25 - REG_20 : 1 + REG_25 - REG_8;
  cum->regno = FIRST_CUM_REG;
  cum->has_stack_args = false;
  if (!libname && stdarg_p (fntype))
    cum->nregs = 0;

  /* Assume the calle may be tail called */

  cfun->machine->sibcall_fails = false;
}


/* Returns the number of registers to allocate for a function argument.  */

static int
avr_num_arg_regs (machine_mode mode, const_tree type)
{
  int size = (mode == BLKmode
	      ? int_size_in_bytes (type)
	      : GET_MODE_SIZE (mode));

  /* Align all function arguments to start in even-numbered registers.
     Odd-sized arguments leave holes above them.  */

  return (size + 1) & ~1;
}


/* Implement `TARGET_FUNCTION_ARG'.  */
/* Controls whether a function argument is passed
   in a register, and which register.  */

static rtx
avr_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = avr_num_arg_regs (arg.mode, arg.type);

  if (cum->nregs && bytes <= cum->nregs)
    return gen_rtx_REG (arg.mode, cum->regno - bytes);

  cum->has_stack_args = true;

  return NULL_RTX;
}


/* Implement `TARGET_FUNCTION_ARG_ADVANCE'.  */
/* Update the summarizer variable CUM to advance past an argument
   in the argument list.  */

static void
avr_function_arg_advance (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = avr_num_arg_regs (arg.mode, arg.type);

  cum->nregs -= bytes;
  cum->regno -= bytes;

  /* A parameter is being passed in a call-saved register.  As the original
     contents of these regs has to be restored before leaving the function,
     a function must not pass arguments in call-saved regs in order to get
     tail-called.  */

  if (cum->regno >= REG_8
      && cum->nregs >= 0
      && !call_used_or_fixed_reg_p (cum->regno))
    {
      /* FIXME: We ship info on failing tail-call in struct machine_function.
	 This uses internals of calls.cc:expand_call() and the way args_so_far
	 is used.  targetm.function_ok_for_sibcall() needs to be extended to
	 pass &args_so_far, too.  At present, CUMULATIVE_ARGS is target
	 dependent so that such an extension is not wanted.  */

      cfun->machine->sibcall_fails = true;
    }

  /* Test if all registers needed by the ABI are actually available.  If the
     user has fixed a GPR needed to pass an argument, an (implicit) function
     call will clobber that fixed register.  See PR45099 for an example.  */

  if (cum->regno >= REG_8
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

  tree fntype_callee = TREE_TYPE (CALL_EXPR_FN (exp_callee));

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
      || avr_naked_function_p (decl_callee))
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


/* Return true if a value of mode MODE is read by __xload_* function
   provided it is located in __memx.  */

bool
avr_xload_libgcc_p (machine_mode mode)
{
  int n_bytes = GET_MODE_SIZE (mode);

  return (n_bytes > 1
	  || avropt_n_flash > 1);
}


/* Return true if a value of mode MODE is read by __fload_* function
   provided it is located in __flashx.  */

bool
avr_fload_libgcc_p (machine_mode)
{
  return (! AVR_HAVE_ELPMX
	  && ! AVR_HAVE_LPMX);
}


/* USE_LIBGCC = true:  Return true when MEM is a mem rtx for address space
   AS that will be loaded using a libgcc support function.
   USE_LIBGCC = false:  Return true when MEM is a mem rtx for address space
   AS that will be loaded inline (without using a libgcc support function).  */

bool
avr_load_libgcc_mem_p (rtx mem, addr_space_t as, bool use_libgcc)
{
  if (MEM_P (mem))
    {
      machine_mode mode = GET_MODE (mem);
      rtx addr = XEXP (mem, 0);

      if (MEM_ADDR_SPACE (mem) != as
	  || GET_MODE (addr) != targetm.addr_space.pointer_mode (as))
	return false;

      switch (as)
	{
	default:
	  gcc_unreachable ();

	case ADDR_SPACE_FLASH:
	  return avr_load_libgcc_p (mem) == use_libgcc;

	case ADDR_SPACE_MEMX:
	  return avr_xload_libgcc_p (mode) == use_libgcc;

	case ADDR_SPACE_FLASHX:
	  return avr_fload_libgcc_p (mode) == use_libgcc;
	}
    }

  return false;
}


/* Like `avr_load_libgcc_mem_p()', but for a single_set insn with
   a SET_SRC according to avr_load_libgcc_mem_p.  */

bool
avr_load_libgcc_insn_p (rtx_insn *insn, addr_space_t as, bool use_libgcc)
{
  rtx set = single_set (insn);
  return (set
	  && avr_load_libgcc_mem_p (SET_SRC (set), as, use_libgcc));
}


/* Return true when INSN has a REG_UNUSED note for hard reg REG.
   rtlanal.cc::find_reg_note() uses == to compare XEXP (link, 0)
   therefore use a custom function.  */

static bool
avr_insn_has_reg_unused_note_p (rtx_insn *insn, rtx reg)
{
  for (rtx link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_UNUSED
	&& REG_P (XEXP (link, 0))
	&& REGNO (reg) >= REGNO (XEXP (link, 0))
	&& END_REGNO (reg) <= END_REGNO (XEXP (link, 0)))
      return true;

  return false;
}


/* A helper for the next function.
   Return nonzero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels.  It may live past calls or jumps though.  */

static bool
_reg_unused_after (rtx_insn *insn, rtx reg, bool look_at_insn)
{
  if (look_at_insn)
    {
      /* If the reg is set by this instruction, then it is safe for our
	 case.  Disregard the case where this is a store to memory, since
	 we are checking a register used in the store address.  */
      rtx set = single_set (insn);
      if (set && !MEM_P (SET_DEST (set))
	  && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	return true;

      /* This case occurs when fuse-add introduced a POST_INC addressing,
	 but the address register is unused after.  */
      if (set)
	{
	  rtx mem = MEM_P (SET_SRC (set)) ? SET_SRC (set) : SET_DEST (set);
	  if (MEM_P (mem)
	      && reg_overlap_mentioned_p (reg, XEXP (mem, 0))
	      && avr_insn_has_reg_unused_note_p (insn, reg))
	    return true;
	}
    }

  while ((insn = NEXT_INSN (insn)))
    {
      rtx set;
      rtx_code code = GET_CODE (insn);

#if 0
      /* If this is a label that existed before reload, then the register
	 if dead here.  However, if this is a label added by reorg, then
	 the register may still be live here.  We can't tell the difference,
	 so we just ignore labels completely.  */
      if (code == CODE_LABEL)
	return true;
      /* else */
#endif

      if (!INSN_P (insn))
	continue;

      if (code == JUMP_INSN)
	return false;

      /* If this is a sequence, we must handle them all at once.
	 We could have for instance a call that sets the target register,
	 and an insn in a delay slot that uses the register.  In this case,
	 we must return 0.  */
      else if (code == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  rtx_sequence *seq = as_a <rtx_sequence *> (PATTERN (insn));
	  bool retval = false;

	  for (int i = 0; i < seq->len (); i++)
	    {
	      rtx_insn *this_insn = seq->insn (i);
	      rtx set = single_set (this_insn);

	      if (CALL_P (this_insn))
		code = CALL_INSN;
	      else if (JUMP_P (this_insn))
		{
		  if (INSN_ANNULLED_BRANCH_P (this_insn))
		    return false;
		  code = JUMP_INSN;
		}

	      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
		return false;
	      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
		{
		  if (!MEM_P (SET_DEST (set)))
		    retval = true;
		  else
		    return false;
		}
	      if (!set
		  && reg_overlap_mentioned_p (reg, PATTERN (this_insn)))
		return false;
	    }
	  if (retval)
	    return true;
	  else if (code == JUMP_INSN)
	    return false;
	}

      if (code == CALL_INSN)
	{
	  rtx tem;
	  for (tem = CALL_INSN_FUNCTION_USAGE (insn); tem; tem = XEXP (tem, 1))
	    if (GET_CODE (XEXP (tem, 0)) == USE
		&& REG_P (XEXP (XEXP (tem, 0), 0))
		&& reg_overlap_mentioned_p (reg, XEXP (XEXP (tem, 0), 0)))
	      return false;
	  if (call_used_or_fixed_reg_p (REGNO (reg)))
	    return true;
	}

      set = single_set (insn);

      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
	return false;
      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	return !MEM_P (SET_DEST (set));
      if (!set && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	return false;
    }
  return true;
}


/* Return nonzero if register REG dead after INSN.  */

bool
reg_unused_after (rtx_insn *insn, rtx reg)
{
  return (dead_or_set_p (insn, reg)
	  || (REG_P (reg) && _reg_unused_after (insn, reg, true)));
}


/* Return true when REGNO is set by INSN but not used by the following code.
   The difference to reg_unused_after() is that reg_unused_after() returns
   true for the entire result even when the result *IS* being used atfer.  */

static bool
avr_result_regno_unused_p (rtx_insn *insn, unsigned regno)
{
  if (!insn || !single_set (insn) || regno >= REG_32)
    return false;

  rtx dest = SET_DEST (single_set (insn));
  if (!REG_P (dest) || !IN_RANGE (regno, REGNO (dest), END_REGNO (dest) - 1))
    return false;

  return (avr_insn_has_reg_unused_note_p (insn, all_regs_rtx[regno])
	  || _reg_unused_after (insn, all_regs_rtx[regno], false));
}


/* Fixme: This is a hack because secondary reloads don't works as expected.

   Find an unused d-register to be used as scratch in INSN.
   EXCLUDE is either NULL_RTX or some register.  In the case where EXCLUDE
   is a register, skip all possible return values that overlap EXCLUDE.
   The policy for the returned register is similar to that of
   `reg_unused_after', i.e. the returned register may overlap the SET_DEST
   of INSN.

   Return a QImode d-register or NULL_RTX if nothing found.  */

static rtx
avr_find_unused_d_reg (rtx_insn *insn, rtx exclude)
{
  bool isr_p = (avr_interrupt_function (current_function_decl)
		|| avr_signal_function (current_function_decl));

  for (int regno = REG_16; regno < REG_32; regno++)
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
	      || (!isr_p && call_used_or_fixed_reg_p (regno))))
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

static const char *
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
      gcc_unreachable ();

    case REG:

      gcc_assert (REG_Z == REGNO (addr));

      switch (n_bytes)
	{
	default:
	  gcc_unreachable ();

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

      for (int i = 0; i < n_bytes; ++i)
	{
	  rtx reg = avr_byte (dest, i);

	  if (i > 0)
	    avr_asm_len ("adiw %2,1", xop, plen, 1);

	  avr_asm_len ("%4lpm", xop, plen, 1);

	  if (REGNO (reg) != LPM_REGNO)
	    avr_asm_len ("mov %0,r0", &reg, plen, 1);
	}

      if (! reg_unused_after (insn, xop[2]))
	avr_asm_len ("adiw %2,1", xop, plen, 1);

      break; /* POST_INC */

    } /* switch CODE (addr) */

  return "";
}


/* If PLEN == NULL: Ouput instructions to load a value from a memory location
   OP[1] in AS1 to register OP[0].
   If PLEN != 0 set *PLEN to the length in words of the instruction sequence.
   Return "".  */

const char *
avr_out_lpm (rtx_insn *insn, rtx *op, int *plen)
{
  rtx xop[7];
  rtx dest = op[0];
  rtx src = SET_SRC (single_set (insn));
  int n_bytes = GET_MODE_SIZE (GET_MODE (dest));
  addr_space_t as = MEM_ADDR_SPACE (src);

  if (plen)
    *plen = 0;

  if (MEM_P (dest))
    {
      warning (0, "writing to address space %qs not supported",
	       avr_addrspace[MEM_ADDR_SPACE (dest)].name);

      return "";
    }

  rtx addr = XEXP (src, 0);
  rtx_code code = GET_CODE (addr);

  gcc_assert (REG_P (dest));
  gcc_assert (REG == code || POST_INC == code);

  xop[0] = dest;
  xop[1] = addr;
  xop[2] = lpm_addr_reg_rtx;
  xop[4] = xstring_empty;
  xop[5] = tmp_reg_rtx;
  xop[6] = XEXP (rampz_rtx, 0);

  int segment = avr_addrspace[as].segment;

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
      gcc_unreachable ();

    case REG:

      gcc_assert (REG_Z == REGNO (addr));

      switch (n_bytes)
	{
	default:
	  gcc_unreachable ();

	case 1:
	  avr_asm_len ("%4lpm %0,%a2", xop, plen, 1);
	  break;

	case 2:
	  if (REGNO (dest) == REG_Z)
	    avr_asm_len ("%4lpm %5,%a2+" CR_TAB
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
	    avr_asm_len ("%4lpm %5,%a2+" CR_TAB
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

      avr_asm_len		    ("%4lpm %A0,%a2+", xop, plen, 1);
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


/* Load a value from 24-bit address space __memx and return "".
   PLEN == 0: Output instructions.
   PLEN != 0: Set *PLEN to the length of the sequence in words.  */

const char *
avr_out_xload (rtx_insn * /*insn*/, rtx *op, int *plen)
{
  rtx xop[4];

  xop[0] = op[0];
  xop[1] = op[1];
  xop[2] = lpm_addr_reg_rtx;
  xop[3] = AVR_HAVE_LPMX ? op[0] : lpm_reg_rtx;

  if (plen)
    *plen = 0;

  if (reg_overlap_mentioned_p (xop[3], lpm_addr_reg_rtx))
    avr_asm_len ("sbrs %1,7", xop, plen, 1);

  avr_asm_len (AVR_HAVE_LPMX ? "lpm %3,%a2" : "lpm", xop, plen, 1);

  avr_asm_len ("sbrc %1,7" CR_TAB
	       "ld %3,%a2", xop, plen, 2);

  if (REGNO (xop[0]) != REGNO (xop[3]))
    avr_asm_len ("mov %0,%3", xop, plen, 1);

  return "";
}


/* Load a value from 24-bit address space __flashx and return "".
   PLEN == 0: Output instructions.
   PLEN != 0: Set *PLEN to the length of the sequence in words.  */

const char *
avr_out_fload (rtx_insn * /*insn*/, rtx *xop, int *plen)
{
  gcc_assert (AVR_HAVE_ELPMX
	      || (! AVR_HAVE_ELPM && AVR_HAVE_LPMX));
  if (plen)
    *plen = 0;

  if (AVR_HAVE_ELPMX)
    avr_asm_len ("out __RAMPZ__,%1", xop, plen, 1);

  const int n_bytes = GET_MODE_SIZE (GET_MODE (xop[0]));
  const char *s_load = AVR_HAVE_ELPMX ? "elpm %0,Z" : "lpm %0,Z";
  const char *s_load_inc = AVR_HAVE_ELPMX ? "elpm %0,Z+" : "lpm %0,Z+";
  const char *s_load_tmp_inc = AVR_HAVE_ELPMX ? "elpm r0,Z+" : "lpm r0,Z+";
  bool use_tmp_for_r30 = false;

  // There are nasty cases where reload assigns a register to dest that
  // overlaps Z, even though fmov<mode> clobbers REG_Z.
  for (int i = 0; i < n_bytes; ++i)
    {
      rtx b = avr_byte (xop[0], i);
      if (i == n_bytes - 1)
	avr_asm_len (s_load, &b, plen, 1);
      else if (REGNO (b) == REG_30)
	{
	  avr_asm_len (s_load_tmp_inc, &b, plen, 1);
	  use_tmp_for_r30 = true;
	}
      else
	avr_asm_len (s_load_inc, &b, plen, 1);
    }

  if (use_tmp_for_r30)
    avr_asm_len ("mov r30,r0", xop, plen, 1);

  if (AVR_HAVE_ELPMX && AVR_HAVE_RAMPD)
    avr_asm_len ("out __RAMPZ__,__zero_reg__", xop, plen, 1);

  return "";
}


/* A helper for `output_reload_insisf' and `output_reload_inhi'.  */
/* Set register OP[0] to compile-time constant OP[1].
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

void
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

  if (REGNO (dest) < REG_16
      && END_REGNO (dest) > REG_16)
    {
      clobber_reg = all_regs_rtx[END_REGNO (dest) - 1];
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
      bool done_byte = false;
      rtx xop[3];

      /* Crop the n-th destination byte.  */

      xdest[n] = avr_byte (dest, n);
      int ldreg_p = test_hard_reg_class (LD_REGS, xdest[n]);

      if (!CONST_INT_P (src)
	  && !CONST_FIXED_P (src)
	  && !CONST_DOUBLE_P (src))
	{
	  static const char *const asm_code[][2] =
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

      xval = avr_byte (src, n);
      ival[n] = INTVAL (xval);

      /* Look if we can reuse the low word by means of MOVW.  */

      if (n == 2
	  && n_bytes >= 4
	  && AVR_HAVE_MOVW)
	{
	  int lo16 = avr_int16 (src, 0);
	  int hi16 = avr_int16 (src, 2);

	  if (lo16 == hi16)
	    {
	      if (lo16 != 0 || ! clear_p)
		avr_asm_len ("movw %C0,%A0", &op[0], len, 1);

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

      if (ival[n] == -1)
	{
	  if (!clear_p)
	    avr_asm_len ("clr %0", &xdest[n], len, 1);

	  avr_asm_len ("dec %0", &xdest[n], len, 1);
	  continue;
	}
      else if (ival[n] == 1)
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


const char *
output_movqi (rtx_insn *insn, rtx operands[], int *plen)
{
  rtx dest = operands[0];
  rtx src = operands[1];

  if (avr_mem_flash_p (src)
      || avr_mem_flash_p (dest))
    {
      return avr_out_lpm (insn, operands, plen);
    }

  gcc_assert (GET_MODE_SIZE (GET_MODE (dest)) == 1);

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
	  return output_reload_inhi (xop, NULL_RTX, plen);
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

static const char *
avr_out_movqi_r_mr_reg_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (src, 0);
  rtx base = XEXP (x, 0);

  if (plen)
    *plen = 0;

  if (!reg_overlap_mentioned_p (dest, base))
    {
      avr_asm_len (TINY_ADIW (%I1, %J1, %o1) CR_TAB
		   "ld %0,%b1", op, plen, 3);
      if (!reg_unused_after (insn, base))
	avr_asm_len (TINY_SBIW (%I1, %J1, %o1), op, plen, 2);
    }
  else
    {
      // PR98762: The base register overlaps dest and is only partly clobbered.
      rtx base2 = all_regs_rtx[1 ^ REGNO (dest)];

      if (!reg_unused_after (insn, base2))
	avr_asm_len ("mov __tmp_reg__,%0", &base2, plen, 1);
      avr_asm_len (TINY_ADIW (%I1, %J1, %o1) CR_TAB
		   "ld %0,%b1", op, plen, 3);
      if (!reg_unused_after (insn, base2))
	avr_asm_len ("mov %0,__tmp_reg__", &base2, plen, 1);
    }

  return "";
}

static const char *
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

      if (AVR_TINY)
	return avr_out_movqi_r_mr_reg_disp_tiny (insn, op, plen);

      if (plen)
	*plen = 0;

      int disp = INTVAL (XEXP (x, 1));
      rtx base = XEXP (x, 0);
      rtx base2 = all_regs_rtx[1 ^ REGNO (dest)];
      bool partial_clobber = (reg_overlap_mentioned_p (dest, base)
			      && ! reg_unused_after (insn, base2));

      if (disp - GET_MODE_SIZE (GET_MODE (src)) >= 63)
	{
	  // PR117744: The base register overlaps dest and is
	  // only partially clobbered.
	  if (partial_clobber)
	    avr_asm_len ("mov __tmp_reg__,%0", &base2, plen, 1);

	  if (REGNO (XEXP (x, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    avr_asm_len ("adiw r28,%o1-63" CR_TAB
			 "ldd %0,Y+63"     CR_TAB
			 "sbiw r28,%o1-63", op, plen, 3);
	  else
	    avr_asm_len ("subi r28,lo8(-%o1)" CR_TAB
			 "sbci r29,hi8(-%o1)" CR_TAB
			 "ld %0,Y"            CR_TAB
			 "subi r28,lo8(%o1)"  CR_TAB
			 "sbci r29,hi8(%o1)", op, plen, 5);

	  if (partial_clobber)
	    avr_asm_len ("mov __tmp_reg__,%0", &base2, plen, 1);

	  return "";
	}
      else if (REGNO (XEXP (x, 0)) == REG_X)
	{
	  /* This is a paranoid case LEGITIMIZE_RELOAD_ADDRESS must exclude
	     it but I have this situation with extremal optimizing options.  */

	  // PR117744: The base register overlaps dest and is
	  // only partially clobbered.
	  bool clobber_r26 = (partial_clobber
			      && REGNO (base) == (REGNO (base) & ~1));
	  if (partial_clobber
	      && ! clobber_r26)
	    avr_asm_len ("mov __tmp_reg__,%0", &base2, plen, 1);

	  avr_asm_len ("adiw r26,%o1" CR_TAB
		       "ld %0,X", op, plen, 2);

	  if (clobber_r26)
	    avr_asm_len ("subi r26,lo8(%o1)", op, plen, 1);
	  else if (partial_clobber)
	    avr_asm_len ("mov %0,__tmp_reg__", &base2, plen, 1);
	  else if (! reg_unused_after (insn, base))
	    avr_asm_len ("sbiw r26,%o1", op, plen, 1);

	  return "";
	}

      return avr_asm_len ("ldd %0,%1", op, plen, -1);
    }

  return avr_asm_len ("ld %0,%1", op, plen, -1);
}


/* Same as movhi_r_mr, but TINY does not have ADIW, SBIW and LDD */

static const char *
avr_out_movhi_r_mr_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);

  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  if (reg_dest == reg_base)  /* R = (R) */
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

static const char *
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

static const char *
avr_out_movhi_r_mr_pre_dec_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);

  /* "volatile" forces reading low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers.  */
  bool mem_volatile_p = MEM_VOLATILE_P (src);

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


static const char *
out_movhi_r_mr (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);
  /* "volatile" forces reading low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers.  */
  bool mem_volatile_p = MEM_VOLATILE_P (src);

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

static const char *
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


static const char *
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

static const char *
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

      if (reg_base == REG_X)  /* (R26) */
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

static const char *
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
	  return *l = 7, ("mov __tmp_reg__,%B1"   CR_TAB
			  "st %0,%A1"             CR_TAB
			  TINY_ADIW (%E0, %F0, 1) CR_TAB
			  "st %0+,__tmp_reg__"    CR_TAB
			  "st %0+,%C1"            CR_TAB
			  "st %0+,%D1");
	}
      else
	{
	  return *l = 9, ("mov __tmp_reg__,%B1"   CR_TAB
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

static const char *
avr_out_movsi_mr_r_reg_disp_tiny (rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = REGNO (XEXP (base, 0));
  int reg_src = true_regnum (src);

  if (reg_base == reg_src)
    {
      *l = 11;
      return ("mov __tmp_reg__,%A1"        CR_TAB
	      "mov __zero_reg__,%B1"       CR_TAB
	      TINY_ADIW (%I0, %J0, %o0)    CR_TAB
	      "st %b0+,__tmp_reg__"        CR_TAB
	      "st %b0+,__zero_reg__"       CR_TAB
	      "st %b0+,%C1"                CR_TAB
	      "st %b0,%D1"                 CR_TAB
	      "clr __zero_reg__"           CR_TAB
	      TINY_SBIW (%I0, %J0, %o0+3));
    }
  else if (reg_src == reg_base - 2)
    {
      // This awkward case can occur when ext-dce turns zero-extend:SI(HI)
      // into a paradoxical subreg, which register allocation may turn into
      // something like *(R28:HI + 7) = R26:SI.  There is actually no need
      // to store the upper 2 bytes of R26:SI as they are unused rubbish.
      // See PR116390.
      *l = 6;
      return (TINY_ADIW (%I0, %J0, %o0)     CR_TAB
	      "st %b0+,%A1"                 CR_TAB
	      "st %b0,%B1"                  CR_TAB
	      TINY_SBIW (%I0, %J0, %o0+1));
    }
  *l = 8;
  return (TINY_ADIW (%I0, %J0, %o0)     CR_TAB
	  "st %b0+,%A1"                 CR_TAB
	  "st %b0+,%B1"                 CR_TAB
	  "st %b0+,%C1"                 CR_TAB
	  "st %b0,%D1"                  CR_TAB
	  TINY_SBIW (%I0, %J0, %o0+3));
}

static const char *
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
	  return *l=4,("out %i0,%A1"   CR_TAB
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

  if (reg_base > 0)  /* (r) */
    {
      if (AVR_TINY)
	return avr_out_movsi_mr_r_reg_no_disp_tiny (insn, op, l);

      if (reg_base == REG_X)  /* (R26) */
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

  gcc_assert (GET_MODE_SIZE (GET_MODE (dest)) == 4);

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

static const char *
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

static const char *
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

static const char *
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

      if (reg_base == REG_X)  /* (R26) */
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
	return avr_asm_len ("ldd %C0,%C1"          CR_TAB
			    "ldd __tmp_reg__,%B1"  CR_TAB
			    "ldd %A0,%A1"          CR_TAB
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
      return avr_asm_len ("lds %A0,%m1"   CR_TAB
			  "lds %B0,%m1+1" CR_TAB
			  "lds %C0,%m1+2", op, plen , -n_words);
    }

  fatal_insn ("unknown move insn:",insn);
  return "";
}


static const char *
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

static const char *
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

static const char *
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

  if (reg_base > 0)  /* (r) */
    {
      if (AVR_TINY)
	return avr_out_store_psi_reg_no_disp_tiny (insn, op, plen);

      if (reg_base == REG_X)  /* (R26) */
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

static const char *
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

static const char *
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

static const char *
avr_out_movhi_mr_r_xmega (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);

  /* "volatile" forces writing low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers like SP.  */
  bool mem_volatile_p = MEM_VOLATILE_P (dest);

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

static const char *
avr_out_movhi_mr_r_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);
  bool mem_volatile_p = MEM_VOLATILE_P (dest);

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
		       "st %0,%A1", op, plen, -7);
    }

  return !mem_volatile_p && reg_unused_after (insn, base)
    ? avr_asm_len ("st %0+,%A1" CR_TAB
		   "st %0,%B1", op, plen, -2)
    : avr_asm_len (TINY_ADIW (%E0, %F0, 1) CR_TAB
		   "st %0,%B1"             CR_TAB
		   "st -%0,%A1", op, plen, -4);
}

static const char *
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

static const char *
avr_out_movhi_mr_r_post_inc_tiny (rtx op[], int *plen)
{
  return avr_asm_len (TINY_ADIW (%I0, %J0, 1)  CR_TAB
		      "st %p0,%B1"    CR_TAB
		      "st -%p0,%A1"   CR_TAB
		      TINY_ADIW (%I0, %J0, 2), op, plen, -6);
}

static const char *
out_movhi_mr_r (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);

  /* "volatile" forces writing high-byte first (no-xmega) resp.
     low-byte first (xmega) even if less efficient, for correct
     operation with 16-bit I/O registers like.  */

  if (AVR_XMEGA)
    return avr_out_movhi_mr_r_xmega (insn, op, plen);

  bool mem_volatile_p = MEM_VOLATILE_P (dest);

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


/* Output code for the "set_some" insn that sets some QImode GPRs.
   $0 is a parallel; for its layout see the description of the next function.
   $0[5] ... $0[8] are SETs of QImode registers to const_int values.  All
      of them are bytes in the register described by $3 and $4.  SET $0[5]
      is mandatory, but all the following ones are optional.
   $1 is a QImode scratch d-register or const0_rtx.
   $2 is the known 8-bit value held in $1 before the insn starts.  When the
      code below clobbers $1, then it must restore $1 to $2 at the end.
   $3 The register number of a GPR.
   $4 The modesize of $3 in 1...4.
   PLEN == 0:  Output instructions.
   PLEN != 0:  Set *PLEN to the length of the sequence in words.  */

const char *
avr_out_set_some (rtx_insn *insn, rtx *xop, int *plen)
{
  const int vlen = XVECLEN (xop[0], 0);
  const int sets_start = 5;
  gcc_assert (vlen > sets_start);

  if (plen)
    *plen = 0;

  rtx op[4];
  rtx &dest = op[0], &src = op[1], &scratch = op[2], &oldval = op[3];
  scratch = REG_P (xop[1]) ? xop[1] : NULL_RTX;
  oldval = NULL_RTX;

  /* There are 3 ways to get a scratch, starting withe the most preferred ones:
     1) avr_find_unused_d_reg() need not to be restored, and it takes care
	of fixed regs.  This is an unlikely case, e.g. with -fno-peephole2.
     2) "set_some" provides a scratch register with a known content.
	This scratch need not be saved but has to be restored to its value.
     3) A last resort approach saves and restores some upper register.
     Notice that "set_some" will only be emit when avr-fuse-move is fed
     with mov insn(s) that don't have a scratch reg but need one;
     hence "set_some" won't have a scratch reg at its disposal, either.  */

  bool knows_way_p = false;

  for (int i = sets_start; i < vlen; ++i)
    {
      rtx xset = XVECEXP (xop[0], 0, i);

      gcc_assert (GET_CODE (xset) == SET
		  && REG_P (dest = XEXP (xset, 0))
		  && CONST_INT_P (src = XEXP (xset, 1)));

      if (src == const0_rtx)
	avr_asm_len ("clr %0", op, plen, 1);
      else if (src == const1_rtx)
	avr_asm_len ("clr %0" CR_TAB
		     "inc %0", op, plen, 2);
      else if (src == constm1_rtx)
	avr_asm_len ("clr %0" CR_TAB
		     "dec %0", op, plen, 2);
      else
	{
	  if (! knows_way_p)
	    {
	      knows_way_p = true;

	      static const machine_mode size_to_mode[4 + 1] =
		{
		  VOIDmode, QImode, HImode, PSImode, SImode
		};

	      const int ex_regno = INTVAL (xop[3]);
	      const int ex_modesize = INTVAL (xop[4]);
	      rtx exclude = gen_rtx_REG (size_to_mode[ex_modesize], ex_regno);
	      rtx dreg = avr_find_unused_d_reg (insn, exclude);

	      if (dreg)
		{
		  // Way 1
		  scratch = dreg;
		  oldval = NULL_RTX;
		}
	      else if (scratch)
		{
		  // Way 2
		  if (! reg_unused_after (insn, scratch))
		    oldval = xop[2];
		}
	      else
		{
		  // Way 3
		  scratch = all_regs_rtx[REG_24];
		  oldval = tmp_reg_rtx;
		  avr_asm_len ("mov %3,%2", op, plen, 1);
		}
	    } // decide about way

	  avr_asm_len ("ldi %2,%1" CR_TAB
		       "mov %0,%2", op, plen, 2);
	} // needs a scratch
    } // for $0[5] ... $0[8].

  if (oldval)
    avr_asm_len (REG_P (oldval)
		 ? "mov %2,%3"
		 : "ldi %2,%3", op, plen, 1);
  return "";
}


/* Implements the `set_some_operation' predicate.
   PARA is a parallel with the following elements:
   [0] is a USE of an 8-bit scratch d-register or const0_rtx.
   [1] is the known value held in [0].  When [0] is used as a scratch,
       then its value has to be restored to [1] after the respective insn.
   [2] is the regno of a GPR, and
   [3] is the mode size of that GPR.  All SETs [5]... of PARA will set
       bytes of that GPR, but in many cases not all of them.
   [4]  In a clobber of REG_CC.
   [5] [6] [7] [8]  SETs of an 8-bit register to a const_int value, where
       all destinations are sub-bytes of [2].  Element [5] is mandatory,
       and the following elements are optional.  */

bool
avr_set_some_operation (rtx para)
{
  const int sets_start = 5;
  const int n_sets = XVECLEN (para, 0) - sets_start;

  if (! IN_RANGE (n_sets, 1, 4))
    return false;

  if (GET_CODE (XVECEXP (para, 0, 4)) != CLOBBER
      || GET_CODE (XVECEXP (para, 0, 0)) != USE
      || GET_CODE (XVECEXP (para, 0, 1)) != USE
      || GET_CODE (XVECEXP (para, 0, 2)) != USE
      || GET_CODE (XVECEXP (para, 0, 3)) != USE)
    return false;

  for (int i = sets_start; i < XVECLEN (para, 0); ++i)
    {
      rtx xset = XVECEXP (para, 0, i);
      if (! xset
	  || GET_CODE (xset) != SET
	  || ! register_operand (XEXP (xset, 0), QImode)
	  || ! const_int_operand (XEXP (xset, 1), QImode))
	return false;
    }

  return true;
}


/* Implement `TARGET_FRAME_POINTER_REQUIRED'.  */
/* Return 1 if frame pointer for current function required.  */

static bool
avr_frame_pointer_required_p (void)
{
  return (cfun->calls_alloca
	  || cfun->calls_setjmp
	  || cfun->has_nonlocal_label
	  || crtl->args.info.has_stack_args
	  || get_frame_size () > 0);
}


/* Returns the condition of the branch following INSN, where INSN is some
   comparison.  If the next insn is not a branch or the condition code set
   by INSN might be used by more insns than the next one, return UNKNOWN.
   For now, just look at the next insn, which misses some opportunities like
   following jumps.  */

static rtx_code
compare_condition (rtx_insn *insn)
{
  rtx set;
  rtx_insn *next = next_real_nondebug_insn (insn);

  if (next
      && JUMP_P (next)
      // If SREG does not die in the next insn, it is used in more than one
      // branch.  This can happen due to pass .avr-ifelse optimizations.
      && dead_or_set_regno_p (next, REG_CC)
      // Branches are (set (pc) (if_then_else (COND (...)))).
      && (set = single_set (next))
      && GET_CODE (SET_SRC (set)) == IF_THEN_ELSE)
    {
      return GET_CODE (XEXP (SET_SRC (set), 0));
    }

  return UNKNOWN;
}


/* Returns true if INSN is a tst insn that only tests the sign.  */

static bool
compare_sign_p (rtx_insn *insn)
{
  rtx_code cond = compare_condition (insn);
  return (cond == GE || cond == LT);
}


/* Returns true if INSN is a compare insn with the EQ or NE condition.  */

static bool
compare_eq_p (rtx_insn *insn)
{
  rtx_code cond = compare_condition (insn);
  return (cond == EQ || cond == NE);
}


/* Implement `TARGET_CANONICALIZE_COMPARISON'.  */
/* Basically tries to convert "difficult" comparisons like GT[U]
   and LE[U] to simple ones.  Some asymmetric comparisons can be
   transformed to EQ or NE against zero.  */

static void
avr_canonicalize_comparison (int *icode, rtx *op0, rtx *op1, bool op0_fixed)
{
  rtx_code code = (rtx_code) *icode;
  machine_mode mode = GET_MODE (*op0);

  bool signed_p = code == GT || code == LE;
  bool unsigned_p = code == GTU || code == LEU;
  bool difficult_p = signed_p || unsigned_p;

  if (// Only do integers and fixed-points.
      (! SCALAR_INT_MODE_P (mode)
       && ! ALL_SCALAR_FIXED_POINT_MODE_P (mode))
      // Only do comparisons against a register.
      || ! register_operand (*op0, mode))
    return;

  // Canonicalize "difficult" reg-reg comparisons.

  if (! op0_fixed
      && difficult_p
      && register_operand (*op1, mode))
    {
      std::swap (*op0, *op1);
      *icode = (int) swap_condition (code);
      return;
    }

  // Canonicalize comparisons against compile-time constants.

  if (CONST_INT_P (*op1)
      || CONST_FIXED_P (*op1))
    {
      // INT_MODE of the same size.
      scalar_int_mode imode = int_mode_for_mode (mode).require ();

      unsigned HOST_WIDE_INT mask = GET_MODE_MASK (imode);
      unsigned HOST_WIDE_INT maxval = signed_p ? mask >> 1 : mask;

      // Convert value *op1 to imode.
      rtx xval = simplify_gen_subreg (imode, *op1, mode, 0);

      // Canonicalize difficult comparisons against const.
      if (difficult_p
	  && (UINTVAL (xval) & mask) != maxval)
	{
	  // Convert *op0 > *op1  to *op0 >= 1 + *op1.
	  // Convert *op0 <= *op1 to *op0 <  1 + *op1.
	  xval = simplify_binary_operation (PLUS, imode, xval, const1_rtx);

	  // Convert value back to its original mode.
	  *op1 = simplify_gen_subreg (mode, xval, imode, 0);

	  // Map  >  to  >=  and  <=  to  <.
	  *icode = (int) avr_normalize_condition (code);

	  return;
	}

      // Some asymmetric comparisons can be turned into EQ or NE.
      if (code == LTU && xval == const1_rtx)
	{
	  *icode = (int) EQ;
	  *op1 = CONST0_RTX (mode);
	  return;
	}

      if (code == GEU && xval == const1_rtx)
	{
	  *icode = (int) NE;
	  *op1 = CONST0_RTX (mode);
	  return;
	}
    }
}


/* Try to turn a GEU or LTU comparison of register XOP[1] into an
   NE / EQ comparison of the higher bytes of XOP[1] against 0.
   XOP[1] has scalar int or scalar fixed-point mode of 2, 3 or 4 bytes.
   XOP[2] is a compile-time constant, and XOP[0] = XOP[1] <comp> XOP[2]
   is the comparison operator.  XOP[3] is the branch label, and XOP[4]
   is a QImode scratch operand.
      When XOP[1] (viewed as a CONST_INT) is an integral power of 256,
   then a GTU or LTU comparison can be turned into a NE or EQ comparison
   of the high bytes against zero.  For example, the C code

	if (x >= 1)
	  ccc = 0;

   where x is an unsigned _Accum may be compiled as:

	or r24,r25		 ;  *cmpsi_lsr
	breq .L1		 ;  branch
	sts ccc,__zero_reg__	 ;  movqi_insn
     .L1:

   In the case of success, the operands will be such that they comprise
   a *cmp<mode>_lsr insn, where mode is HI, PSI or SI, and XOP[0] will be
   a NE or EQ branch condition.  Otherwise, XOP[] is unchanged.  */

void
avr_maybe_cmp_lsr (rtx *xop)
{
  rtx_code comp = GET_CODE (xop[0]);

  if ((comp == GEU || comp == LTU)
      && (CONST_INT_P (xop[2]) || CONST_FIXED_P (xop[2])))
    {
      rtx xreg = avr_to_int_mode (xop[1]);
      rtx xval = avr_to_int_mode (xop[2]);
      machine_mode imode = GET_MODE (xreg);
      auto uval = UINTVAL (xval) & GET_MODE_MASK (imode);
      int shift = exact_log2 (uval);

      if (shift == 8 || shift == 16 || shift == 24)
	{
	  // Operands such that the compare becomes *cmp<mode>_lsr.
	  xop[1] = gen_rtx_LSHIFTRT (imode, xreg, GEN_INT (shift));
	  xop[2] = const0_rtx;
	  xop[4] = gen_rtx_SCRATCH (QImode);
	  // Branch condition.
	  xop[0] = gen_rtx_fmt_ee (comp == GEU ? NE : EQ,
				   VOIDmode, xop[1], xop[2]);
	}
    }
}


/* Output an EQ / NE compare of HI, PSI or SI register XOP[0] against 0,
   where only the bits starting at XOP[1] are relevant.  XOP[1] is a
   const_int that is 8, 16 or 24.  Return "".
   PLEN == 0:  Output instructions.
   PLEN != 0:  Set *PLEN to the length of the sequence in words.  */

const char *
avr_out_cmp_lsr (rtx_insn *insn, rtx *xop, int *plen)
{
  rtx xreg = xop[0];
  const int n_bytes = GET_MODE_SIZE (GET_MODE (xreg));
  const int shift = INTVAL (xop[1]);
  const rtx_code cond = compare_condition (insn);

  gcc_assert (shift == 8 || shift == 16 || shift == 24);
  gcc_assert (shift < 8 * n_bytes);
  gcc_assert (cond == UNKNOWN || cond == NE || cond == EQ);

  const bool used_p = ! reg_unused_after (insn, xreg);

  if (plen)
    *plen = 0;

  if (shift / 8 == n_bytes - 1)
    {
      rtx xmsb = avr_byte (xreg, n_bytes - 1);
      avr_asm_len ("tst %0", &xmsb, plen, 1);
    }
  else if (n_bytes == 4
	   && shift <= 16
	   && AVR_HAVE_ADIW
	   && REGNO (xreg) >= REG_22
	   // The sequence also works when xreg is unused after,
	   // but SBIW is slower than OR.
	   && used_p)
    {
      avr_asm_len ("sbiw %C0,0", &xreg, plen, 1);
      if (shift == 8)
	avr_asm_len ("cpc %B0,__zero_reg__", &xreg, plen, 1);
    }
  else
    {
      rtx op[2] = { avr_byte (xreg, shift / 8), tmp_reg_rtx };
      if (used_p)
	{
	  avr_asm_len ("mov %1,%0", op, plen, 1);
	  op[0] = tmp_reg_rtx;
	}

      for (int i = 1 + shift / 8; i < n_bytes; ++i)
	{
	  op[1] = avr_byte (xreg, i);
	  avr_asm_len ("or %0,%1", op, plen, 1);
	}
    }

  return "";
}


/* Output compare instruction

      compare (XOP[0], XOP[1])

   for a register XOP[0] and a compile-time constant XOP[1].  Return "".
   XOP[2] is an 8-bit scratch register as needed.

   PLEN == NULL:  Output instructions.
   PLEN != NULL:  Set *PLEN to the length (in words) of the sequence.
                  Don't output anything.  */

const char *
avr_out_compare (rtx_insn *insn, rtx *xop, int *plen)
{
  /* Register to compare and value to compare against. */
  rtx xreg = xop[0];
  rtx xval = xop[1];

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

  gcc_assert (REG_P (xreg));
  gcc_assert ((CONST_INT_P (xval) && n_bytes <= 4)
	      || (const_double_operand (xval, VOIDmode) && n_bytes == 8));

  if (plen)
    *plen = 0;

  const rtx_code cond = compare_condition (insn);
  const bool eqne_p = cond == EQ || cond == NE;

  /* Comparisons == +/-1 and != +/-1 can be done similar to camparing
     against 0 by ORing the bytes.  This is one instruction shorter.
     Notice that 64-bit comparisons are always against reg:ALL8 18 (ACC_A)
     and therefore don't use this.  */

  if (eqne_p
      && ! test_hard_reg_class (LD_REGS, xreg)
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

  /* Comparisons == and != may change the order in which the sub-bytes are
     being compared.  Start with the high 16 bits so we can use SBIW.  */

  if (n_bytes == 4
      && eqne_p
      && AVR_HAVE_ADIW
      && REGNO (xreg) >= REG_22
      && (xval == const0_rtx
	  || (IN_RANGE (avr_int16 (xval, 2), 0, 63)
	      && reg_unused_after (insn, xreg))))
    {
      xop[2] = avr_word (xval, 2);
      return avr_asm_len ("sbiw %C0,%2"      CR_TAB
			  "sbci %B0,hi8(%1)" CR_TAB
			  "sbci %A0,lo8(%1)", xop, plen, 3);
    }

  bool changed[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };

  /* The >= and < comparisons may skip the lower bytes when the according bytes
     of the constant are all zeros.  In that case, the comparison may start
     at a byte other than the LSB.  */

  const int start = ((cond == GEU || cond == LTU || cond == GE || cond == LT)
		     && INTVAL (xval) != 0)
    ? ctz_hwi (INTVAL (xval)) / 8
    : 0;

  for (int i = start; i < n_bytes; i++)
    {
      /* We compare byte-wise.  */
      xop[0] = avr_byte (xreg, i);
      xop[1] = avr_byte (xval, i);

      /* 8-bit value to compare with this byte.  */
      unsigned int val8 = avr_uint8 (xval, i);

      /* Word registers >= R24 can use SBIW/ADIW with 0..63.  */

      if (i == start
	  && i % 2 == 0
	  && n_bytes - start >= 2
	  && avr_adiw_reg_p (xop[0]))
	{
	  int val16 = avr_int16 (xval, i);

	  if (IN_RANGE (val16, 0, 63)
	      && (val8 == 0
		  || reg_unused_after (insn, xreg)))
	    {
	      avr_asm_len ("sbiw %0,%1", xop, plen, 1);
	      changed[i] = changed[i + 1] = val8 != 0;
	      i++;
	      continue;
	    }

	  if (IN_RANGE (val16, -63, -1)
	      && eqne_p
	      && n_bytes - start == 2
	      && reg_unused_after (insn, xreg))
	    {
	      return avr_asm_len ("adiw %0,%n1", xop, plen, 1);
	    }
	}

      /* Comparing against 0 is easy.  */

      if (val8 == 0)
	{
	  avr_asm_len (i == start
		       ? "cp %0,__zero_reg__"
		       : "cpc %0,__zero_reg__", xop, plen, 1);
	  continue;
	}

      /* Upper registers can compare and subtract-with-carry immediates.
	 Notice that compare instructions do the same as respective subtract
	 instruction; the only difference is that comparisons don't write
	 the result back to the target register.  */

      if (test_hard_reg_class (LD_REGS, xop[0]))
	{
	  if (i == start)
	    {
	      avr_asm_len ("cpi %0,%1", xop, plen, 1);
	      continue;
	    }
	  else if (reg_unused_after (insn, xreg))
	    {
	      avr_asm_len ("sbci %0,%1", xop, plen, 1);
	      changed[i] = true;
	      continue;
	    }
	}

      /* When byte comparisons for an EQ or NE comparison look like
	     compare (x[i], C)
	     compare (x[j], C)
	 then we can instead use
	     compare (x[i], C)
	     compare (x[j], x[i])
	 which is shorter, and the outcome of the comparison is the same.  */

      if (eqne_p)
	{
	  bool found = false;

	  for (int j = start; j < i && ! found; ++j)
	    if (val8 == avr_uint8 (xval, j)
		// Make sure that we didn't clobber x[j] above.
		&& ! changed[j])
	      {
		rtx op[] = { xop[0], avr_byte (xreg, j) };
		avr_asm_len ("cpc %0,%1", op, plen, 1);
		found = true;
	      }

	  if (found)
	    continue;
	}

      /* Must load the value into the scratch register.  */

      gcc_assert (REG_P (xop[2]));

      if (clobber_val != (int) val8)
	avr_asm_len ("ldi %2,%1", xop, plen, 1);
      clobber_val = (int) val8;

      avr_asm_len (i == start
		   ? "cp %0,%2"
		   : "cpc %0,%2", xop, plen, 1);
    }

  return "";
}


/* Prepare operands of compare_const_di2 to be used with avr_out_compare.  */

const char *
avr_out_compare64 (rtx_insn *insn, rtx *op, int *plen)
{
  rtx xop[3] = { gen_rtx_REG (DImode, ACC_A), op[0], op[1] };

  return avr_out_compare (insn, xop, plen);
}


/* Output test instruction for HImode.  */

const char *
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

const char *
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

const char *
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


/* Output a comparison of a zero- or sign-extended register against a
   plain register.  CODE is SIGN_EXTEND or ZERO_EXTEND.  Return "".

   PLEN != 0: Set *PLEN to the code length in words. Don't output anything.
   PLEN == 0: Print instructions.  */

const char *
avr_out_cmp_ext (rtx xop[], rtx_code code, int *plen)
{
  // The smaller reg is the one that's to be extended.  Get its index as z.
  int z = GET_MODE_SIZE (GET_MODE (xop[1])) < GET_MODE_SIZE (GET_MODE (xop[0]));
  rtx zreg = xop[z];
  rtx reg = xop[1 - z];
  machine_mode mode = GET_MODE (reg);
  machine_mode zmode = GET_MODE (zreg);
  rtx zex;

  if (plen)
    *plen = 0;

  // zex holds the extended bytes above zreg.  This is 0 for ZERO_EXTEND,
  // and 0 or -1 for SIGN_EXTEND.

  if (code == SIGN_EXTEND)
    {
      // Sign-extend the high-byte of zreg to tmp_reg.
      int zmsb = GET_MODE_SIZE (zmode) - 1;
      rtx xzmsb = avr_byte (zreg, zmsb);

      avr_asm_len ("mov __tmp_reg__,%0" CR_TAB
		   "rol __tmp_reg__"    CR_TAB
		   "sbc __tmp_reg__,__tmp_reg__", &xzmsb, plen, 3);
      zex = tmp_reg_rtx;
    }
  else if (code == ZERO_EXTEND)
    {
      zex = zero_reg_rtx;
    }
  else
    gcc_unreachable ();

  // Now output n_bytes bytes of the very comparison.

  int n_bytes = GET_MODE_SIZE (mode);

  avr_asm_len ("cp %0,%1", xop, plen, 1);

  for (int b = 1; b < n_bytes; ++b)
    {
      rtx regs[2];
      regs[1 - z] = avr_byte (reg, b);
      regs[z] = b < GET_MODE_SIZE (zmode) ? avr_byte (zreg, b) : zex;

      avr_asm_len ("cpc %0,%1", regs, plen, 1);
    }

  return "";
}


/* Generate asm equivalent for various shifts.  This only handles cases
   that are not already carefully hand-optimized in ?sh<mode>3_out.

   OPERANDS[0] resp. %0 in TEMPL is the operand to be shifted.
   OPERANDS[2] is the shift count as CONST_INT, MEM or REG.
   OPERANDS[3] is a QImode scratch register from LD regs if
               available and SCRATCH, otherwise (no scratch available)

   TEMPL is an assembler template that shifts by one position.
   T_LEN is the length of this template.
   PLEN != 0: Set *PLEN to the length of the sequence in words.
   PLEN == 0: Output instructions.  */

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
      /* Operand 3 is a scratch register if this is a
         parallel with three elements i.e. a set,
         a clobber of a scratch, and clobber of REG_CC.
         If a scratch reg is not available, then the parallel
         will contain only a set and clobber of REG_CC. */
      bool scratch = (GET_CODE (PATTERN (insn)) == PARALLEL
		      && XVECLEN (PATTERN (insn), 0) == 3
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

	  op[3] = all_regs_rtx[((REGNO (op[0]) - 1) & 15) + REG_16];
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
ashlqi3_out (rtx_insn *insn, rtx operands[], int *plen)
{
  if (CONST_INT_P (operands[2]))
    {
      int reg0 = REGNO (operands[0]);
      int reg1 = REGNO (operands[1]);
      bool ldreg_p = test_hard_reg_class (LD_REGS, operands[0]);
      int offs = INTVAL (operands[2]);

      if (plen)
	*plen = 0;

      if (offs <= 3
	  || (offs <= 5 && ! ldreg_p))
	{
	  for (int i = 0; i < offs; ++i)
	    avr_asm_len ("lsl %0", operands, plen, 1);
	  return "";
	}

      switch (offs)
	{
	default:
	  if (offs < 8)
	    break;
	  return avr_asm_len ("clr %0", operands, plen, 1);

	case 1:
	case 2:
	case 3:
	  gcc_unreachable ();

	case 4:
	  return avr_asm_len ("swap %0" CR_TAB
			      "andi %0,0xf0", operands, plen, 2);
	case 5:
	  return avr_asm_len ("swap %0" CR_TAB
			      "lsl %0"  CR_TAB
			      "andi %0,0xe0", operands, plen, 3);
	case 6:
	  if (ldreg_p && reg0 == reg1)
	    return avr_asm_len ("swap %0" CR_TAB
				"lsl %0"  CR_TAB
				"lsl %0"  CR_TAB
				"andi %0,0xc0", operands, plen, 4);
	  if (ldreg_p && reg0 != reg1 && AVR_HAVE_MUL)
	    return avr_asm_len ("ldi %0,1<<6" CR_TAB
				"mul %0,%1"   CR_TAB
				"mov %0,r0"   CR_TAB
				"clr __zero_reg__", operands, plen, 4);
	  return reg0 != reg1
	    ? avr_asm_len ("clr %0"    CR_TAB
			   "bst %1,0"  CR_TAB
			   "bld %0,6"  CR_TAB
			   "bst %1,1"  CR_TAB
			   "bld %0,7", operands, plen, 5)
	    : avr_asm_len ("lsl %0"  CR_TAB
			   "lsl %0"  CR_TAB
			   "lsl %0"  CR_TAB
			   "lsl %0"  CR_TAB
			   "lsl %0"  CR_TAB
			   "lsl %0", operands, plen, 6);
	case 7:
	  return avr_asm_len ("bst %1,0" CR_TAB
			      "clr %0"   CR_TAB
			      "bld %0,7", operands, plen, 3);
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt ("lsl %0",
		      insn, operands, plen, 1);
  return "";
}


/* Output a 16-bit left shift  XOP[0] = XOP[1] << XOP[2]  using MUL.
   XOP[3] is an upper 8-bit scratch register.  This function is currently
   only used for offsets 5 and 6 but works for offsets 1...7 as well.  */

static const char*
avr_out_ashlhi3_mul (rtx *xop, bool scratch_p, int *plen)
{
  gcc_assert (scratch_p && AVR_HAVE_MUL);

  // Takes 7 words and 9 cycles.
  return avr_asm_len ("ldi %3,1<<%2" CR_TAB
		      "mul %B1,%3"   CR_TAB
		      "mov %B0,r0"   CR_TAB
		      "mul %A1,%3"   CR_TAB
		      "mov %A0,r0"   CR_TAB
		      "or  %B0,r1"   CR_TAB
		      "clr __zero_reg__", xop, plen, -7);
}


/* 16bit shift left ((short)x << i)   */

const char *
ashlhi3_out (rtx_insn *insn, rtx operands[], int *plen)
{
  if (CONST_INT_P (operands[2]))
    {
      bool scratch = (GET_CODE (PATTERN (insn)) == PARALLEL
		      && XVECLEN (PATTERN (insn), 0) == 3
		      && REG_P (operands[3]));
      bool ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      bool reg1_unused_after = reg_unused_after (insn, operands[1]);
      int size;
      int reg0 = REGNO (operands[0]);
      int reg1 = REGNO (operands[1]);
      bool use_mul_p = reg1 != reg0 || (scratch && AVR_HAVE_MUL);

      if (plen)
	*plen = 0;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 16)
	    break;

	  return avr_asm_len ("clr %B0" CR_TAB
			      "clr %A0", operands, plen, 2);
	case 4:
	  if (avr_optimize_size_max_p () && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    return avr_asm_len ("swap %A0"      CR_TAB
				"swap %B0"      CR_TAB
				"andi %B0,0xf0" CR_TAB
				"eor %B0,%A0"   CR_TAB
				"andi %A0,0xf0" CR_TAB
				"eor %B0,%A0", operands, plen, 6);
	  if (scratch)
	    return avr_asm_len ("swap %A0"    CR_TAB
				"swap %B0"    CR_TAB
				"ldi %3,0xf0" CR_TAB
				"and %B0,%3"  CR_TAB
				"eor %B0,%A0" CR_TAB
				"and %A0,%3"  CR_TAB
				"eor %B0,%A0", operands, plen, 7);
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  size = (scratch ? 5 : 6) + (reg1 != reg0) * (2 - AVR_HAVE_MOVW);
	  if (avr_optimize_size_max_p () && (size < 7 || !use_mul_p))
	    {
	      if (reg0 != reg1)
		{
		  if (AVR_HAVE_MOVW)
		    avr_asm_len ("movw %0,%1", operands, plen, 1);
		  else
		    avr_asm_len ("mov %A0,%A1" CR_TAB
				 "mov %B0,%B1", operands, plen, 2);
		}
	      break;  // scratch ? 5 : 6
	    }

	  if (use_mul_p)
	    return avr_out_ashlhi3_mul (operands, scratch, plen); // 7

	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    return avr_asm_len ("lsl %A0"       CR_TAB
				"rol %B0"       CR_TAB
				"swap %A0"      CR_TAB
				"swap %B0"      CR_TAB
				"andi %B0,0xf0" CR_TAB
				"eor %B0,%A0"   CR_TAB
				"andi %A0,0xf0" CR_TAB
				"eor %B0,%A0", operands, plen, 8);
	  if (scratch)
	    return avr_asm_len ("lsl %A0"     CR_TAB
				"rol %B0"     CR_TAB
				"swap %A0"    CR_TAB
				"swap %B0"    CR_TAB
				"ldi %3,0xf0" CR_TAB
				"and %B0,%3"  CR_TAB
				"eor %B0,%A0" CR_TAB
				"and %A0,%3"  CR_TAB
				"eor %B0,%A0", operands, plen, 9);
	  break;  /* 10 */

	case 6:
	  size = (scratch ? 5 : 6) + (reg1 != reg0) * (2 - AVR_HAVE_MOVW);
	  if (avr_optimize_size_max_p () && (size < 7 || !use_mul_p))
	    {
	      if (reg0 != reg1)
		{
		  if (AVR_HAVE_MOVW)
		    avr_asm_len ("movw %0,%1", operands, plen, 1);
		  else
		    avr_asm_len ("mov %A0,%A1" CR_TAB
				 "mov %B0,%B1", operands, plen, 2);
		}
	      break;  // scratch ? 5 : 6
	    }

	  if (use_mul_p)
	    return avr_out_ashlhi3_mul (operands, scratch, plen); // 7

	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  return avr_asm_len ("clr __tmp_reg__" CR_TAB
			      "lsr %B0"         CR_TAB
			      "ror %A0"         CR_TAB
			      "ror __tmp_reg__" CR_TAB
			      "lsr %B0"         CR_TAB
			      "ror %A0"         CR_TAB
			      "ror __tmp_reg__" CR_TAB
			      "mov %B0,%A0"     CR_TAB
			      "mov %A0,__tmp_reg__", operands, plen, 9);
	case 7:
	  return reg1_unused_after
	    ? avr_asm_len ("lsr %B1"     CR_TAB
			   "mov %B0,%A1" CR_TAB
			   "clr %A0"     CR_TAB
			   "ror %B0"     CR_TAB
			   "ror %A0", operands, plen, 5)
	    : avr_asm_len ("bst %B1,0"   CR_TAB
			   "mov %B0,%A1" CR_TAB
			   "clr %A0"     CR_TAB
			   "ror %B0"     CR_TAB
			   "ror %A0"     CR_TAB
			   "bld %B0,7", operands, plen, 6);
	case 8:
	  return avr_asm_len ("mov %B0,%A1" CR_TAB
			      "clr %A0", operands, plen, 2);
	case 9:
	  return avr_asm_len ("mov %B0,%A1" CR_TAB
			      "clr %A0"     CR_TAB
			      "lsl %B0", operands, plen, 3);
	case 10:
	  return avr_asm_len ("mov %B0,%A1" CR_TAB
			      "clr %A0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0", operands, plen, 4);
	case 11:
	  return avr_asm_len ("mov %B0,%A1" CR_TAB
			      "clr %A0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0", operands, plen, 5);
	case 12:
	  if (ldi_ok)
	    return avr_asm_len ("mov %B0,%A1" CR_TAB
				"clr %A0"     CR_TAB
				"swap %B0"    CR_TAB
				"andi %B0,0xf0", operands, plen, 4);
	  if (scratch)
	    return avr_asm_len ("mov %B0,%A1" CR_TAB
				"clr %A0"     CR_TAB
				"swap %B0"    CR_TAB
				"ldi %3,0xf0" CR_TAB
				"and %B0,%3", operands, plen, 5);

	  return avr_asm_len ("mov %B0,%A1" CR_TAB
			      "clr %A0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0", operands, plen, 6);
	case 13:
	  if (ldi_ok)
	    return avr_asm_len ("mov %B0,%A0" CR_TAB
				"clr %A0"     CR_TAB
				"swap %B0"    CR_TAB
				"lsl %B0"     CR_TAB
				"andi %B0,0xe0", operands, plen, 5);
	  if (AVR_HAVE_MUL && scratch)
	    return avr_asm_len ("ldi %3,0x20" CR_TAB
				"mul %A0,%3"  CR_TAB
				"mov %B0,r0"  CR_TAB
				"clr %A0"     CR_TAB
				"clr __zero_reg__", operands, plen, 5);
	  if (optimize_size && scratch)
	    break;  /* 5 */

	  if (scratch)
	    return avr_asm_len ("mov %B0,%A0" CR_TAB
				"clr %A0"     CR_TAB
				"swap %B0"    CR_TAB
				"lsl %B0"     CR_TAB
				"ldi %3,0xe0" CR_TAB
				"and %B0,%3", operands, plen, 6);
	  if (AVR_HAVE_MUL)
	    return avr_asm_len ("set"        CR_TAB
				"bld r1,5"   CR_TAB
				"mul %A0,r1" CR_TAB
				"mov %B0,r0" CR_TAB
				"clr %A0"    CR_TAB
				"clr __zero_reg__", operands, plen, 6);
	  return avr_asm_len ("mov %B0,%A0" CR_TAB
			      "clr %A0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0"     CR_TAB
			      "lsl %B0", operands, plen, 7);
	case 14:
	  if (AVR_HAVE_MUL && ldi_ok)
	    return avr_asm_len ("ldi %B0,0x40" CR_TAB
				"mul %A0,%B0"  CR_TAB
				"mov %B0,r0"   CR_TAB
				"clr %A0"      CR_TAB
				"clr __zero_reg__", operands, plen, 5);
	  if (AVR_HAVE_MUL && scratch)
	    return avr_asm_len ("ldi %3,0x40" CR_TAB
				"mul %A0,%3"  CR_TAB
				"mov %B0,r0"  CR_TAB
				"clr %A0"     CR_TAB
				"clr __zero_reg__", operands, plen, 5);
	  if (optimize_size && ldi_ok)
	    return avr_asm_len ("mov %B0,%A0" CR_TAB
				"ldi %A0,6" "\n1:\t"
				"lsl %B0"     CR_TAB
				"dec %A0"     CR_TAB
				"brne 1b", operands, plen, 5);
	  if (optimize_size && scratch)
	    break;  /* 5 */

	  return avr_asm_len ("clr %B0" CR_TAB
			      "lsr %A0" CR_TAB
			      "ror %B0" CR_TAB
			      "lsr %A0" CR_TAB
			      "ror %B0" CR_TAB
			      "clr %A0", operands, plen, 6);
	case 15:
	  return avr_asm_len ("bst %A1,0" CR_TAB
			      "clr %A0"   CR_TAB
			      "clr %B0"   CR_TAB
			      "bld %B0,7", operands, plen, 4);
	} // switch
    }

  out_shift_with_cnt ("lsl %A0" CR_TAB
		      "rol %B0", insn, operands, plen, 2);
  return "";
}


/* 24-bit shift left */

const char *
avr_out_ashlpsi3 (rtx_insn *insn, rtx *op, int *plen)
{
  if (plen)
    *plen = 0;

  if (CONST_INT_P (op[2]))
    {
      int reg0 = REGNO (op[0]);
      int reg1 = REGNO (op[1]);
      bool reg1_unused_after = reg_unused_after (insn, op[1]);

      switch (INTVAL (op[2]))
	{
	default:
	  if (INTVAL (op[2]) < 24)
	    break;

	  return avr_asm_len ("clr %A0" CR_TAB
			      "clr %B0" CR_TAB
			      "clr %C0", op, plen, 3);
	case 8:
	  return reg0 >= reg1
	    ? avr_asm_len ("mov %C0,%B1"  CR_TAB
			   "mov %B0,%A1"  CR_TAB
			   "clr %A0", op, plen, 3)
	    : avr_asm_len ("clr %A0"      CR_TAB
			   "mov %B0,%A1"  CR_TAB
			   "mov %C0,%B1", op, plen, 3);
	case 15:
	  avr_asm_len (reg1_unused_after
		       ? "lsr %B1"
		       : "bst %B1,0", op, plen, 1);
	  if (reg0 + 2 != reg1)
	    avr_asm_len ("mov %C0,%A1", op, plen, 1);
	  avr_asm_len ("clr %A0"  CR_TAB
		       "clr %B0"  CR_TAB
		       "ror %C0"  CR_TAB
		       "ror %B0", op, plen, 5);
	  return reg1_unused_after
	    ? ""
	    : avr_asm_len ("bld %C0,7", op, plen, 1);

	case 16:
	  if (reg0 + 2 != reg1)
	    avr_asm_len ("mov %C0,%A1", op, plen, 1);

	  return avr_asm_len ("clr %B0"  CR_TAB
			      "clr %A0", op, plen, 2);
	case 23:
	  return avr_asm_len ("bst %A1,0" CR_TAB
			      "clr %A0"   CR_TAB
			      "clr %B0"   CR_TAB
			      "clr %C0"   CR_TAB
			      "bld %C0,7", op, plen, 5);
	}
    }

  out_shift_with_cnt ("lsl %A0" CR_TAB
		      "rol %B0" CR_TAB
		      "rol %C0", insn, op, plen, 3);
  return "";
}


/* 32bit shift left ((long)x << i)   */

const char *
ashlsi3_out (rtx_insn *insn, rtx operands[], int *plen)
{
  if (CONST_INT_P (operands[2]))
    {
      int off = INTVAL (operands[2]);
      int reg0 = true_regnum (operands[0]);
      int reg1 = true_regnum (operands[1]);
      bool reg1_unused_after = reg_unused_after (insn, operands[1]);
      bool scratch_p = (GET_CODE (PATTERN (insn)) == PARALLEL
			&& XVECLEN (PATTERN (insn), 0) == 3
			&& REG_P (operands[3]));
      if (plen)
	*plen = 0;

      switch (off)
	{
	default:
	  if (off < 32)
	    break;

	  return AVR_HAVE_MOVW
	    ? avr_asm_len ("clr %D0" CR_TAB
			   "clr %C0" CR_TAB
			   "movw %A0,%C0", operands, plen, 3)
	    : avr_asm_len ("clr %D0" CR_TAB
			   "clr %C0" CR_TAB
			   "clr %B0" CR_TAB
			   "clr %A0", operands, plen, 4);
	case 8:
	  return reg0 >= reg1
	    ? avr_asm_len ("mov %D0,%C1"  CR_TAB
			   "mov %C0,%B1"  CR_TAB
			   "mov %B0,%A1"  CR_TAB
			   "clr %A0", operands, plen, 4)
	    : avr_asm_len ("clr %A0"      CR_TAB
			   "mov %B0,%A1"  CR_TAB
			   "mov %C0,%B1"  CR_TAB
			   "mov %D0,%C1", operands, plen, 4);
	case 15:
	  avr_asm_len (reg1_unused_after
		       ? "lsr %C1"
		       : "bst %C1,0", operands, plen, 1);
	  if (reg0 + 2 != reg1)
	    {
	      if (AVR_HAVE_MOVW)
		avr_asm_len ("movw %C0,%A1", operands, plen, 1);
	      else
		avr_asm_len ("mov %C0,%A1"  CR_TAB
			     "mov %D0,%B1", operands, plen, 2);
	    }
	  avr_asm_len ("clr %A0"  CR_TAB
		       "clr %B0"  CR_TAB
		       "ror %D0"  CR_TAB
		       "ror %C0"  CR_TAB
		       "ror %B0", operands, plen, 5);
	  return reg1_unused_after
	    ? ""
	    : avr_asm_len ("bld %D0,7", operands, plen, 1);

	case 16:
	  if (reg0 + 2 == reg1)
	    return avr_asm_len ("clr %B0"  CR_TAB
				"clr %A0", operands, plen, 2);
	  return AVR_HAVE_MOVW
	    ? avr_asm_len ("movw %C0,%A1" CR_TAB
			   "clr %B0"      CR_TAB
			   "clr %A0", operands, plen, 3)
	    : avr_asm_len ("mov %C0,%A1"  CR_TAB
			   "mov %D0,%B1"  CR_TAB
			   "clr %B0"      CR_TAB
			   "clr %A0", operands, plen, 4);
	case 30:
	  if (AVR_HAVE_MUL && scratch_p)
	    return avr_asm_len ("ldi %3,1<<6"       CR_TAB
				"mul %3,%A1"        CR_TAB
				"mov %D0,r0"        CR_TAB
				"clr __zero_reg__"  CR_TAB
				"clr %C0"           CR_TAB
				"clr %B0"           CR_TAB
				"clr %A0", operands, plen, 7);
	  // Fallthrough

	case 28:
	case 29:
	  {
	    const bool ld_reg0_p = avr_ld_regno_p (reg0 + 3); // %D0
	    const bool ld_reg1_p = avr_ld_regno_p (reg1 + 0); // %A1
	    if (ld_reg0_p
		|| (ld_reg1_p && reg1_unused_after)
		|| scratch_p)
	      {
		if (ld_reg0_p)
		  avr_asm_len ("mov %D0,%A1"    CR_TAB
			       "swap %D0"       CR_TAB
			       "andi %D0,0xf0", operands, plen, 3);
		else if (ld_reg1_p && reg1_unused_after)
		  avr_asm_len ("swap %A1"       CR_TAB
			       "andi %A1,0xf0"  CR_TAB
			       "mov %D0,%A1", operands, plen, 3);
		else
		  avr_asm_len ("mov %D0,%A1"    CR_TAB
			       "swap %D0"       CR_TAB
			       "ldi %3,0xf0"    CR_TAB
			       "and %D0,%3", operands, plen, 4);
		for (int i = 28; i < off; ++i)
		  avr_asm_len ("lsl %D0", operands, plen, 1);
		return avr_asm_len ("clr %C0"  CR_TAB
				    "clr %B0"  CR_TAB
				    "clr %A0", operands, plen, 3);
	      }
	  }
	  // Fallthrough

	case 24:
	case 25:
	case 26:
	case 27:
	  avr_asm_len ("mov %D0,%A1", operands, plen, 1);
	  for (int i = 24; i < off; ++i)
	    avr_asm_len ("lsl %D0", operands, plen, 1);
	  return avr_asm_len ("clr %C0"      CR_TAB
			      "clr %B0"      CR_TAB
			      "clr %A0", operands, plen, 3);
	case 31:
	  return AVR_HAVE_MOVW
	    ? avr_asm_len ("bst %A1,0"    CR_TAB
			   "clr %A0"      CR_TAB
			   "clr %B0"      CR_TAB
			   "movw %C0,%A0" CR_TAB
			   "bld %D0,7", operands, plen, 5)
	    : avr_asm_len ("bst %A1,0" CR_TAB
			   "clr %A0"   CR_TAB
			   "clr %B0"   CR_TAB
			   "clr %D0"   CR_TAB
			   "clr %C0"   CR_TAB
			   "bld %D0,7", operands, plen, 6);
	}
    }

  out_shift_with_cnt ("lsl %A0" CR_TAB
		      "rol %B0" CR_TAB
		      "rol %C0" CR_TAB
		      "rol %D0", insn, operands, plen, 4);
  return "";
}


/* 8-bit arithmetic shift right  (int8_t) x >> i.  */

const char *
ashrqi3_out (rtx_insn *insn, rtx operands[], int *plen)
{
  if (CONST_INT_P (operands[2]))
    {
      if (plen)
	*plen = 0;

      const int offs = INTVAL (operands[2]);
      bool reg1_unused_after = reg_unused_after (insn, operands[1]);

      if (IN_RANGE (offs, 0, 5))
	{
	  for (int i = 0; i < offs; ++i)
	    avr_asm_len ("asr %0", operands, plen, 1);
	  return "";
	}
      else if (offs == 6)
	{
	  return reg1_unused_after
	    ? avr_asm_len ("bst %1,6"  CR_TAB
			   "lsl %1"    CR_TAB
			   "sbc %0,%0" CR_TAB
			   "bld %0,0", operands, plen, 4)
	    : avr_asm_len ("mov %0,%1" CR_TAB
			   "bst %0,6"  CR_TAB
			   "lsl %0"    CR_TAB
			   "sbc %0,%0" CR_TAB
			   "bld %0,0", operands, plen, 5);
	}
      else if (offs >= 7)
	{
	  rtx xop[2] = { operands[0], operands[1] };
	  if (! reg_unused_after (insn, xop[1]))
	    {
	      avr_asm_len ("mov %0,%1", xop, plen, 1);
	      xop[1] = xop[0];
	    }
	  return avr_asm_len ("lsl %1" CR_TAB
			      "sbc %0,%0", xop, plen, 2);
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt ("asr %0",
		      insn, operands, plen, 1);
  return "";
}


/* 16bit arithmetic shift right  ((signed short)x >> i) */

const char *
ashrhi3_out (rtx_insn *insn, rtx operands[], int *plen)
{
  if (CONST_INT_P (operands[2]))
    {
      bool scratch = (GET_CODE (PATTERN (insn)) == PARALLEL
		      && XVECLEN (PATTERN (insn), 0) == 3
		      && REG_P (operands[3]));
      bool ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      bool reg1_unused_after = reg_unused_after (insn, operands[1]);

      if (plen)
	*plen = 0;

      switch (INTVAL (operands[2]))
	{
	case 4:
	case 5:
	  /* XXX try to optimize this too? */
	  break;

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  return avr_asm_len ("mov __tmp_reg__,%A0" CR_TAB
			      "mov %A0,%B0"         CR_TAB
			      "lsl __tmp_reg__"     CR_TAB
			      "rol %A0"             CR_TAB
			      "sbc %B0,%B0"         CR_TAB
			      "lsl __tmp_reg__"     CR_TAB
			      "rol %A0"             CR_TAB
			      "rol %B0", operands, plen, 8);
	case 7:
	  return reg1_unused_after
	    ? avr_asm_len ("lsl %A1"     CR_TAB
			   "mov %A0,%B1" CR_TAB
			   "rol %A0"     CR_TAB
			   "sbc %B0,%B0", operands, plen, 4)
	    : avr_asm_len ("mov %A0,%A1"     CR_TAB
			   "lsl %A0"     CR_TAB
			   "mov %A0,%B1" CR_TAB
			   "rol %A0"     CR_TAB
			   "sbc %B0,%B0", operands, plen, 5);
	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    return reg0 == reg1
	      ? avr_asm_len ("mov %A0,%B0" CR_TAB
			     "lsl %B0"     CR_TAB
			     "sbc %B0,%B0", operands, plen, 3)
	      : avr_asm_len ("mov %A0,%B1" CR_TAB
			     "clr %B0"     CR_TAB
			     "sbrc %A0,7"  CR_TAB
			     "dec %B0", operands, plen, 4);
	  }

	case 9:
	  return avr_asm_len ("mov %A0,%B0" CR_TAB
			      "lsl %B0"      CR_TAB
			      "sbc %B0,%B0" CR_TAB
			      "asr %A0", operands, plen, 4);
	case 10:
	  return avr_asm_len ("mov %A0,%B0" CR_TAB
			      "lsl %B0"     CR_TAB
			      "sbc %B0,%B0" CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0", operands, plen, 5);
	case 11:
	  if (AVR_HAVE_MUL && ldi_ok)
	    return avr_asm_len ("ldi %A0,0x20" CR_TAB
				"muls %B0,%A0" CR_TAB
				"mov %A0,r1"   CR_TAB
				"sbc %B0,%B0"  CR_TAB
				"clr __zero_reg__", operands, plen, 5);
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  return avr_asm_len ("mov %A0,%B0" CR_TAB
			      "lsl %B0"     CR_TAB
			      "sbc %B0,%B0" CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0", operands, plen, 6);
	case 12:
	  if (AVR_HAVE_MUL && ldi_ok)
	    return avr_asm_len ("ldi %A0,0x10" CR_TAB
				"muls %B0,%A0" CR_TAB
				"mov %A0,r1"   CR_TAB
				"sbc %B0,%B0"  CR_TAB
				"clr __zero_reg__", operands, plen, 5);
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  return avr_asm_len ("mov %A0,%B0" CR_TAB
			      "lsl %B0"     CR_TAB
			      "sbc %B0,%B0" CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0", operands, plen, 7);
	case 13:
	  if (AVR_HAVE_MUL && ldi_ok)
	    return avr_asm_len ("ldi %A0,0x08" CR_TAB
				"muls %B0,%A0" CR_TAB
				"mov %A0,r1"   CR_TAB
				"sbc %B0,%B0"  CR_TAB
				"clr __zero_reg__", operands, plen, 5);
	  if (optimize_size)
	    break;  /* scratch ? 5 : 7 */
	  return avr_asm_len ("mov %A0,%B0" CR_TAB
			      "lsl %B0"     CR_TAB
			      "sbc %B0,%B0" CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0"     CR_TAB
			      "asr %A0", operands, plen, 8);
	case 14:
	  return reg1_unused_after
	    ? avr_asm_len ("bst %B1,6"   CR_TAB
			   "lsl %B1"     CR_TAB
			   "sbc %B0,%B0" CR_TAB
			   "sbc %A0,%A0" CR_TAB
			   "bld %A0,0", operands, plen, 5)
	    : avr_asm_len ("mov %B0,%B1"   CR_TAB
			   "bst %B0,6"   CR_TAB
			   "lsl %B0"     CR_TAB
			   "sbc %B0,%B0" CR_TAB
			   "sbc %A0,%A0" CR_TAB
			   "bld %A0,0", operands, plen, 6);
	default:
	  if (INTVAL (operands[2]) < 16)
	    break;

	  /* fall through */

	case 15:
	  {
	    rtx xop[2] = { operands[0], operands[1] };
	    if (! reg_unused_after (insn, xop[1]))
	      {
		avr_asm_len ("mov %B0,%B1", xop, plen, 1);
		xop[1] = xop[0];
	      }
	    return avr_asm_len ("lsl %B1"     CR_TAB
				"sbc %A0,%A0" CR_TAB
				"mov %B0,%A0", xop, plen, 3);
	  }
	} // switch
    }

  out_shift_with_cnt ("asr %B0" CR_TAB
		      "ror %A0", insn, operands, plen, 2);
  return "";
}


/* 24-bit arithmetic shift right */

const char *
avr_out_ashrpsi3 (rtx_insn *insn, rtx *op, int *plen)
{
  int dest = REGNO (op[0]);
  int src = REGNO (op[1]);

  if (CONST_INT_P (op[2]))
    {
      if (plen)
	*plen = 0;

      bool reg1_unused_after = reg_unused_after (insn, op[1]);

      switch (INTVAL (op[2]))
	{
	case 8:
	  return dest <= src
	    ? avr_asm_len ("mov %A0,%B1" CR_TAB
			   "mov %B0,%C1" CR_TAB
			   "clr %C0"     CR_TAB
			   "sbrc %B0,7"  CR_TAB
			   "dec %C0", op, plen, 5)
	    : avr_asm_len ("clr %C0"     CR_TAB
			   "sbrc %C1,7"  CR_TAB
			   "dec %C0"     CR_TAB
			   "mov %B0,%C1" CR_TAB
			   "mov %A0,%B1", op, plen, 5);
	case 15:
	  avr_asm_len (reg1_unused_after
		       ? "lsl %B1"
		       : "bst %B1,7", op, plen, 1);
	  if (dest != src + 2)
	    avr_asm_len ("mov %A0,%C1", op, plen, 1);
	  avr_asm_len ("rol %A0"      CR_TAB
		       "sbc %B0,%B0"  CR_TAB
		       "sbc %C0,%C0", op, plen, 3);
	  return reg1_unused_after
	    ? ""
	    : avr_asm_len ("bld %A0,0", op, plen, 1);

	case 16:
	  if (dest != src + 2)
	    avr_asm_len ("mov %A0,%C1", op, plen, 1);
	  return reg1_unused_after && dest != src + 2
	    ? avr_asm_len ("rol %C1"      CR_TAB
			   "sbc %B0,%B0"  CR_TAB
			   "sbc %C0,%C0", op, plen, 3)
	    : avr_asm_len ("clr %B0"     CR_TAB
			   "sbrc %A0,7"  CR_TAB
			   "com %B0"     CR_TAB
			   "mov %C0,%B0", op, plen, 4);
	case 22:
	  {
	    rtx xop[2] = { op[0], op[1] };
	    if (! reg1_unused_after)
	      {
		avr_asm_len ("mov %C0,%C1", xop, plen, 1);
		xop[1] = xop[0];
	      }
	    return avr_asm_len ("bst %C1,6"   CR_TAB
				"lsl %C1"     CR_TAB
				"sbc %C0,%C0" CR_TAB
				"sbc %B0,%B0" CR_TAB
				"sbc %A0,%A0" CR_TAB
				"bld %A0,0", xop, plen, 6);
	  }

	default:
	  if (INTVAL (op[2]) < 24)
	    break;

	  /* fall through */

	case 23:
	  {
	    rtx xop[2] = { op[0], op[1] };
	    if (! reg1_unused_after)
	      {
		avr_asm_len ("mov %C0,%C1", xop, plen, 1);
		xop[1] = xop[0];
	      }
	    return avr_asm_len ("lsl %C1"     CR_TAB
				"sbc %A0,%A0" CR_TAB
				"mov %B0,%A0" CR_TAB
				"mov %C0,%A0", xop, plen, 4);
	  }
	} /* switch */
    }

  out_shift_with_cnt ("asr %C0" CR_TAB
		      "ror %B0" CR_TAB
		      "ror %A0", insn, op, plen, 3);
  return "";
}


/* 32-bit arithmetic shift right  ((signed long)x >> i) */

const char *
ashrsi3_out (rtx_insn *insn, rtx operands[], int *plen)
{
  if (CONST_INT_P (operands[2]))
    {
      if (plen)
	*plen = 0;

      int reg0 = true_regnum (operands[0]);
      int reg1 = true_regnum (operands[1]);
      bool reg1_unused_after = reg_unused_after (insn, operands[1]);

      switch (INTVAL (operands[2]))
	{
	case 8:
	  return reg0 <= reg1
	    ? avr_asm_len ("mov %A0,%B1" CR_TAB
			   "mov %B0,%C1" CR_TAB
			   "mov %C0,%D1" CR_TAB
			   "clr %D0"     CR_TAB
			   "sbrc %C0,7"  CR_TAB
			   "dec %D0", operands, plen, 6)
	    : avr_asm_len ("clr %D0"     CR_TAB
			   "sbrc %D1,7"  CR_TAB
			   "dec %D0"     CR_TAB
			   "mov %C0,%D1" CR_TAB
			   "mov %B0,%C1" CR_TAB
			   "mov %A0,%B1", operands, plen, 6);
	case 15:
	  avr_asm_len (reg1_unused_after
		       ? "lsl %B1"
		       : "bst %B1,7", operands, plen, 1);
	  if (reg0 != reg1 + 2)
	    {
	      if (AVR_HAVE_MOVW)
		avr_asm_len ("movw %A0,%C1", operands, plen, 1);
	      else
		avr_asm_len ("mov %A0,%C1"  CR_TAB
			     "mov %B0,%D1", operands, plen, 2);
	    }
	  avr_asm_len ("rol %A0"     CR_TAB
		       "rol %B0"     CR_TAB
		       "sbc %C0,%C0" CR_TAB
		       "sbc %D0,%D0", operands, plen, 4);
	  return reg1_unused_after
	    ? ""
	    : avr_asm_len ("bld %A0,0", operands, plen, 1);

	case 16:
	  if (reg0 == reg1 + 2)
	    return avr_asm_len ("clr %D0"     CR_TAB
				"sbrc %B0,7"  CR_TAB
				"com %D0"     CR_TAB
				"mov %C0,%D0", operands, plen, 4);
	  if (AVR_HAVE_MOVW)
	    avr_asm_len ("movw %A0,%C1", operands, plen, 1);
	  else
	    avr_asm_len ("mov %B0,%D1" CR_TAB
			 "mov %A0,%C1", operands, plen, 2);
	  return reg1_unused_after
	    ? avr_asm_len ("lsl %D1"      CR_TAB
			   "sbc %D0,%D0"  CR_TAB
			   "mov %C0,%D0", operands, plen, 3)
	    : avr_asm_len ("clr %D0"     CR_TAB
			   "sbrc %B0,7"  CR_TAB
			   "com %D0"     CR_TAB
			   "mov %C0,%D0", operands, plen, 4);
	case 24:
	  return reg1_unused_after
	    ? avr_asm_len ("mov %A0,%D1" CR_TAB
			   "lsl %D1"     CR_TAB
			   "sbc %D0,%D0" CR_TAB
			   "mov %B0,%D0" CR_TAB
			   "mov %C0,%D0", operands, plen, 5)
	    : avr_asm_len ("mov %A0,%D1" CR_TAB
			   "clr %D0"     CR_TAB
			   "sbrc %A0,7"  CR_TAB
			   "com %D0"     CR_TAB
			   "mov %B0,%D0" CR_TAB
			   "mov %C0,%D0", operands, plen, 6);
	case 30:
	  {
	    rtx xop[2] = { operands[0], operands[1] };
	    if (! reg1_unused_after)
	      {
		avr_asm_len ("mov %D0,%D1", xop, plen, 1);
		xop[1] = xop[0];
	      }
	    avr_asm_len ("bst %D1,6" CR_TAB
			 "lsl %D1" CR_TAB
			 "sbc %A0,%A0" CR_TAB
			 "sbc %B0,%B0", xop, plen, 4);
	    return AVR_HAVE_MOVW
	      ? avr_asm_len ("movw %C0,%A0" CR_TAB
			     "bld %A0,0", xop, plen, 2)
	      : avr_asm_len ("mov %C0,%A0" CR_TAB
			     "mov %D0,%A0" CR_TAB
			     "bld %A0,0", xop, plen, 3);
	  }

	default:
	  if (INTVAL (operands[2]) < 32)
	    break;

	  /* fall through */

	case 31:
	  {
	    rtx xop[2] = { operands[0], operands[1] };
	    if (! reg1_unused_after)
	      {
		avr_asm_len ("mov %D0,%D1", xop, plen, 1);
		xop[1] = xop[0];
	      }
	    return AVR_HAVE_MOVW
	      ? avr_asm_len ("lsl %D1"     CR_TAB
			     "sbc %A0,%A0" CR_TAB
			     "mov %B0,%A0" CR_TAB
			     "movw %C0,%A0", xop, plen, 4)
	      : avr_asm_len ("lsl %D1"     CR_TAB
			     "sbc %A0,%A0" CR_TAB
			     "mov %B0,%A0" CR_TAB
			     "mov %C0,%A0" CR_TAB
			     "mov %D0,%A0", xop, plen, 5);
	  }
	} // switch
    }

  out_shift_with_cnt ("asr %D0" CR_TAB
		      "ror %C0" CR_TAB
		      "ror %B0" CR_TAB
		      "ror %A0", insn, operands, plen, 4);
  return "";
}

/* 8-bit logic shift right ((unsigned char)x >> i) */

const char *
lshrqi3_out (rtx_insn *insn, rtx operands[], int *plen)
{
  if (CONST_INT_P (operands[2]))
    {
      int reg0 = REGNO (operands[0]);
      int reg1 = REGNO (operands[1]);
      bool ldreg_p = test_hard_reg_class (LD_REGS, operands[0]);
      int offs = INTVAL (operands[2]);

      if (plen)
	*plen = 0;

      if (offs <= 3
	  || (offs <= 5 && ! ldreg_p))
	{
	  for (int i = 0; i < offs; ++i)
	    avr_asm_len ("lsr %0", operands, plen, 1);
	  return "";
	}

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 8)
	    break;
	  return avr_asm_len ("clr %0", operands, plen, 1);

	case 1:
	case 2:
	case 3:
	  gcc_unreachable ();

	case 4:
	  return avr_asm_len ("swap %0" CR_TAB
			      "andi %0,0x0f", operands, plen, 2);
	case 5:
	  return avr_asm_len ("swap %0" CR_TAB
			      "lsr %0"  CR_TAB
			      "andi %0,0x7", operands, plen, 3);
	case 6:
	  if (ldreg_p && reg0 == reg1)
	    return avr_asm_len ("swap %0" CR_TAB
				"lsr %0"  CR_TAB
				"lsr %0"  CR_TAB
				"andi %0,0x3", operands, plen, 4);
	  if (ldreg_p && reg0 != reg1 && AVR_HAVE_MUL)
	    return avr_asm_len ("ldi %0,1<<2" CR_TAB
				"mul %0,%1"   CR_TAB
				"mov %0,r1"   CR_TAB
				"clr __zero_reg__", operands, plen, 4);
	  return reg0 != reg1
	    ? avr_asm_len ("clr %0"    CR_TAB
			   "bst %1,6"  CR_TAB
			   "bld %0,0"  CR_TAB
			   "bst %1,7"  CR_TAB
			   "bld %0,1", operands, plen, 5)
	    : avr_asm_len ("lsr %0"  CR_TAB
			   "lsr %0"  CR_TAB
			   "lsr %0"  CR_TAB
			   "lsr %0"  CR_TAB
			   "lsr %0"  CR_TAB
			   "lsr %0", operands, plen, 6);
	case 7:
	  return avr_asm_len ("bst %1,7" CR_TAB
			      "clr %0"   CR_TAB
			      "bld %0,0", operands, plen, 3);
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt ("lsr %0",
		      insn, operands, plen, 1);
  return "";
}


/* 16-bit logic shift right ((unsigned short)x >> i) */

const char *
lshrhi3_out (rtx_insn *insn, rtx operands[], int *plen)
{
  if (CONST_INT_P (operands[2]))
    {
      bool scratch = (GET_CODE (PATTERN (insn)) == PARALLEL
		      && XVECLEN (PATTERN (insn), 0) == 3
		      && REG_P (operands[3]));
      bool ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      bool reg1_unused_after = reg_unused_after (insn, operands[1]);

      if (plen)
	*plen = 0;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 16)
	    break;
	  return avr_asm_len ("clr %B0" CR_TAB
			      "clr %A0", operands, plen, 2);

	case 4:
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    return avr_asm_len ("swap %B0"      CR_TAB
				"swap %A0"      CR_TAB
				"andi %A0,0x0f" CR_TAB
				"eor %A0,%B0"   CR_TAB
				"andi %B0,0x0f" CR_TAB
				"eor %A0,%B0", operands, plen, 6);
	  if (scratch)
	    return avr_asm_len ("swap %B0"    CR_TAB
				"swap %A0"    CR_TAB
				"ldi %3,0x0f" CR_TAB
				"and %A0,%3"  CR_TAB
				"eor %A0,%B0" CR_TAB
				"and %B0,%3"  CR_TAB
				"eor %A0,%B0", operands, plen, 7);
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    return avr_asm_len ("lsr %B0"       CR_TAB
				"ror %A0"       CR_TAB
				"swap %B0"      CR_TAB
				"swap %A0"      CR_TAB
				"andi %A0,0x0f" CR_TAB
				"eor %A0,%B0"   CR_TAB
				"andi %B0,0x0f" CR_TAB
				"eor %A0,%B0", operands, plen, 8);
	  if (scratch)
	    return avr_asm_len ("lsr %B0"     CR_TAB
				"ror %A0"     CR_TAB
				"swap %B0"    CR_TAB
				"swap %A0"    CR_TAB
				"ldi %3,0x0f" CR_TAB
				"and %A0,%3"  CR_TAB
				"eor %A0,%B0" CR_TAB
				"and %B0,%3"  CR_TAB
				"eor %A0,%B0", operands, plen, 9);
	  break;  /* 10 */

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  return avr_asm_len ("clr __tmp_reg__" CR_TAB
			      "lsl %A0"         CR_TAB
			      "rol %B0"         CR_TAB
			      "rol __tmp_reg__" CR_TAB
			      "lsl %A0"         CR_TAB
			      "rol %B0"         CR_TAB
			      "rol __tmp_reg__" CR_TAB
			      "mov %A0,%B0"     CR_TAB
			      "mov %B0,__tmp_reg__", operands, plen, 9);
	case 7:
	  return reg1_unused_after
	    ? avr_asm_len ("lsl %A1"     CR_TAB
			   "mov %A0,%B1" CR_TAB
			   "rol %A0"     CR_TAB
			   "sbc %B0,%B0" CR_TAB
			   "neg %B0", operands, plen, 5)
	    : avr_asm_len ("bst %A1,7"   CR_TAB
			   "mov %A0,%B1" CR_TAB
			   "rol %A0"     CR_TAB
			   "sbc %B0,%B0" CR_TAB
			   "neg %B0"     CR_TAB
			   "bld %A0,0", operands, plen, 6);
	case 8:
	  return avr_asm_len ("mov %A0,%B1" CR_TAB
			      "clr %B0", operands, plen, 2);
	case 9:
	  return avr_asm_len ("mov %A0,%B1" CR_TAB
			      "clr %B0"     CR_TAB
			      "lsr %A0", operands, plen, 3);
	case 10:
	  return avr_asm_len ("mov %A0,%B1" CR_TAB
			      "clr %B0"     CR_TAB
			      "lsr %A0"     CR_TAB
			      "lsr %A0", operands, plen, 4);
	case 11:
	  return avr_asm_len ("mov %A0,%B1" CR_TAB
			      "clr %B0"     CR_TAB
			      "lsr %A0"     CR_TAB
			      "lsr %A0"     CR_TAB
			      "lsr %A0", operands, plen, 5);
	case 12:
	  if (ldi_ok)
	    return avr_asm_len ("mov %A0,%B1" CR_TAB
				"clr %B0"     CR_TAB
				"swap %A0"    CR_TAB
				"andi %A0,0x0f", operands, plen, 4);
	  return scratch
	    ? avr_asm_len ("mov %A0,%B1" CR_TAB
			   "clr %B0"     CR_TAB
			   "swap %A0"    CR_TAB
			   "ldi %3,0x0f" CR_TAB
			   "and %A0,%3", operands, plen, 5)
	    : avr_asm_len ("mov %A0,%B1" CR_TAB
			   "clr %B0"     CR_TAB
			   "lsr %A0"     CR_TAB
			   "lsr %A0"     CR_TAB
			   "lsr %A0"     CR_TAB
			   "lsr %A0", operands, plen, 6);
	case 13:
	  if (ldi_ok)
	    return avr_asm_len ("mov %A0,%B0" CR_TAB
				"clr %B0"     CR_TAB
				"swap %A0"    CR_TAB
				"lsr %A0"     CR_TAB
				"andi %A0,0x07", operands, plen, 5);
	  if (AVR_HAVE_MUL && scratch)
	    return avr_asm_len ("ldi %3,0x08" CR_TAB
				"mul %B0,%3"  CR_TAB
				"mov %A0,r1"  CR_TAB
				"clr %B0"     CR_TAB
				"clr __zero_reg__", operands, plen, 5);
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (scratch)
	    return avr_asm_len ("mov %A0,%B0" CR_TAB
				"clr %B0"     CR_TAB
				"swap %A0"    CR_TAB
				"lsr %A0"     CR_TAB
				"ldi %3,0x07" CR_TAB
				"and %A0,%3", operands, plen, 6);
	  return AVR_HAVE_MUL
	    ? avr_asm_len ("set"        CR_TAB
			   "bld r1,3"   CR_TAB
			   "mul %B0,r1" CR_TAB
			   "mov %A0,r1" CR_TAB
			   "clr %B0"    CR_TAB
			   "clr __zero_reg__", operands, plen, 6)
	    : avr_asm_len ("mov %A0,%B0" CR_TAB
			   "clr %B0"     CR_TAB
			   "lsr %A0"     CR_TAB
			   "lsr %A0"     CR_TAB
			   "lsr %A0"     CR_TAB
			   "lsr %A0"     CR_TAB
			   "lsr %A0", operands, plen, 7);
	case 14:
	  if (AVR_HAVE_MUL && ldi_ok)
	    return avr_asm_len ("ldi %A0,0x04" CR_TAB
				"mul %B0,%A0"  CR_TAB
				"mov %A0,r1"   CR_TAB
				"clr %B0"      CR_TAB
				"clr __zero_reg__", operands, plen, 5);
	  if (AVR_HAVE_MUL && scratch)
	    return avr_asm_len ("ldi %3,0x04" CR_TAB
				"mul %B0,%3"  CR_TAB
				"mov %A0,r1"  CR_TAB
				"clr %B0"     CR_TAB
				"clr __zero_reg__", operands, plen, 5);
	  if (optimize_size && ldi_ok)
	    return avr_asm_len ("mov %A0,%B0" CR_TAB
				"ldi %B0,6" "\n1:\t"
				"lsr %A0"     CR_TAB
				"dec %B0"     CR_TAB
				"brne 1b", operands, plen, 5);
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  return avr_asm_len ("clr %A0" CR_TAB
			      "lsl %B0" CR_TAB
			      "rol %A0" CR_TAB
			      "lsl %B0" CR_TAB
			      "rol %A0" CR_TAB
			      "clr %B0", operands, plen, 6);
	case 15:
	  return avr_asm_len ("bst %B1,7" CR_TAB
			      "clr %A0"   CR_TAB
			      "clr %B0"   CR_TAB
			      "bld %A0,0", operands, plen, 4);
	}
    }

  out_shift_with_cnt ("lsr %B0" CR_TAB
		      "ror %A0", insn, operands, plen, 2);
  return "";
}


/* 24-bit logic shift right */

const char *
avr_out_lshrpsi3 (rtx_insn *insn, rtx *op, int *plen)
{
  int dest = REGNO (op[0]);
  int src = REGNO (op[1]);
  bool src_unused_after_p = reg_unused_after (insn, op[1]);

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
	case 15:
	  avr_asm_len (src_unused_after_p
		       ? "lsl %B1"
		       : "bst %B1,7", op, plen, 1);
	  if (dest != src + 2)
	    avr_asm_len ("mov %A0,%C1", op, plen, 1);
	  avr_asm_len ("clr %C0"  CR_TAB
		       "clr %B0"  CR_TAB
		       "rol %A0"  CR_TAB
		       "rol %B0", op, plen, 4);
	  return src_unused_after_p
	    ? ""
	    : avr_asm_len ("bld %A0,0", op, plen, 1);

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
	  return avr_asm_len ("bst %C1,7" CR_TAB
			      "clr %A0"   CR_TAB
			      "clr %B0"   CR_TAB
			      "clr %C0"   CR_TAB
			      "bld %A0,0", op, plen, 5);
	} /* switch */
    }

  out_shift_with_cnt ("lsr %C0" CR_TAB
		      "ror %B0" CR_TAB
		      "ror %A0", insn, op, plen, 3);
  return "";
}


/* 32-bit logic shift right ((unsigned int)x >> i) */

const char *
lshrsi3_out (rtx_insn *insn, rtx operands[], int *plen)
{
  if (CONST_INT_P (operands[2]))
    {
      int off = INTVAL (operands[2]);
      int reg0 = true_regnum (operands[0]);
      int reg1 = true_regnum (operands[1]);
      bool reg1_unused_after = reg_unused_after (insn, operands[1]);
      bool scratch_p = (GET_CODE (PATTERN (insn)) == PARALLEL
			&& XVECLEN (PATTERN (insn), 0) == 3
			&& REG_P (operands[3]));
      if (plen)
	*plen = 0;

      switch (off)
	{
	default:
	  if (off < 32)
	    break;

	  return AVR_HAVE_MOVW
	    ? avr_asm_len ("clr %D0" CR_TAB
			   "clr %C0" CR_TAB
			   "movw %A0,%C0", operands, plen, 3)
	    : avr_asm_len ("clr %D0" CR_TAB
			   "clr %C0" CR_TAB
			   "clr %B0" CR_TAB
			   "clr %A0", operands, plen, 4);
	case 8:
	  return reg0 <= reg1
	    ? avr_asm_len ("mov %A0,%B1" CR_TAB
			   "mov %B0,%C1" CR_TAB
			   "mov %C0,%D1" CR_TAB
			   "clr %D0", operands, plen, 4)
	    : avr_asm_len ("clr %D0"     CR_TAB
			   "mov %C0,%D1" CR_TAB
			   "mov %B0,%C1" CR_TAB
			   "mov %A0,%B1", operands, plen, 4);
	case 15:
	  avr_asm_len (reg1_unused_after
		       ? "lsl %B1"
		       : "bst %B1,7", operands, plen, 1);
	  if (reg0 != reg1 + 2)
	    {
	      if (AVR_HAVE_MOVW)
		avr_asm_len ("movw %A0,%C1", operands, plen, 1);
	      else
		avr_asm_len ("mov %A0,%C1"  CR_TAB
			     "mov %B0,%D1", operands, plen, 2);
	    }
	  avr_asm_len ("clr %D0"  CR_TAB
		       "clr %C0"  CR_TAB
		       "rol %A0"  CR_TAB
		       "rol %B0"  CR_TAB
		       "rol %C0", operands, plen, 5);
	  return reg1_unused_after
	    ? ""
	    : avr_asm_len ("bld %A0,0", operands, plen, 1);

	case 16:
	  if (reg0 == reg1 + 2)
	    return avr_asm_len ("clr %C0"  CR_TAB
				"clr %D0", operands, plen, 2);
	  return AVR_HAVE_MOVW
	    ? avr_asm_len ("movw %A0,%C1" CR_TAB
			   "clr %C0"      CR_TAB
			   "clr %D0", operands, plen, 3)
	    : avr_asm_len ("mov %B0,%D1" CR_TAB
			   "mov %A0,%C1" CR_TAB
			   "clr %C0"     CR_TAB
			   "clr %D0", operands, plen, 4);
	case 30:
	  if (AVR_HAVE_MUL && scratch_p)
	    return avr_asm_len ("ldi %3,1<<2"       CR_TAB
				"mul %3,%D1"        CR_TAB
				"mov %A0,r1"        CR_TAB
				"clr __zero_reg__"  CR_TAB
				"clr %B0"           CR_TAB
				"clr %C0"           CR_TAB
				"clr %D0", operands, plen, 7);
	  // Fallthrough

	case 29:
	case 28:
	  {
	    const bool ld_reg0_p = avr_ld_regno_p (reg0 + 0); // %A0
	    const bool ld_reg1_p = avr_ld_regno_p (reg1 + 3); // %D1
	    if (ld_reg0_p
		|| (ld_reg1_p && reg1_unused_after)
		|| scratch_p)
	      {
		if (ld_reg0_p)
		  avr_asm_len ("mov %A0,%D1"    CR_TAB
			       "swap %A0"       CR_TAB
			       "andi %A0,0x0f", operands, plen, 3);
		else if (ld_reg1_p && reg1_unused_after)
		  avr_asm_len ("swap %D1"       CR_TAB
			       "andi %D1,0x0f"  CR_TAB
			       "mov %A0,%D1", operands, plen, 3);
		else
		  avr_asm_len ("mov %A0,%D1"    CR_TAB
			       "swap %A0"       CR_TAB
			       "ldi %3,0x0f"    CR_TAB
			       "and %A0,%3", operands, plen, 4);
		for (int i = 28; i < off; ++i)
		  avr_asm_len ("lsr %A0", operands, plen, 1);
		return avr_asm_len ("clr %B0"  CR_TAB
				    "clr %C0"  CR_TAB
				    "clr %D0", operands, plen, 3);
	      }
	  }
	  // Fallthrough

	case 27:
	case 26:
	case 25:
	case 24:
	  avr_asm_len ("mov %A0,%D1", operands, plen, 1);
	  for (int i = 24; i < off; ++i)
	    avr_asm_len ("lsr %A0", operands, plen, 1);
	  return avr_asm_len ("clr %B0"     CR_TAB
			      "clr %C0"     CR_TAB
			      "clr %D0", operands, plen, 3);
	case 31:
	  return AVR_HAVE_MOVW
	    ? avr_asm_len ("bst %D1,7"    CR_TAB
			   "clr %A0"      CR_TAB
			   "clr %B0"      CR_TAB
			   "movw %C0,%A0" CR_TAB
			   "bld %A0,0", operands, plen, 5)
	    : avr_asm_len ("bst %D1,7" CR_TAB
			   "clr %A0"   CR_TAB
			   "clr %B0"   CR_TAB
			   "clr %C0"   CR_TAB
			   "clr %D0"   CR_TAB
			   "bld %A0,0", operands, plen, 6);
	} // switch
    }

  out_shift_with_cnt ("lsr %D0" CR_TAB
		      "ror %C0" CR_TAB
		      "ror %B0" CR_TAB
		      "ror %A0", insn, operands, plen, 4);
  return "";
}


/* When INSN is a PARALLEL with two SETs, a SET of REG_CC and a SET of a
   GPR, then return the second SET and set *CCMODE to the first SET's mode.
   Otherwise, return single_set and set *CCMODE to VOIDmode.  */

static rtx
avr_cc_set (rtx_insn *insn, machine_mode *ccmode)
{
  // single_set() not only depends on the anatomy of an insn but also
  // on REG_UNUSED notes, thus we have to analyze by hand so that the
  // result only depends on the pattern.

  rtx pat = PATTERN (insn);

  if (GET_CODE (pat) == PARALLEL
      && XVECLEN (pat, 0) == 2
      && GET_CODE (XVECEXP (pat, 0, 0)) == SET
      && GET_CODE (XVECEXP (pat, 0, 1)) == SET)
    {
      rtx ccset = XVECEXP (pat, 0, 0);
      *ccmode = GET_MODE (SET_DEST (ccset));
      return XVECEXP (pat, 0, 1);
    }

  *ccmode = VOIDmode;
  return single_set (insn);
}


/* Output addition of registers YOP[0] and YOP[1]

      YOP[0] += extend (YOP[1])

   or subtraction of registers YOP[0] and YOP[2]

      YOP[0] -= extend (YOP[2])

   where the integer modes satisfy  SI >= YOP[0].mode >= YOP[1/2].mode >= QI,
   and the extension may be sign-extend, zero-extend or reg (no extend).
   INSN is either a single_set or a true parallel insn.  In the latter case,
   INSN has two SETs: A SET of REG_CC and a SET like in the single_set case.
   Returns "".

   If PLEN == NULL output the instructions.
   If PLEN != NULL set *PLEN to the length of the sequence in words.  */

const char *
avr_out_plus_ext (rtx_insn *insn, rtx *yop, int *plen)
{
  rtx regs[2];
  machine_mode ccmode;

  /* Ouch! Whether or not an insn is a single_set does not only depend
     on the anatomy of the pattern, but also on REG_UNUSED notes.
     Hence we have to dig by hand... */

  const rtx src = SET_SRC (avr_cc_set (insn, &ccmode));
  const rtx_code add = GET_CODE (src);
  gcc_assert (GET_CODE (src) == PLUS || GET_CODE (src) == MINUS);

  // Use XOP[] in the remainder with XOP[0] = YOP[0] and XOP[1] = YOP[1/2].
  rtx xop[2] = { yop[0], yop[add == PLUS ? 1 : 2] };
  const rtx xreg = XEXP (src, add == PLUS ? 1 : 0);
  const rtx xext = XEXP (src, add == PLUS ? 0 : 1);
  const rtx_code ext = GET_CODE (xext);

  gcc_assert (REG_P (xreg)
	      && (ext == ZERO_EXTEND || ext == SIGN_EXTEND || ext == REG));

  const int n_bytes0 = GET_MODE_SIZE (GET_MODE (xop[0]));
  const int n_bytes1 = GET_MODE_SIZE (GET_MODE (xop[1]));
  rtx msb1 = all_regs_rtx[n_bytes1 - 1 + REGNO (xop[1])];

  const char *const s_ADD = add == PLUS ? "add %0,%1" : "sub %0,%1";
  const char *const s_ADC = add == PLUS ? "adc %0,%1" : "sbc %0,%1";
  const char *const s_DEC = add == PLUS
    ? "adc %0,__zero_reg__"  CR_TAB  "sbrc %1,7"  CR_TAB  "dec %0"
    : "sbc %0,__zero_reg__"  CR_TAB  "sbrc %1,7"  CR_TAB  "inc %0";

  // A register that containts 8 copies of $1.msb.
  rtx ext_reg = ext == ZERO_EXTEND ? zero_reg_rtx : NULL_RTX;

  if (plen)
    *plen = 0;

  if (ext == SIGN_EXTEND
      && (n_bytes0 > 1 + n_bytes1
	  || reg_overlap_mentioned_p (msb1, xop[0])
	  // The insn also wants to set SREG.N and SREG.Z.
	  || ccmode == CCZNmode))
    {
      // Sign-extending more than one byte: Set tmp_reg to 0 or -1
      // depending on $1.msb. Same for the pathological case where
      // $0 and $1 overlap.

      regs[0] = ext_reg = tmp_reg_rtx;
      regs[1] = msb1;

      avr_asm_len ("mov %0,%1"  CR_TAB
		   "lsl %0"     CR_TAB
		   "sbc %0,%0", regs, plen, 3);
    }

  // Adding the bytes of $1 is just plain additions / subtractions.
  // Same for the extended bytes when we have ext_reg.

  avr_asm_len (s_ADD, xop, plen, 1);

  for (int i = 1; i < n_bytes0; ++i)
    {
      regs[0] = all_regs_rtx[i + REGNO (xop[0])];
      regs[1] = i < n_bytes1 ? all_regs_rtx[i + REGNO (xop[1])] : ext_reg;

      if (! regs[1])
	{
	  // Extending just 1 byte:  This is one instruction shorter
	  // than sign-extending $1.msb to tmp_reg.

	  regs[1] = msb1;
	  avr_asm_len (s_DEC, regs, plen, 3);
	}
      else
	avr_asm_len (s_ADC, regs, plen, 1);
    }

  return "";
}


/* Output code for addition of a sign-bit

      YOP[0] += YOP[1] <CMP> 0

   or such a subtraction:

      YOP[0] -= YOP[2] <CMP> 0

   where CMP is in { GE, LT }.
   If PLEN == NULL output the instructions.
   If PLEN != NULL set *PLEN to the length of the sequence in words.  */

const char *
avr_out_add_msb (rtx_insn *insn, rtx *yop, rtx_code cmp, int *plen)
{
  const rtx_code add = GET_CODE (SET_SRC (single_set (insn)));
  const machine_mode mode = GET_MODE (yop[0]);
  const int n_bytes = GET_MODE_SIZE (mode);
  rtx sigop = yop[add == PLUS ? 1 : 2];
  rtx msb = avr_byte (sigop, GET_MODE_SIZE (GET_MODE (sigop)) - 1);
  rtx op[3] = { yop[0], msb, nullptr };

  if (plen)
    *plen = 0;

  if (n_bytes == 1
      || (n_bytes == 2 && avr_adiw_reg_p (op[0])))
    {
      avr_asm_len (cmp == LT
		   ? "sbrc %1,7"
		   : "sbrs %1,7", op, plen, 1);
      const char *s_add = add == PLUS
	? n_bytes == 1 ? "inc %0" : "adiw %0,1"
	: n_bytes == 1 ? "dec %0" : "sbiw %0,1";
      return avr_asm_len (s_add, op, plen, 1);
    }

  bool labl_p = false;
  const char *s_code0 = nullptr;

  // Default code provided SREG.C = MSBit.
  const char *s_code = add == PLUS
    ? "adc %2,__zero_reg__"
    : "sbc %2,__zero_reg__";

  if (cmp == LT)
    {
      if (reg_unused_after (insn, sigop)
	  && ! reg_overlap_mentioned_p (msb, op[0]))
	avr_asm_len ("lsl %1", op, plen, 1);
      else
	avr_asm_len ("mov __tmp_reg__,%1" CR_TAB
		     "lsl __tmp_reg__", op, plen, 2);
    }
  else if (test_hard_reg_class (LD_REGS, msb))
    {
      avr_asm_len ("cpi %1,0x80", op, plen, 1);
    }
  else if (test_hard_reg_class (LD_REGS, op[0]))
    {
      labl_p = true;
      avr_asm_len ("tst %1" CR_TAB
		   "brmi 0f", op, plen, 2);
      s_code0 = add == PLUS ? "subi %2,-1" : "subi %2,1";
      s_code  = add == PLUS ? "sbci %2,-1" : "sbci %2,0";
    }
  else
    {
      labl_p = true;
      avr_asm_len ("tst %1"  CR_TAB
		   "brmi 0f" CR_TAB
		   "sec", op, plen, 3);
    }

  for (int i = 0; i < n_bytes; ++i)
    {
      op[2] = avr_byte (op[0], i);
      avr_asm_len (i == 0 && s_code0
		   ? s_code0
		   : s_code, op, plen, 1);
    }

  return labl_p
    ? avr_asm_len ("0:", op, plen, 0)
    : "";
}


/* Output addition of register XOP[0] and compile time constant XOP[2].
   XINSN is a single_set insn or an insn pattern.
   CODE == PLUS:  perform addition by using ADD instructions or
   CODE == MINUS: perform addition by using SUB instructions:

      XOP[0] = XOP[0] + XOP[2]

   Or perform addition/subtraction with register XOP[2] depending on CODE:

      XOP[0] = XOP[0] +/- XOP[2]

   If PLEN == NULL, print assembler instructions to perform the operation;
   otherwise, set *PLEN to the length of the instruction sequence (in words)
   printed with PLEN == NULL.  XOP[3] is an 8-bit scratch register or NULL_RTX.

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
avr_out_plus_1 (rtx xinsn, rtx *xop, int *plen, rtx_code code,
		rtx_code code_sat, int sign, bool out_label)
{
  rtx_insn *insn = xinsn && INSN_P (xinsn)
    ? as_a <rtx_insn *> (xinsn)
    : nullptr;

  /* MODE of the operation.  */
  machine_mode mode = GET_MODE (xop[0]);

  /* INT_MODE of the same size.  */
  scalar_int_mode imode = int_mode_for_mode (mode).require ();

  /* Number of bytes to operate on.  */
  int n_bytes = GET_MODE_SIZE (mode);

  int regno0 = REGNO (xop[0]);
  if (optimize && code_sat == UNKNOWN)
    while (n_bytes && avr_result_regno_unused_p (insn, regno0 + n_bytes - 1))
      n_bytes -= 1;

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
      if (optimize
	  && REGNO (xop[0]) != REGNO (xop[2])
	  && reg_overlap_mentioned_p (xop[0], xop[2]))
	{
	  /* PR118878: Paradoxical SUBREGs may result in overlapping
	     registers.  The assumption is that the overlapping part
	     is unused garbage.  */
	  gcc_assert (n_bytes <= 4);
	  int delta = (int) REGNO (xop[0]) - (int) REGNO (xop[2]);
	  n_bytes = std::min (n_bytes, std::abs (delta));
	}

      for (int i = 0; i < n_bytes; i++)
	{
	  /* We operate byte-wise on the destination.  */
	  op[0] = avr_byte (xop[0], i);
	  op[1] = avr_byte (xop[2], i);

	  if (i == 0)
	    avr_asm_len (code == PLUS ? "add %0,%1" : "sub %0,%1",
			 op, plen, 1);
	  else
	    avr_asm_len (code == PLUS ? "adc %0,%1" : "sbc %0,%1",
			 op, plen, 1);
	}

      if (MINUS == code
	  && REGNO (xop[0]) == REGNO (xop[2]))
	return;

      goto saturate;
    }

  if (CONST_FIXED_P (xval))
    xval = avr_to_int_mode (xval);

  /* Adding/Subtracting zero is a no-op.  */

  if (xval == const0_rtx)
    return;

  if (MINUS == code)
    xval = simplify_unary_operation (NEG, imode, xval, imode);

  op[2] = xop[3];

  if (SS_PLUS == code_sat && MINUS == code
      && sign < 0
      && 0x80 == avr_uint8 (xval, n_bytes - 1))
    {
      /* We compute x + 0x80 by means of SUB instructions.  We negated the
	 constant subtrahend above and are left with  x - (-128)  so that we
	 need something like SUBI r,128 which does not exist because SUBI sets
	 V according to the sign of the subtrahend.  Notice the only case
	 where this must be done is when NEG overflowed in case [2s] because
	 the V computation needs the right sign of the subtrahend.  */

      rtx msb = avr_byte (xop[0], n_bytes - 1);

      avr_asm_len ("subi %0,128" CR_TAB
		   "brmi 0f", &msb, plen, 2);
      out_brvc = false;

      goto saturate;
    }

  for (int i = 0; i < n_bytes; i++)
    {
      /* We operate byte-wise on the destination.  */
      rtx reg8 = avr_byte (xop[0], i);
      rtx xval8 = avr_byte (xval, i);

      /* 8-bit value to operate with this byte. */
      unsigned int val8 = UINTVAL (xval8) & GET_MODE_MASK (QImode);

      /* Registers R16..R31 can operate with immediate.  */
      bool ld_reg_p = test_hard_reg_class (LD_REGS, reg8);

      op[0] = reg8;
      op[1] = gen_int_mode (val8, QImode);

      /* To get usable cc0 no low-bytes must have been skipped.  */

      if (!started
	  && i % 2 == 0
	  && i + 2 <= n_bytes
	  && avr_adiw_reg_p (reg8))
	{
	  unsigned int val16 = avr_uint16 (xval, i);

	  /* Registers R24, X, Y, Z can use ADIW/SBIW with constants < 64
	     i.e. operate word-wise.  */

	  if (val16 < 64)
	    {
	      if (val16 != 0)
		{
		  started = true;
		  avr_asm_len (code == PLUS ? "adiw %0,%1" : "sbiw %0,%1",
			       op, plen, 1);
		}

	      i++;
	      continue;
	    }
	}

      if (AVR_TINY
	  && optimize
	  && i == 0
	  && n_bytes == 2
	  // When that pass adjusts the frame pointer, then we know that
	  // reg Y points to ordinary memory, and the only side-effect
	  // of -Y and Y+ is the side effect on Y.
	  && avropt_fuse_add >= 2
	  && frame_pointer_needed
	  && REGNO (xop[0]) == FRAME_POINTER_REGNUM)
	{
	  if (insn
	      && _reg_unused_after (insn, xop[0], false))
	    return;

	  if (AVR_HAVE_8BIT_SP)
	    {
	      avr_asm_len ("subi %A0,%n2", xop, plen, 1);
	      return;
	    }
	  else if (xop[2] == const1_rtx || xop[2] == constm1_rtx)
	    {
	      avr_asm_len (xop[2] == const1_rtx
			   ? "ld __tmp_reg__,%a0+"
			   : "ld __tmp_reg__,-%a0", xop, plen, 1);
	      return;
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
	  break;
	}

      switch (code)
	{
	case PLUS:

	  gcc_assert (plen != nullptr || (op[2] && REG_P (op[2])));

	  if (plen != nullptr && UNKNOWN != code_sat)
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
	      gcc_assert (plen != nullptr || REG_P (op[2]));

	      if (clobber_val != (int) val8)
		avr_asm_len ("ldi %2,%1", op, plen, 1);
	      clobber_val = (int) val8;

	      avr_asm_len (started ? "sbc %0,%2" : "sub %0,%2", op, plen, 1);
	    }

	  break; /* MINUS */

	default:
	  /* Unknown code */
	  gcc_unreachable ();
	}

      started = true;

    } /* for all sub-bytes */

 saturate:

  if (UNKNOWN == code_sat)
    return;

  /* Vanilla addition/subtraction is done.  We are left with saturation.

     We have to compute  A = A <op> B  where  A  is a register and
     B is a register or a non-zero compile time constant CONST.
     A is register class "r" if unsigned && B is REG.  Otherwise, A is in "d".
     B stands for the original operand $2 in XINSN.  In the case of B = CONST,
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

     The cases a - b actually perform  a - (-(-b))  if B is CONST.  */

  op[0] = avr_byte (xop[0], n_bytes - 1);
  op[1] = n_bytes > 1 ? avr_byte (xop[0], n_bytes - 2) : NULL_RTX;

  bool need_copy = true;
  int len_call = 1 + AVR_HAVE_JMP_CALL;

  switch (code_sat)
    {
    default:
      gcc_unreachable ();

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

	  op[2] = avr_byte (xop[2], n_bytes - 1);

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

	  op[2] = avr_byte (xop[2], n_bytes - 1);

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
	gcc_unreachable ();

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
   is not a compile-time constant:

      XOP[0] = XOP[0] +/- XOP[2]

   This is a helper for the function below.  The only insns that need this
   are additions/subtraction for pointer modes, i.e. HImode and PSImode.  */

static const char *
avr_out_plus_symbol (rtx *xop, rtx_code code, int *plen)
{
  machine_mode mode = GET_MODE (xop[0]);

  /* Only pointer modes want to add symbols.  */

  gcc_assert (mode == HImode || mode == PSImode);

  if (mode == HImode
      && const_0mod256_operand (xop[2], HImode))
    return avr_asm_len (PLUS == code
			? "subi %B0,hi8(-(%2))"
			: "subi %B0,hi8(%2)", xop, plen, -1);

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

   PLEN defaults to NULL.

   OUT_LABEL defaults to TRUE.  For a description, see AVR_OUT_PLUS_1.

   Return ""  */

const char *
avr_out_plus (rtx insn, rtx *xop, int *plen, bool out_label)
{
  int len_plus, len_minus;
  rtx op[4];
  rtx xpattern = INSN_P (insn) ? single_set (as_a <rtx_insn *> (insn)) : insn;
  rtx xdest = SET_DEST (xpattern);
  machine_mode mode = GET_MODE (xdest);
  scalar_int_mode imode = int_mode_for_mode (mode).require ();
  int n_bytes = GET_MODE_SIZE (mode);
  rtx_code code_sat = GET_CODE (SET_SRC (xpattern));
  rtx_code code
    = (PLUS == code_sat || SS_PLUS == code_sat || US_PLUS == code_sat
       ? PLUS : MINUS);

  /* PLUS and MINUS don't saturate:  Use modular wrap-around.  */

  if (PLUS == code_sat || MINUS == code_sat)
    code_sat = UNKNOWN;

  if (n_bytes <= 4 && REG_P (xop[2]))
    {
      avr_out_plus_1 (insn, xop, plen, code, code_sat, 0, out_label);
      return "";
    }

  if (n_bytes == 8)
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
	  return avr_out_plus_symbol (xop, code, plen);
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

  int sign = avr_int8 (op[2], n_bytes - 1) < 0 ? -1 : 1;

  /* If we subtract and the subtrahend is a constant, then negate it
     so that avr_out_plus_1 can be used.  */

  if (MINUS == code)
    op[2] = simplify_unary_operation (NEG, imode, op[2], imode);

  /* Work out the shortest sequence.  */

  avr_out_plus_1 (insn, op, &len_minus, MINUS, code_sat, sign, out_label);
  avr_out_plus_1 (insn, op, &len_plus, PLUS, code_sat, sign, out_label);

  if (plen)
    *plen = (len_minus <= len_plus) ? len_minus : len_plus;
  else if (len_minus <= len_plus)
    avr_out_plus_1 (insn, op, nullptr, MINUS, code_sat, sign, out_label);
  else
    avr_out_plus_1 (insn, op, nullptr, PLUS, code_sat, sign, out_label);

  return "";
}


/* Output an addition with a compile-time constant that sets SREG.N:

      XOP[0] += XOP[1]

   where XOP[0] is a HI, PSI or SI register, and XOP[1] is a register or a
   compile-time constant.  XOP[2] is SCRATCH or a QI clobber reg.  Return "".

   If PLEN == NULL output the instructions.
   If PLEN != NULL set *PLEN to the length of the sequence in words.  */

const char *
avr_out_plus_set_N (rtx *xop, int *plen)
{
  gcc_assert (xop[1] != const0_rtx);

  // The output function for vanilla additions, avr_out_plus_1, can be
  // used because it always issues an operation on the MSB (except when
  // the addend is zero).

  rtx op[] = { xop[0], xop[0], xop[1], xop[2] };

  if (REG_P (xop[1]))
    {
      avr_out_plus_1 (NULL_RTX, op, plen, PLUS, UNKNOWN, 0, false);
    }
  else
    {
      int len_plus, len_minus;

      avr_out_plus_1 (NULL_RTX, op, &len_plus,  PLUS,  UNKNOWN, 0, false);
      avr_out_plus_1 (NULL_RTX, op, &len_minus, MINUS, UNKNOWN, 0, false);

      avr_out_plus_1 (NULL_RTX, op, plen, len_minus < len_plus ? MINUS : PLUS,
		      UNKNOWN, 0, false);
    }

  return "";
}


/* Output an instruction sequence for addition of REG in XOP[0] and CONST_INT
   in XOP[1] in such a way that SREG.Z and SREG.N are set according to the
   result.  The mode is HI, PSI or SI.  XOP[2] might be a d-regs clobber
   register.  If XOP[2] is SCRATCH, then the addition can be performed
   without a clobber reg.  Return "".

   If PLEN == NULL, then output the instructions.
   If PLEN != NULL, then set *PLEN to the length of the sequence in words. */

const char *
avr_out_plus_set_ZN (rtx *xop, int *plen)
{
  if (plen)
    *plen = 0;

  // Register to compare and value to compare against.
  rtx xreg = xop[0];
  rtx xval = xop[1];

  machine_mode mode = GET_MODE (xreg);

  // Number of bytes to operate on.
  int n_bytes = GET_MODE_SIZE (mode);

  if (n_bytes == 2
      && avr_adiw_reg_p (xreg)
      && IN_RANGE (INTVAL (xval), 1, 63))
    {
      // Add 16-bit value in [1..63] to a w register.
      return avr_asm_len ("adiw %0,%1", xop, plen, 1);
    }

  // Addition won't work; subtract the negative of XVAL instead.
  xval = simplify_unary_operation (NEG, mode, xval, mode);

  // Value (0..0xff) held in clobber register xop[2] or -1 if unknown.
  int clobber_val = -1;

  // [0] = Current sub-register.
  // [1] = Current partial xval.
  // [2] = 8-bit clobber d-register or SCRATCH.
  rtx op[3];
  op[2] = xop[2];

  // Work byte-wise from LSB to MSB.  The lower two bytes might be
  // SBIW'ed in one go.
  for (int i = 0; i < n_bytes; ++i)
    {
      op[0] = avr_byte (xreg, i);

      if (i == 0
	  && n_bytes >= 2
	  && avr_adiw_reg_p (op[0]))
	{
	  op[1] = avr_word (xval, 0);
	  if (IN_RANGE (INTVAL (op[1]), 0, 63))
	    {
	      // SBIW can handle the lower 16 bits.
	      avr_asm_len ("sbiw %0,%1", op, plen, 1);

	      // Next byte has already been handled: Skip it.
	      ++i;
	      continue;
	    }
	}

      op[1] = avr_byte (xval, i);

      if (test_hard_reg_class (LD_REGS, op[0]))
	{
	  // d-regs can subtract immediates.
	  avr_asm_len (i == 0
		       ? "subi %0,%1"
		       : "sbci %0,%1", op, plen, 1);
	}
      else
	{
	  int val8 = 0xff & INTVAL (op[1]);
	  if (val8 == 0)
	    {
	      // Any register can subtract 0.
	      avr_asm_len (i == 0
			   ? "sub %0,__zero_reg__"
			   : "sbc %0,__zero_reg__", op, plen, 1);
	    }
	  else
	    {
	      // Use d-register to hold partial xval.

	      if (val8 != clobber_val)
		{
		  // Load partial xval to QI clobber reg and memoize for later.
		  gcc_assert (REG_P (op[2]));
		  avr_asm_len ("ldi %2,%1", op, plen, 1);
		  clobber_val = val8;
		}

	      avr_asm_len (i == 0
			   ? "sub %0,%2"
			   : "sbc %0,%2", op, plen, 1);
	    }
	}
    } // Loop bytes.

  return "";
}


/* A helper worker for `op8_ZN_operator'.  Allow

     OP0 <code> OP1

  QImode operations that set SREG.N and SREG.Z in a usable way.
  these are:

  * OP0 is a QImode register, and
  * OP1 is a QImode register or CONST_INT, and

  the allowed operations is one of:

  * SHIFTs with a const_int offset in { 1, 2, 3 }.
  * MINUS and XOR with a register operand
  * IOR and AND with a register operand, or d-reg + const_int
  * PLUS with a register operand, or d-reg + const_int,
    or a const_int in { -2, -1, 1, 2 }.  */

bool
avr_op8_ZN_operator (rtx op)
{
  const rtx_code code = GET_CODE (op);
  rtx op0 = XEXP (op, 0);
  rtx op1 = XEXP (op, 1);

  if (! register_operand (op0, QImode)
      || ! (register_operand (op1, QImode)
	    || const_int_operand (op1, QImode)))
    return false;

  const bool reg1_p = REG_P (op1);
  const bool ld_reg0_p = test_hard_reg_class (LD_REGS, op0);

  switch (code)
    {
    default:
      break;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      return const_1_to_3_operand (op1, QImode);

    case MINUS:
    case XOR:
      return reg1_p;

    case IOR:
    case AND:
      return reg1_p || ld_reg0_p;

    case PLUS:
      return reg1_p || ld_reg0_p || abs1_abs2_operand (op1, QImode);
    }

  return false;
}


/* Output a QImode instruction sequence for

      XOP[0] = XOP[0]  <CODE>  XOP[2]

   where XOP[0] is a register, and the possible operands and CODEs
   are according to  `avr_op8_ZN_operator'  from above.  Return "".

   If PLEN == NULL, then output the instructions.
   If PLEN != NULL, then set *PLEN to the length of the sequence in words.  */

const char *
avr_out_op8_set_ZN (rtx_code code, rtx *xop, int *plen)
{
  const bool reg2_p = REG_P (xop[2]);
  const int ival = CONST_INT_P (xop[2]) ? (int) INTVAL (xop[2]) : 0;

  gcc_assert (op8_ZN_operator (gen_rtx_fmt_ee (code, QImode, xop[0], xop[2]),
			       QImode));
  if (plen)
    *plen = 0;

  const char *tpl = nullptr;
  int times = 1;

  if (code == ASHIFT)
    tpl = "lsl %0", times = ival;
  else if (code == LSHIFTRT)
    tpl = "lsr %0", times = ival;
  else if (code == ASHIFTRT)
    tpl = "asr %0", times = ival;
  else if (code == MINUS)
    tpl = "sub %0,%2";
  else if (code == XOR)
    tpl = "eor %0,%2";
  else if (code == AND)
    tpl = reg2_p ? "and %0,%2" : "andi %0,lo8(%2)";
  else if (code == IOR)
    tpl = reg2_p ? "or %0,%2" : "ori %0,lo8(%2)";
  else if (code == PLUS)
    {
      if (ival
	  && ! test_hard_reg_class (LD_REGS, xop[0]))
	{
	  tpl = ival > 0 ? "inc %0" : "dec %0";
	  times = std::abs (ival);
	}
      else
	tpl = reg2_p ? "add %0,%2" : "subi %0,lo8(%n2)";
    }
  else
    gcc_unreachable ();

  for (int i = 0; i < times; ++i)
    avr_asm_len (tpl, xop, plen, 1);

  return "";
}


/* Used in the "length" attribute of insn "*op8.for.cczn.<code>".  */

int
avr_len_op8_set_ZN (rtx_code code, rtx *xop)
{
  int len;
  (void) avr_out_op8_set_ZN (code, xop, &len);

  return len;
}


/* Output bit operation (IOR, AND, XOR) with register XOP[0] and compile
   time constant XOP[2]:

      XOP[0] = XOP[0] <op> XOP[2]

   and return "".  If PLEN == NULL, print assembler instructions to perform the
   operation; otherwise, set *PLEN to the length of the instruction sequence
   (in words) printed with PLEN == NULL.  XOP[3] is either an 8-bit clobber
   register or SCRATCH if no clobber register is needed for the operation.
   XINSN is an INSN_P or a pattern of an insn.  */

const char *
avr_out_bitop (rtx xinsn, rtx *xop, int *plen)
{
  rtx_insn *insn = xinsn && INSN_P (xinsn)
    ? as_a <rtx_insn *> (xinsn)
    : nullptr;
  rtx xpattern = insn ? single_set (insn) : xinsn;

  /* CODE and MODE of the operation.  */
  rtx_code code = GET_CODE (SET_SRC (xpattern));
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
      rtx reg8 = avr_byte (xop[0], i);

      if (optimize
	  && avr_result_regno_unused_p (insn, REGNO (reg8)))
	continue;

      /* 8-bit value to operate with this byte. */
      unsigned int val8 = avr_uint8 (xop[2], i);

      /* Number of bits set in the current byte of the constant.  */
      int pop8 = popcount_hwi (val8);

      /* Registers R16..R31 can operate with immediate.  */
      bool ld_reg_p = test_hard_reg_class (LD_REGS, reg8);

      op[0] = reg8;
      op[1] = GEN_INT (val8);

      switch (code)
	{
	case IOR:

	  if (pop8 == 0)
	    continue;
	  else if (ld_reg_p)
	    avr_asm_len ("ori %0,%1", op, plen, 1);
	  else if (pop8 == 1)
	    {
	      if (set_t != 1)
		avr_asm_len ("set", op, plen, 1);
	      set_t = 1;

	      op[1] = GEN_INT (exact_log2 (val8));
	      avr_asm_len ("bld %0,%1", op, plen, 1);
	    }
	  else if (pop8 == 8)
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

	  if (pop8 == 8)
	    continue;
	  else if (pop8 == 0)
	    avr_asm_len ("clr %0", op, plen, 1);
	  else if (ld_reg_p)
	    avr_asm_len ("andi %0,%1", op, plen, 1);
	  else if (pop8 == 7)
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

	  if (pop8 == 0)
	    continue;
	  else if (pop8 == 8)
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
	  gcc_unreachable ();
	}
    } /* for all sub-bytes */

  return "";
}


/* Emit code for

       XOP[0] = XOP[0] <xior> (XOP[1] <shift> BITOFF)

   where XOP[0] and XOP[1] are hard registers with integer mode,
   <xior> is XOR or IOR, and <shift> is LSHIFTRT or ASHIFT with a
   non-negative shift offset BITOFF.  This function emits the operation
   in terms of byte-wise operations in QImode.  */

void
avr_emit_xior_with_shift (rtx_insn *insn, rtx *xop, int bitoff)
{
  rtx src = SET_SRC (single_set (insn));
  rtx_code xior = GET_CODE (src);
  gcc_assert (xior == XOR || xior == IOR);
  gcc_assert (bitoff % 8 == 0);

  // Work out the shift offset in bytes; negative for shift right.
  rtx_code shift = GET_CODE (XEXP (src, 0));
  int byteoff = 0?0
    : shift == ASHIFT ? bitoff / 8
    : shift == LSHIFTRT ? -bitoff / 8
    // Not a shift but something like REG or ZERO_EXTEND:
    // Use xop[1] as is, without shifting it.
    : 0;

  // Work out which hard REGNOs belong to the operands.
  int size0 = GET_MODE_SIZE (GET_MODE (xop[0]));
  int size1 = GET_MODE_SIZE (GET_MODE (xop[1]));
  int regno0_lo = REGNO (xop[0]), regno0_hi = regno0_lo + size0 - 1;
  int regno1_lo = REGNO (xop[1]), regno1_hi = regno1_lo + size1 - 1;
  int regoff = regno0_lo - regno1_lo + byteoff;

  // The order of insns matters in the rare case when xop[1] overlaps xop[0].
  int beg = regoff > 0 ? regno1_hi : regno1_lo;
  int end = regoff > 0 ? regno1_lo : regno1_hi;
  int inc = regoff > 0 ? -1 : 1;

  rtx (*gen)(rtx,rtx,rtx) = xior == XOR ? gen_xorqi3 : gen_iorqi3;

  for (int i = beg; i != end + inc; i += inc)
    {
      if (IN_RANGE (i + regoff, regno0_lo, regno0_hi))
	{
	  rtx reg0 = all_regs_rtx[i + regoff];
	  rtx reg1 = all_regs_rtx[i];
	  emit_insn (gen (reg0, reg0, reg1));
	}
    }
}


/* Output sign extension from XOP[1] to XOP[0] and return "".
   If PLEN == NULL, print assembler instructions to perform the operation;
   otherwise, set *PLEN to the length of the instruction sequence (in words)
   as printed with PLEN == NULL.  */

const char *
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

const char *
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


/* Output instructions to insert an inverted bit into OP[0]: $0.$1 = ~$2.$3.
   If PLEN = NULL then output the respective instruction sequence which
   is a combination of BST / BLD and some instruction(s) to invert the bit.
   If PLEN != NULL then store the length of the sequence (in words) in *PLEN.
   Return "".  */

const char *
avr_out_insert_notbit (rtx_insn *insn, rtx op[], int *plen)
{
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


/* Output instructions for  XOP[0] = (XOP[1] <Shift> XOP[2]) & XOP[3]  where
   -  XOP[0] and XOP[1] have the same mode which is one of: QI, HI, PSI, SI.
   -  XOP[3] is an exact const_int power of 2.
   -  XOP[2] and XOP[3] are const_int.
   -  <Shift> is any of: ASHIFT, LSHIFTRT, ASHIFTRT.
   -  The result depends on XOP[1].
   or  XOP[0] = XOP[1] & XOP[2]  where
   -  XOP[0] and XOP[1] have the same mode which is one of: QI, HI, PSI, SI.
   -  XOP[2] is an exact const_int power of 2.
   Returns "".
   PLEN != 0: Set *PLEN to the code length in words.  Don't output anything.
   PLEN == 0: Output instructions.  */

const char*
avr_out_insv (rtx_insn *insn, rtx xop[], int *plen)
{
  machine_mode mode = GET_MODE (xop[0]);
  int n_bytes = GET_MODE_SIZE (mode);
  rtx xsrc = SET_SRC (single_set (insn));

  gcc_assert (AND == GET_CODE (xsrc));

  rtx xop2 = xop[2];
  rtx xop3 = xop[3];

  if (REG_P (XEXP (xsrc, 0)))
    {
      // This function can also handle AND with an exact power of 2,
      // which can be regarded as a XOP[1] shift with offset 0.
      rtx xshift = gen_rtx_ASHIFT (mode, xop[1], const0_rtx);
      xsrc = gen_rtx_AND (mode, xshift, xop[2]);
      xop3 = xop[2];
      xop2 = const0_rtx;
    }

  // Any of ASHIFT, LSHIFTRT, ASHIFTRT.
  rtx_code code = GET_CODE (XEXP (xsrc, 0));
  int shift = code == ASHIFT ? INTVAL (xop2) : -INTVAL (xop2);

  // Determines the position of the output bit.
  unsigned mask = GET_MODE_MASK (mode) & INTVAL (xop3);

  // Position of the output / input bit, respectively.
  int obit = exact_log2 (mask);
  int ibit = obit - shift;

  gcc_assert (IN_RANGE (obit, 0, GET_MODE_BITSIZE (mode) - 1));
  gcc_assert (IN_RANGE (ibit, 0, GET_MODE_BITSIZE (mode) - 1));

  // In the remainder, use the sub-bytes that hold the bits.
  rtx op[4] =
    {
      // Output
      avr_byte (xop[0], obit / 8), GEN_INT (obit & 7),
      // Input
      avr_byte (xop[1], ibit / 8), GEN_INT (ibit & 7)
    };
  obit &= 7;
  ibit &= 7;

  // The length of the default sequence at the end of this function.
  // We only emit anything other than the default when we find a sequence
  // that is strictly shorter than the default sequence; which is:
  // BST + <CLR-result-bytes> + BLD.
  const int len0 = 2 + n_bytes - (n_bytes == 4 && AVR_HAVE_MOVW);

  // Finding something shorter than the default sequence implies that there
  // must be at most 2 instructions that deal with the bytes containing the
  // relevant bits.  In addition, we need  N_BYTES - 1  instructions to clear
  // the remaining result bytes.

  const int n_clr = n_bytes - 1;
  bool clr_p = false;
  bool andi_p = false;

  if (plen)
    *plen = 0;

  if (REGNO (op[0]) == REGNO (op[2])
      // Output reg allows ANDI.
      && test_hard_reg_class (LD_REGS, op[0]))
    {
      if (1 + n_clr < len0
	  // Same byte and bit: A single ANDI will do.
	  && obit == ibit)
	{
	  clr_p = andi_p = true;
	}
      else if (2 + n_clr < len0
	       // |obit - ibit| = 4:  SWAP + ANDI will do.
	       && (obit == ibit + 4 || obit == ibit - 4))
	{
	  avr_asm_len ("swap %0", op, plen, 1);
	  clr_p = andi_p = true;
	}
      else if (2 + n_clr < len0
	       // LSL + ANDI will do.
	       && obit == ibit + 1)
	{
	  avr_asm_len ("lsl %0", op, plen, 1);
	  clr_p = andi_p = true;
	}
      else if (2 + n_clr < len0
	       // LSR + ANDI will do.
	       && obit == ibit - 1)
	{
	  avr_asm_len ("lsr %0", op, plen, 1);
	  clr_p = andi_p = true;
	}
    }

  if (REGNO (op[0]) != REGNO (op[2])
      && obit == ibit)
    {
      if (2 + n_clr < len0
	  // Same bit but different byte: MOV + ANDI will do.
	  && test_hard_reg_class (LD_REGS, op[0]))
	{
	  avr_asm_len ("mov %0,%2", op, plen, 1);
	  clr_p = andi_p = true;
	}
      else if (2 + n_clr < len0
	       // Same bit but different byte:  We can use ANDI + MOV,
	       // but only if the input byte is LD_REGS and unused after.
	       && test_hard_reg_class (LD_REGS, op[2])
	       && reg_unused_after (insn, op[2]))
	{
	  avr_asm_len ("andi %2,1<<%3"  CR_TAB
		       "mov %0,%2", op, plen, 2);
	  clr_p = true;
	}
    }

  // Output remaining instructions of the shorter sequence.

  if (andi_p)
    avr_asm_len ("andi %0,1<<%1", op, plen, 1);

  if (clr_p)
    {
      for (int b = 0; b < n_bytes; ++b)
	{
	  rtx byte = avr_byte (xop[0], b);
	  if (REGNO (byte) != REGNO (op[0]))
	    avr_asm_len ("clr %0", &byte, plen, 1);
	}

      // CLR_P means we found a shorter sequence, so we are done now.
      return "";
    }

  // No shorter sequence found, just emit  BST, CLR*, BLD  sequence.

  avr_asm_len ("bst %2,%3", op, plen, -1);

  if (n_bytes == 4 && AVR_HAVE_MOVW)
    avr_asm_len ("clr %A0"   CR_TAB
		 "clr %B0"   CR_TAB
		 "movw %C0,%A0", xop, plen, 3);
  else
    for (int b = 0; b < n_bytes; ++b)
      {
	rtx byte = avr_byte (xop[0], b);
	avr_asm_len ("clr %0", &byte, plen, 1);
      }

  return avr_asm_len ("bld %0,%1", op, plen, 1);
}


/* Output instructions to extract a bit to 8-bit register XOP[0].
   The input XOP[1] is a register or an 8-bit MEM in the lower I/O range.
   XOP[2] is the const_int bit position.  Return "".

   PLEN != 0: Set *PLEN to the code length in words.  Don't output anything.
   PLEN == 0: Output instructions.  */

const char *
avr_out_extr (rtx_insn *insn, rtx xop[], int *plen)
{
  rtx dest = xop[0];
  rtx src = xop[1];
  int bit = INTVAL (xop[2]);

  if (GET_MODE (src) != QImode)
    {
      src = xop[1] = avr_byte (src, bit / 8);
      bit %= 8;
      xop[2] = GEN_INT (bit);
    }

  if (MEM_P (src))
    {
      xop[1] = XEXP (src, 0); // address
      gcc_assert (low_io_address_operand (xop[1], Pmode));

      return avr_asm_len ("clr %0"      CR_TAB
			  "sbic %i1,%2" CR_TAB
			  "inc %0", xop, plen, -3);
    }

  gcc_assert (REG_P (src));

  bool ld_dest_p = test_hard_reg_class (LD_REGS, dest);
  bool ld_src_p = test_hard_reg_class (LD_REGS, src);

  if (ld_dest_p
      && REGNO (src) == REGNO (dest))
    {
      if (bit == 0)
	return avr_asm_len ("andi %0,1", xop, plen, -1);
      if (bit == 1)
	return avr_asm_len ("lsr %0" CR_TAB
			    "andi %0,1", xop, plen, -2);
      if (bit == 4)
	return avr_asm_len ("swap %0" CR_TAB
			    "andi %0,1", xop, plen, -2);
    }

  if (bit == 0
      && REGNO (src) != REGNO (dest))
  {
    if (ld_dest_p)
      return avr_asm_len ("mov %0,%1" CR_TAB
			  "andi %0,1", xop, plen, -2);
    if (ld_src_p
	&& reg_unused_after (insn, src))
      return avr_asm_len ("andi %1,1" CR_TAB
			  "mov %0,%1", xop, plen, -2);
  }

  return avr_asm_len ("bst %1,%2" CR_TAB
		      "clr %0"    CR_TAB
		      "bld %0,0", xop, plen, -3);
}


/* Output instructions to extract a negated bit to 8-bit register XOP[0].
   The input XOP[1] is an 8-bit register or MEM in the lower I/O range.
   XOP[2] is the const_int bit position.  Return "".

   PLEN != 0: Set *PLEN to the code length in words.  Don't output anything.
   PLEN == 0: Output instructions.  */

const char *
avr_out_extr_not (rtx_insn * /* insn */, rtx xop[], int *plen)
{
  rtx dest = xop[0];
  rtx src = xop[1];
  int bit = INTVAL (xop[2]);

  if (MEM_P (src))
    {
      xop[1] = XEXP (src, 0); // address
      gcc_assert (low_io_address_operand (xop[1], Pmode));

      return avr_asm_len ("clr %0"      CR_TAB
			  "sbis %i1,%2" CR_TAB
			  "inc %0", xop, plen, -3);
    }

  gcc_assert (REG_P (src));

  bool ld_src_p = test_hard_reg_class (LD_REGS, src);

  if (ld_src_p
      && REGNO (src) == REGNO (dest))
    {
      if (bit == 0)
	return avr_asm_len ("inc %0" CR_TAB
			    "andi %0,1", xop, plen, -2);
      if (bit == 1)
	return avr_asm_len ("lsr %0" CR_TAB
			    "inc %0" CR_TAB
			    "andi %0,1", xop, plen, -3);
      if (bit == 4)
	return avr_asm_len ("swap %0" CR_TAB
			    "inc %0"  CR_TAB
			    "andi %0,1", xop, plen, -3);
    }

  if (bit == 7
      && ld_src_p)
    return avr_asm_len ("cpi %1,0x80" CR_TAB
			"sbc %0,%0"   CR_TAB
			"neg %0", xop, plen, -3);

  if (REGNO (src) != REGNO (dest))
    return avr_asm_len ("clr %0"     CR_TAB
			"sbrs %1,%2" CR_TAB
			"inc %0", xop, plen, -3);

  return avr_asm_len ("clr __tmp_reg__" CR_TAB
		      "sbrs %1,%2"      CR_TAB
		      "inc __tmp_reg__" CR_TAB
		      "mov %0,__tmp_reg__", xop, plen, -4);
}


/* Outputs instructions needed for fixed point type conversion.
   This includes converting between any fixed point type, as well
   as converting to any integer type.  Conversion between integer
   types is not supported.

   Converting signed fractional types requires a bit shift if converting
   to or from any unsigned fractional type because the decimal place is
   shifted by 1 bit.  When the destination is a signed fractional, the sign
   is stored in either the carry or T bit.  */

const char *
avr_out_fract (rtx_insn *insn, rtx operands[], bool intsigned, int *plen)
{
  rtx xop[6];
  rtx_code shift = UNKNOWN;
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

  if (dest.fbit % 8 == 0 && src.fbit % 8 == 7)
    shift = ASHIFT;
  else if (dest.fbit % 8 == 7 && src.fbit % 8 == 0)
    shift = ASHIFTRT;
  else if (dest.fbit % 8 == src.fbit % 8)
    shift = UNKNOWN;
  else
    gcc_unreachable ();

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
	  avr_asm_len ("\n0:", nullptr, plen, 0);
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
      const char *code = nullptr;
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
		       "clc\n0:", nullptr, plen, 2);
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
	      avr_asm_len ("dec __zero_reg__", nullptr, plen, 1);
	      if (have_carry)
		avr_asm_len ("clt" CR_TAB
			     "bld __zero_reg__,7", nullptr, plen, 2);
	      else
		avr_asm_len ("lsr __zero_reg__", nullptr, plen, 1);
	      avr_asm_len (have_carry && lsb_in_tmp_reg
			   ? "adc __tmp_reg__,__zero_reg__"
			   : have_carry ? "adc %2,__zero_reg__"
			   : lsb_in_tmp_reg ? "add __tmp_reg__,__zero_reg__"
			   : "add %2,__zero_reg__",
			   xop, plen, 1);
	      avr_asm_len ("eor __zero_reg__,__zero_reg__", nullptr, plen, 1);
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

const char *
avr_out_round (rtx_insn * /*insn*/, rtx *xop, int *plen)
{
  scalar_mode mode = as_a <scalar_mode> (GET_MODE (xop[0]));
  scalar_int_mode imode = int_mode_for_mode (mode).require ();
  // The smallest fractional bit not cleared by the rounding is 2^(-RP).
  int fbit = (int) GET_MODE_FBIT (mode);
  double_int i_add = double_int_zero.set_bit (fbit-1 - INTVAL (xop[2]));
  wide_int wi_add = wi::set_bit_in_zero (fbit-1 - INTVAL (xop[2]),
					 GET_MODE_PRECISION (imode));
  // Lengths of PLUS and AND parts.
  int len_add = 0, *plen_add = plen ? &len_add : nullptr;
  int len_and = 0, *plen_and = plen ? &len_and : nullptr;

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
  avr_out_plus (xpattern, op, plen_add, false /* Don't print "0:" */);

  avr_asm_len ("rjmp 1f" CR_TAB
	       "0:", nullptr, plen_add, 1);

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
  avr_asm_len ("1:", nullptr, plen, 0);

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

  int move_size = GET_MODE_SIZE (move_mode);
  /* Number of bytes/words to rotate.  */
  int offset = (num  >> 3) / move_size;
  /* Number of moves needed.  */
  int size = GET_MODE_SIZE (mode) / move_size;
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

  /* As we pretend jump tables in .text, fix branch offsets crossing jump
     tables now.  */

  if (JUMP_TABLE_DATA_P (insn))
    return 0;

  /* Some complex insns don't need length adjustment and therefore
     the length need not/must not be adjusted for these insns.
     It is easier to state this in an insn attribute "adjust_len" than
     to clutter up code here...  */

  if (!NONDEBUG_INSN_P (insn) || recog_memoized (insn) == -1)
    {
      return len;
    }

  /* Read from insn attribute "adjust_len" if/how length is to be adjusted.  */

  attr_adjust_len adjust_len = get_attr_adjust_len (insn);

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
    case ADJUST_LEN_EXTR_NOT: avr_out_extr_not (insn, op, &len); break;
    case ADJUST_LEN_EXTR: avr_out_extr (insn, op, &len); break;
    case ADJUST_LEN_INSV: avr_out_insv (insn, op, &len); break;
    case ADJUST_LEN_SET_SOME: avr_out_set_some (insn, op, &len); break;

    case ADJUST_LEN_PLUS: avr_out_plus (insn, op, &len); break;
    case ADJUST_LEN_PLUS_EXT: avr_out_plus_ext (insn, op, &len); break;
    case ADJUST_LEN_ADDTO_SP: avr_out_addto_sp (op, &len); break;

    case ADJUST_LEN_MOV8:  output_movqi (insn, op, &len); break;
    case ADJUST_LEN_MOV16: output_movhi (insn, op, &len); break;
    case ADJUST_LEN_MOV24: avr_out_movpsi (insn, op, &len); break;
    case ADJUST_LEN_MOV32: output_movsisf (insn, op, &len); break;
    case ADJUST_LEN_CPYMEM: avr_out_cpymem (insn, op, &len); break;
    case ADJUST_LEN_XLOAD: avr_out_xload (insn, op, &len); break;
    case ADJUST_LEN_FLOAD: avr_out_fload (insn, op, &len); break;
    case ADJUST_LEN_SEXT: avr_out_sign_extend (insn, op, &len); break;
    case ADJUST_LEN_SEXTR: avr_out_sextr (insn, op, &len); break;

    case ADJUST_LEN_SFRACT: avr_out_fract (insn, op, true, &len); break;
    case ADJUST_LEN_UFRACT: avr_out_fract (insn, op, false, &len); break;
    case ADJUST_LEN_ROUND: avr_out_round (insn, op, &len); break;

    case ADJUST_LEN_TSTHI: avr_out_tsthi (insn, op, &len); break;
    case ADJUST_LEN_TSTPSI: avr_out_tstpsi (insn, op, &len); break;
    case ADJUST_LEN_TSTSI: avr_out_tstsi (insn, op, &len); break;
    case ADJUST_LEN_COMPARE: avr_out_compare (insn, op, &len); break;
    case ADJUST_LEN_COMPARE64: avr_out_compare64 (insn, op, &len); break;
    case ADJUST_LEN_CMP_UEXT: avr_out_cmp_ext (op, ZERO_EXTEND, &len); break;
    case ADJUST_LEN_CMP_SEXT: avr_out_cmp_ext (op, SIGN_EXTEND, &len); break;
    case ADJUST_LEN_CMP_LSR: avr_out_cmp_lsr (insn, op, &len); break;

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
    case ADJUST_LEN_ADD_SET_ZN: avr_out_plus_set_ZN (op, &len); break;
    case ADJUST_LEN_ADD_SET_N:  avr_out_plus_set_N (op, &len); break;

    case ADJUST_LEN_ADD_GE0: avr_out_add_msb (insn, op, GE, &len); break;
    case ADJUST_LEN_ADD_LT0: avr_out_add_msb (insn, op, LT, &len); break;

    case ADJUST_LEN_INSV_NOTBIT: avr_out_insert_notbit (insn, op, &len); break;

    default:
      gcc_unreachable ();
    }

  return len;
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
	default_assemble_integer (avr_byte (x, n), 1, aligned_p);

      return true;
    }

  if (AVR_TINY
      && avr_address_tiny_pm_p (x))
    {
      x = plus_constant (Pmode, x, avr_arch->flash_pm_offset);
    }

  return default_assemble_integer (x, size, aligned_p);
}


/* Implement `TARGET_CLASS_MAX_NREGS'.  Reasons described in comments for
   avr_hard_regno_nregs. */

static unsigned char
avr_class_max_nregs (reg_class_t rclass, machine_mode mode)
{
  if (rclass == CC_REG && GET_MODE_CLASS (mode) == MODE_CC)
    return 1;

  return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
}


/* Implement `TARGET_CLASS_LIKELY_SPILLED_P'.  */
/* Return value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.  */

static bool
avr_class_likely_spilled_p (reg_class_t c)
{
  return (c != ALL_REGS
	  && (AVR_TINY ? 1 : c != ADDW_REGS));
}


/* Valid attributes:
   progmem   -	Put data to program memory.
   signal    -	Make a function to be hardware interrupt.
		After function prologue interrupts remain disabled.
   interrupt -	Make a function to be hardware interrupt. Before function
		prologue interrupts are enabled by means of SEI.
   noblock   -	The function is an ISR that starts with a SEI instruction.
   naked     -	Don't generate function prologue/epilogue and RET
		instruction.  */

/* Handle a "progmem" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
avr_handle_progmem_attribute (tree *node, tree name, tree args,
			      int /*flags*/, bool *no_add_attrs)
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
avr_handle_fndecl_attribute (tree *node, tree name, tree /*args*/,
			     int /*flags*/, bool *no_add_attrs)
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
avr_handle_fntype_attribute (tree *node, tree name, tree /*args*/,
			     int /*flags*/, bool *no_add_attrs)
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
			   int /*flags*/, bool *no_add)
{
  bool io_p = startswith (IDENTIFIER_POINTER (name), "io");
  HOST_WIDE_INT io_start = avr_arch->sfr_offset;
  HOST_WIDE_INT io_end = strcmp (IDENTIFIER_POINTER (name), "io_low") == 0
    ? io_start + 0x1f
    : io_start + 0x3f;
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
		   || ! IN_RANGE (TREE_INT_CST_LOW (arg), io_start, io_end)))
	{
	  warning_at (loc, OPT_Wattributes, "%qE attribute address out of range"
		      " 0x%x%s0x%x", name, (int) io_start, "...", (int) io_end);
	  *no_add = true;
	}
      else
	{
	  tree attribs = DECL_ATTRIBUTES (*node);
	  const char *names[] = { "io", "io_low", "address", nullptr };
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

  // Optimizers must not draw any conclusions from "static int addr;" etc.
  // because the contents of `addr' are not given by its initializer but
  // by the contents at the address as specified by the attribute.
  if (VAR_P (*node) && ! *no_add)
    TREE_THIS_VOLATILE (*node) = 1;

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
	}
      if (!attr || !TREE_VALUE (attr))
	attr = lookup_attribute ("address", DECL_ATTRIBUTES (decl));
      gcc_assert (attr && TREE_VALUE (attr) && TREE_VALUE (TREE_VALUE (attr)));
      return GEN_INT (TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr))));
    }
  return x;
}


/* AVR attributes.  */
TARGET_GNU_ATTRIBUTES (avr_attribute_table,
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "progmem",   0, 0, false, false, false, false,
    avr_handle_progmem_attribute, NULL },
  { "signal", 0, -1, true,  false, false, false,
    avr_handle_fndecl_attribute, NULL },
  { "interrupt", 0, -1, true,  false, false, false,
    avr_handle_fndecl_attribute, NULL },
  { "noblock", 0, 0, true,  false, false, false,
    avr_handle_fndecl_attribute, NULL },
  { "no_gccisr", 0, 0, true,  false, false, false,
    avr_handle_fndecl_attribute, NULL },
  { "naked",     0, 0, false, true,  true,  false,
    avr_handle_fntype_attribute, NULL },
  { "OS_task",   0, 0, false, true,  true,  false,
    avr_handle_fntype_attribute, NULL },
  { "OS_main",   0, 0, false, true,  true,  false,
    avr_handle_fntype_attribute, NULL },
  { "io",        0, 1, true, false, false,  false,
    avr_handle_addr_attribute, NULL },
  { "io_low",    0, 1, true, false, false,  false,
    avr_handle_addr_attribute, NULL },
  { "address",   1, 1, true, false, false,  false,
    avr_handle_addr_attribute, NULL },
  { "absdata",   0, 0, true, false, false,  false,
    avr_handle_absdata_attribute, NULL }
});


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
  else if (avr_addrspace[as].segment >= avropt_n_flash)
    {
      if (loc != UNKNOWN_LOCATION)
	error_at (loc, "address space %qs not supported for devices with "
		  "flash size up to %d KiB", avr_addrspace[as].name,
		  64 * avropt_n_flash);
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


/* Implement `TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID'.  Zero is a valid
   address in all address spaces. Even in ADDR_SPACE_FLASH1 etc..,
   a zero address is valid and means 0x<RAMPZ val>0000, where RAMPZ is
   set to the appropriate segment value. */

static bool
avr_addr_space_zero_address_valid (addr_space_t)
{
  return true;
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
  if (TREE_CODE (decl) != VAR_DECL)
    return 0;

  if (avr_decl_memx_p (decl)
      || avr_decl_flashx_p (decl))
    return 2;

  if (avr_decl_flash_p (decl))
    return 1;

  if (NULL_TREE
      != lookup_attribute ("progmem", attributes))
    return -1;

  tree a = decl;

  do
    a = TREE_TYPE (a);
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
  return (VAR_P (decl)
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
      tree target = TREE_TYPE (typ);

      /* Pointer to function: Test the function's return type.  */

      if (FUNCTION_TYPE == TREE_CODE (target))
	return avr_nonconst_pointer_addrspace (TREE_TYPE (target));

      /* "Ordinary" pointers... */

      while (TREE_CODE (target) == ARRAY_TYPE)
	target = TREE_TYPE (target);

      /* Pointers to non-generic address space must be const.  */

      addr_space_t as = TYPE_ADDR_SPACE (target);

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
  const char *reason = nullptr;

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

  return reason == nullptr;
}


/* Helper for `avr_insert_attributes'.  Print an error when there are invalid
   attributes named NAME, where NAME is in { "signal", "interrupt" }.  */

static void
avr_handle_isr_attribute (tree node, tree *attrs, const char *name)
{
  bool seen = false;

  for (tree list = lookup_attribute (name, *attrs); list;
       list = lookup_attribute (name, TREE_CHAIN (list)))
    {
      seen = true;
      for (tree v = TREE_VALUE (list); v; v = TREE_CHAIN (v))
	{
	  int num = avr_isr_number (TREE_VALUE (v));
	  if (! num)
	    error ("attribute %qs expects a constant positive integer"
		   " argument", name);
	  if (TARGET_CVT
	      && num >= 4)
	    error ("vector number %d of %q+D is out of range 1%s3 for"
		   " compact vector table", num, node, "...");
	}
    }

  if (seen
      && ! lookup_attribute ("used", *attrs))
    {
      *attrs = tree_cons (get_identifier ("used"), NULL, *attrs);
    }
}


/* Implement `TARGET_INSERT_ATTRIBUTES'.  */

static void
avr_insert_attributes (tree node, tree *attributes)
{
  if (VAR_P (node)
      && ! TREE_STATIC (node)
      && ! DECL_EXTERNAL (node))
    {
      const char *names[] = { "io", "io_low", "address", nullptr };
      for (const char **p = names; *p; ++p)
	if (lookup_attribute (*p, *attributes))
	  error ("variable %q+D with attribute %qs must be located in "
		 "static storage", node, *p);
    }

  avr_pgm_check_var_decl (node);

  if (TARGET_MAIN_IS_OS_TASK
      && TREE_CODE (node) == FUNCTION_DECL
      && MAIN_NAME_P (DECL_NAME (node))
      // FIXME:  We'd like to also test `flag_hosted' which is only
      // available in the C-ish fronts, hence no such test for now.
      // Instead, we test the return type of "main" which is not exactly
      // the same but good enough.
      && INTEGRAL_TYPE_P (TREE_TYPE (TREE_TYPE (node)))
      && NULL == lookup_attribute ("OS_task", *attributes))
    {
      *attributes = tree_cons (get_identifier ("OS_task"),
			       NULL, *attributes);
    }

#if defined WITH_AVRLIBC
  if (avropt_call_main == 0
      && TREE_CODE (node) == FUNCTION_DECL
      && MAIN_NAME_P (DECL_NAME (node)))
    {
      const char *s_section_name = nullptr;

      if (tree a_sec = lookup_attribute ("section", *attributes))
	if (TREE_VALUE (a_sec))
	  if (tree t_section_name = TREE_VALUE (TREE_VALUE (a_sec)))
	    if (TREE_CODE (t_section_name) == STRING_CST)
	      s_section_name = TREE_STRING_POINTER (t_section_name);

      bool in_init9_p = s_section_name && !strcmp (s_section_name, ".init9");

      if (s_section_name && !in_init9_p)
	{
	  warning (OPT_Wattributes, "%<section(\"%s\")%> attribute on main"
		   " function inhibits %<-mno-call-main%>", s_section_name);
	}
      else
	{
	  if (!lookup_attribute ("noreturn", *attributes))
	    *attributes = tree_cons (get_identifier ("noreturn"),
				     NULL_TREE, *attributes);
	  // Put main into section .init9 so that it is executed even
	  // though it's not called.
	  if (!in_init9_p)
	    {
	      tree init9 = build_string (1 + strlen (".init9"), ".init9");
	      tree arg = build_tree_list (NULL_TREE, init9);
	      *attributes = tree_cons (get_identifier ("section"),
				       arg, *attributes);
	    }
	  avr_no_call_main_p = true;
	}
    } // -mno-call-main
#endif // AVR-LibC

  avr_handle_isr_attribute (node, attributes, "signal");
  avr_handle_isr_attribute (node, attributes, "interrupt");

  /* Add the section attribute if the variable is in progmem.  */

  if (VAR_P (node)
      && (TREE_STATIC (node) || DECL_EXTERNAL (node))
      && avr_progmem_p (node, *attributes))
    {
      tree node0 = node;

      /* For C++, we have to peel arrays in order to get correct
	 determination of readonlyness.  */

      do
	node0 = TREE_TYPE (node0);
      while (TREE_CODE (node0) == ARRAY_TYPE);

      if (error_mark_node == node0)
	return;

      addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (node));

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

#ifdef HAVE_LD_AVR_AVRXMEGA2_FLMAP
static const bool have_avrxmega2_flmap = true;
#else
static const bool have_avrxmega2_flmap = false;
#endif

#ifdef HAVE_LD_AVR_AVRXMEGA4_FLMAP
static const bool have_avrxmega4_flmap = true;
#else
static const bool have_avrxmega4_flmap = false;
#endif

#ifdef HAVE_LD_AVR_AVRXMEGA3_RODATA_IN_FLASH
static const bool have_avrxmega3_rodata_in_flash = true;
#else
static const bool have_avrxmega3_rodata_in_flash = false;
#endif


static bool
avr_rodata_in_flash_p ()
{
  switch (avr_arch_index)
    {
    default:
      break;

    case ARCH_AVRTINY:
      return true;

    case ARCH_AVRXMEGA3:
      return have_avrxmega3_rodata_in_flash;

    case ARCH_AVRXMEGA2:
      return avropt_flmap && have_avrxmega2_flmap && avropt_rodata_in_ram != 1;

    case ARCH_AVRXMEGA4:
      return avropt_flmap && have_avrxmega4_flmap && avropt_rodata_in_ram != 1;
    }

  return false;
}


/* Implement `ASM_OUTPUT_ALIGNED_DECL_LOCAL'.  */
/* Implement `ASM_OUTPUT_ALIGNED_DECL_COMMON'.  */
/* Track need of __do_clear_bss.  */

void
avr_asm_output_aligned_decl_common (FILE *stream, tree /* decl */,
				    const char *name,
				    unsigned HOST_WIDE_INT size,
				    unsigned int align, bool local_p)
{
  /* __gnu_lto_slim is just a marker for the linker injected by toplev.cc.
     There is no need to trigger __do_clear_bss code for them.  */

  if (!startswith (name, "__gnu_lto"))
    avr_need_clear_bss_p = true;

  if (local_p)
    ASM_OUTPUT_ALIGNED_LOCAL (stream, name, size, align);
  else
    ASM_OUTPUT_ALIGNED_COMMON (stream, name, size, align);
}


/* Implement `ASM_OUTPUT_ALIGNED_BSS'.  */

void
avr_asm_asm_output_aligned_bss (FILE *file, tree decl, const char *name,
				unsigned HOST_WIDE_INT size, int align,
				void (*default_func)
				  (FILE *, tree, const char *,
				   unsigned HOST_WIDE_INT, int))
{
  if (!startswith (name, "__gnu_lto"))
    avr_need_clear_bss_p = true;

  default_func (file, decl, name, size, align);
}


/* Unnamed section callback for data_section
   to track need of __do_copy_data.  */

static void
avr_output_data_section_asm_op (const char *data)
{
  avr_need_copy_data_p = true;

  /* Dispatch to default.  */
  output_section_asm_op (data);
}


/* Unnamed section callback for bss_section
   to track need of __do_clear_bss.  */

static void
avr_output_bss_section_asm_op (const char *data)
{
  avr_need_clear_bss_p = true;

  /* Dispatch to default.  */
  output_section_asm_op (data);
}


/* Unnamed section callback for progmem*.data sections.  */

static void
avr_output_progmem_section_asm_op (const char *data)
{
  fprintf (asm_out_file, "\t.section\t%s,\"a\",@progbits\n", data);
}


/* A noswitch section callback to output symbol definitions for
   attributes "io", "io_low" and "address".  */

static bool
avr_output_addr_attrib (tree decl, const char *name,
			unsigned HOST_WIDE_INT /* size */,
			unsigned HOST_WIDE_INT /* align */)
{
  gcc_assert (DECL_RTL_SET_P (decl));

  FILE *stream = asm_out_file;
  bool local_p = ! DECL_WEAK (decl) && ! TREE_PUBLIC (decl);
  rtx symbol, mem = DECL_RTL (decl);

  if (mem != NULL_RTX && MEM_P (mem)
      && SYMBOL_REF_P ((symbol = XEXP (mem, 0)))
      && (SYMBOL_REF_FLAGS (symbol) & (SYMBOL_FLAG_IO | SYMBOL_FLAG_ADDRESS)))
    {
      if (! local_p)
	{
	  fprintf (stream, "\t%s\t", DECL_WEAK (decl) ? ".weak" : ".globl");
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
	{
	  const char *names[] = { "io", "io_low", "address", nullptr };
	  for (const char **p = names; *p; ++p)
	    if (lookup_attribute (*p, DECL_ATTRIBUTES (decl)))
	      {
		error ("static attribute %qs declaration for %q+D needs an "
		       "address", *p, decl);
		break;
	      }
	}

      return true;
    }

  gcc_unreachable ();

  return false;
}


/* Implement `TARGET_ASM_INIT_SECTIONS'.  */

static void
avr_asm_init_sections (void)
{
  /* Override section callbacks to keep track of `avr_need_clear_bss_p',
     `avr_need_copy_data_p' and `avr_has_rodata_p'.
     Track also .rodata for the case when .rodata is located in RAM.  */

  if (! avr_rodata_in_flash_p ())
    readonly_data_section->unnamed.callback = avr_output_data_section_asm_op;
  data_section->unnamed.callback = avr_output_data_section_asm_op;
  bss_section->unnamed.callback = avr_output_bss_section_asm_op;
  tls_comm_section->noswitch.callback = avr_output_addr_attrib;
}


/* Implement `TARGET_ASM_NAMED_SECTION'.  */
/* Track need of __do_clear_bss, __do_copy_data for named sections.  */

static void
avr_asm_named_section (const char *name, unsigned int flags, tree decl)
{
  if (flags & AVR_SECTION_PROGMEM
      // Only use section .progmem*.data if there is no attribute section.
      && ! (decl
	    && DECL_SECTION_NAME (decl)
	    && symtab_node::get (decl)
	    && ! symtab_node::get (decl)->implicit_section))
    {
      addr_space_t as = (flags & AVR_SECTION_PROGMEM) / SECTION_MACH_DEP;
      const char *old_prefix = ".rodata";
      const char *new_prefix = avr_addrspace[as].section_name;

      if (startswith (name, old_prefix))
	{
	  const char *sname = ACONCAT ((new_prefix,
					name + strlen (old_prefix), nullptr));
	  default_elf_asm_named_section (sname, flags, decl);
	  return;
	}

      default_elf_asm_named_section (new_prefix, flags, decl);
      return;
    }

  if (!avr_need_copy_data_p)
    avr_need_copy_data_p = (startswith (name, ".data")
			    || startswith (name, ".gnu.linkonce.d"));

  if (!avr_has_rodata_p)
    avr_has_rodata_p = (startswith (name, ".rodata")
			|| startswith (name, ".gnu.linkonce.r"));

  if (!avr_need_clear_bss_p)
    avr_need_clear_bss_p = startswith (name, ".bss");

  default_elf_asm_named_section (name, flags, decl);
}


/* Implement `TARGET_SECTION_TYPE_FLAGS'.  */

static unsigned int
avr_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags = default_section_type_flags (decl, name, reloc);

  if (startswith (name, ".noinit"))
    {
      if (decl && VAR_P (decl)
	  && DECL_INITIAL (decl) == NULL_TREE)
	flags |= SECTION_BSS;  /* @nobits */
      else
	warning (0, "only uninitialized variables can be placed in the "
		 "%<.noinit%> section");
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
      flags &= ~SECTION_NOTYPE;
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
      || DECL_SECTION_NAME (node) != nullptr)
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

      if (!AVR_TINY && avr_progmem_p (decl, attr) == -1)
	as = ADDR_SPACE_FLASH;

      AVR_SYMBOL_SET_ADDR_SPACE (sym, as);

      tree io_low_attr = lookup_attribute ("io_low", attr);
      tree io_attr = lookup_attribute ("io", attr);
      tree address_attr = lookup_attribute ("address", attr);

      if (io_low_attr
	  && TREE_VALUE (io_low_attr) && TREE_VALUE (TREE_VALUE (io_low_attr)))
	addr_attr = io_low_attr;
      else if (io_attr
	       && TREE_VALUE (io_attr) && TREE_VALUE (TREE_VALUE (io_attr)))
	addr_attr = io_attr;
      else
	addr_attr = address_attr;

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

      if (io_attr || io_low_attr || address_attr)
	{
	  if (DECL_INITIAL (decl))
	    {
	      /* Initializers are not yet parsed in TARGET_INSERT_ATTRIBUTES,
		 hence deny initializers now.  The values of symbols with an
		 address attribute are determined by the attribute, not by
		 some initializer.  */

	      error ("variable %q+D with attribute %qs must not have an "
		     "initializer", decl,
		     io_low_attr ? "io_low" : io_attr ? "io" : "address");
	    }
	  else
	    {
	      /* PR112952: The only way to output a variable declaration in a
		 custom manner is by means of a noswitch section callback.
		 There are only three noswitch sections: comm_section,
		 lcomm_section and tls_comm_section.  And there is no way to
		 wire a custom noswitch section to a decl.  As lcomm_section
		 is bypassed with -fdata-sections -fno-common, there is no
		 other way than making use of tls_comm_section.  As we are
		 using that section anyway, also use it in the public case.  */

	      DECL_COMMON (decl) = 1;
	      set_decl_section_name (decl, (const char *) nullptr);
	      set_decl_tls_model (decl, (tls_model) 2);
	    }
	}
    }

  if (AVR_TINY
      && decl
      && VAR_P (decl)
      && MEM_P (rtl)
      && SYMBOL_REF_P (XEXP (rtl, 0)))
    {
      rtx sym = XEXP (rtl, 0);
      bool progmem_p = avr_progmem_p (decl, DECL_ATTRIBUTES (decl)) == -1;

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
  section *sect = default_elf_select_section (decl, reloc, align);

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
	  const char *name = sect->named.name;
	  const char *old_prefix = ".rodata";
	  const char *new_prefix = avr_addrspace[as].section_name;

	  if (startswith (name, old_prefix))
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
    error ("architecture %qs supported for assembler only", avropt_mmcu);

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

  if (avr_need_copy_data_p
      || (avr_has_rodata_p && ! avr_rodata_in_flash_p ()))
    fputs (".global __do_copy_data\n", asm_out_file);

  if (avr_need_clear_bss_p)
    fputs (".global __do_clear_bss\n", asm_out_file);

  /* Don't let __call_main call main() and exit().
     Defining this symbol will keep the code from being pulled
     in from lib<mcu>.a as requested by AVR-LibC's gcrt1.S.
     We invoke main() by other means: putting it in .init9.  */

  if (avr_no_call_main_p)
    fputs (".global __call_main\n"
	   "__call_main = 0\n", asm_out_file);
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
avr_register_move_cost (machine_mode /*mode*/, reg_class_t from, reg_class_t to)
{
  return (from == STACK_REG ? 6
	  : to == STACK_REG ? 12
	  : 2);
}


/* Implement `TARGET_MEMORY_MOVE_COST' */

static int
avr_memory_move_cost (machine_mode mode, reg_class_t /*rclass*/, bool /*in*/)
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


/* Return the expected cost of a conditional branch like
   (set (pc)
	(if_then_else (X)
		      (label_ref *)
		      (pc)))
   where X is some comparison operator.  */

static int
avr_cbranch_cost (rtx x)
{
  bool difficult_p = difficult_comparison_operator (x, VOIDmode);

  if (reload_completed)
    {
      // After reload, we basically just have plain branches.
      return COSTS_N_INSNS (1 + difficult_p);
    }

  rtx xreg = XEXP (x, 0);
  rtx xval = XEXP (x, 1);
  machine_mode mode = GET_MODE (xreg);
  if (mode == VOIDmode)
    mode = GET_MODE (xval);
  int size = GET_MODE_SIZE (mode);

  if (GET_CODE (xreg) == ZERO_EXTEND
      || GET_CODE (xval) == ZERO_EXTEND)
    {
      // *cbranch<HISI:mode>.<code><QIPSI:mode>.0/1, code = zero_extend.
      return COSTS_N_INSNS (size + 1);
    }

  if (GET_CODE (xreg) == SIGN_EXTEND
      || GET_CODE (xval) == SIGN_EXTEND)
    {
      // *cbranch<HISI:mode>.<code><QIPSI:mode>.0/1, code = sign_extend.
      // Make it a bit cheaper than it actually is (less reg pressure).
      return COSTS_N_INSNS (size + 1 + 1);
    }

  if (GET_CODE (xreg) == ZERO_EXTRACT
      && XEXP (xreg, 1) == const1_rtx)
    {
      // Branch on a single bit, with an additional edge due to less
      // register pressure.
      return (int) COSTS_N_INSNS (1.5);
    }

  bool reg_p = register_operand (xreg, mode);
  bool reg_or_0_p = reg_or_0_operand (xval, mode);

  return COSTS_N_INSNS (size
			// For the branch
			+ 1 + difficult_p
			// Combine might propagate constants other than zero
			// into the 2nd operand.  Make that more expensive.
			+ 1 * (!reg_p || !reg_or_0_p));
}


/* Mutually recursive subroutine of `avr_rtx_cost' for calculating the
   cost of an RTX operand given its context.  X is the rtx of the
   operand, MODE is its mode, and OUTER is the rtx_code of this
   operand's parent operator.  */

static int
avr_operand_rtx_cost (rtx x, machine_mode mode, rtx_code outer,
		      int opno, bool speed)
{
  rtx_code code = GET_CODE (x);

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

  int total = 0;
  avr_rtx_costs (x, mode, outer, opno, &total, speed);
  return total;
}


/* Return the default shift costs for an n-byte shift with a constant
   bit offset in terms of cycles (speed) or in terms of words (!speed).  */

static int
avr_default_shift_costs (int n_bytes, int offset, bool speed)
{
  int c_space = 3 + n_bytes;
  int c_speed = offset <= 4
    ? (3 + n_bytes) * offset
    // For larger offsets, don't make the speed costs more costly than
    // an unrolled shift, because we cannot rollback from an unrolled shift.
    : n_bytes * offset;

  return COSTS_N_INSNS (speed ? c_speed : c_space);
}


/* Worker function for AVR backend's rtx_cost function.
   X is rtx expression whose cost is to be calculated.
   Return true if the complete cost has been computed.
   Return false if subexpressions should be scanned.
   In either case, *TOTAL contains the cost result.  */

static bool
avr_rtx_costs_1 (rtx x, machine_mode mode, int outer_code,
		 int /*opno*/, int *total, bool speed)
{
  const rtx_code code = GET_CODE (x);
  const int n_bytes = GET_MODE_SIZE (mode);
  const HOST_WIDE_INT val1 = BINARY_P (x) && CONST_INT_P (XEXP (x, 1))
    ? INTVAL (XEXP (x, 1))
    : -1;

  if (avropt_pr118012)
    {
      if ((code == IOR || code == XOR || code == PLUS)
	  && GET_CODE (XEXP (x, 0)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == ZERO_EXTEND
	  && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == AND
	  && XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 1) == const1_rtx)
	{
	  *total = COSTS_N_INSNS (2 + n_bytes);
	  return true;
	}
    }

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
      *total = COSTS_N_INSNS (n_bytes);
      return true;

    case NEG:
      if (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == ZERO_EXTRACT)
	{
	  // Just a sign_extract of bit 0?
	  rtx y = XEXP (XEXP (x, 0), 0);
	  if (XEXP (y, 1) == const1_rtx
	      && XEXP (y, 2) == const0_rtx)
	    {
	      *total = COSTS_N_INSNS (1 + n_bytes
				      - (AVR_HAVE_MOVW && n_bytes == 4));
	      return true;
	    }
	}

      switch (mode)
	{
	case E_QImode:
	case E_SFmode:
	  *total = COSTS_N_INSNS (1);
	  break;

	case E_HImode:
	case E_PSImode:
	case E_SImode:
	  *total = COSTS_N_INSNS (2 * n_bytes - 1);
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
      *total = COSTS_N_INSNS (n_bytes);
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case ZERO_EXTEND:
      *total = COSTS_N_INSNS (n_bytes
			      - GET_MODE_SIZE (GET_MODE (XEXP (x, 0))));
      *total += avr_operand_rtx_cost (XEXP (x, 0), GET_MODE (XEXP (x, 0)),
				      code, 0, speed);
      return true;

    case SIGN_EXTEND:
      *total = COSTS_N_INSNS (n_bytes + 2
			      - GET_MODE_SIZE (GET_MODE (XEXP (x, 0))));
      *total += avr_operand_rtx_cost (XEXP (x, 0), GET_MODE (XEXP (x, 0)),
				      code, 0, speed);
      return true;

    case PLUS:
      // uint16_t += 2 * uint8_t;
      if (mode == HImode
	  && GET_CODE (XEXP (x, 0)) == ASHIFT
	  && REG_P (XEXP (x, 1))
	  && XEXP (XEXP (x, 0), 1) == const1_rtx
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == ZERO_EXTEND)
	{
	  *total = COSTS_N_INSNS (4);
	  return true;
	}

      // *usum_widenqihi
      if (mode == HImode
	  && GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	  && GET_CODE (XEXP (x, 1)) == ZERO_EXTEND)
	{
	  *total = COSTS_N_INSNS (3);
	  return true;
	}
      // *aligned_add_symbol
      if (mode == HImode
	  && GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	  && const_0mod256_operand (XEXP (x, 1), HImode))
	{
	  *total = COSTS_N_INSNS (1.5);
	  return true;
	}

      // *add<PSISI:mode>3.zero_extend.<QIPSI:mode>
      // *addhi3_zero_extend
      if (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	  && REG_P (XEXP (x, 1)))
	{
	  *total = COSTS_N_INSNS (n_bytes);
	  return true;
	}
      if (REG_P (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == ZERO_EXTEND)
	{
	  *total = COSTS_N_INSNS (n_bytes);
	  return true;
	}

      // *add<HISI:mode>3.sign_extend.<QIPSI:mode>
      if (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	  && REG_P (XEXP (x, 1)))
	{
	  int size2 = GET_MODE_SIZE (GET_MODE (XEXP (XEXP (x, 0), 0)));
	  *total = COSTS_N_INSNS (2 + n_bytes
				  + (n_bytes > 1 + size2));
	  return true;
	}

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
	  else
	    *total = COSTS_N_INSNS (4);
	  break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case MINUS:
      // *udiff_widenqihi
      if (mode == HImode
	  && GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	  && GET_CODE (XEXP (x, 1)) == ZERO_EXTEND)
	{
	  *total = COSTS_N_INSNS (2);
	  return true;
	}
      // *sub<HISI:mode>3.zero_extend.<QIPSI:mode>
      if (REG_P (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == ZERO_EXTEND)
	{
	  *total = COSTS_N_INSNS (n_bytes);
	  return true;
	}
      // *sub<HISI:mode>3.sign_extend.<QIPSI:mode>
      if (REG_P (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == SIGN_EXTEND)
	{
	  int size2 = GET_MODE_SIZE (GET_MODE (XEXP (XEXP (x, 1), 0)));
	  *total = COSTS_N_INSNS (2 + n_bytes
				  + (n_bytes > 1 + size2));
	  return true;
	}

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
      if (IOR == code
	  && AND == GET_CODE (XEXP (x, 0))
	  && AND == GET_CODE (XEXP (x, 1))
	  && single_zero_operand (XEXP (XEXP (x, 0), 1), mode))
	{
	  // Open-coded bit transfer.
	  *total = COSTS_N_INSNS (2);
	  return true;
	}
      if (AND == code
	  && single_one_operand (XEXP (x, 1), mode)
	  && (ASHIFT == GET_CODE (XEXP (x, 0))
	      || ASHIFTRT == GET_CODE (XEXP (x, 0))
	      || LSHIFTRT == GET_CODE (XEXP (x, 0))))
	{
	  // "*insv.any_shift.<mode>
	  *total = COSTS_N_INSNS (1 + n_bytes);
	  return true;
	}
      *total = COSTS_N_INSNS (n_bytes);
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      if (!CONST_INT_P (XEXP (x, 1)))
	*total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
      return true;

    case XOR:
      *total = COSTS_N_INSNS (n_bytes);
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
      return true;

    case MULT:
      if (avropt_pr118012)
	{
	  if (GET_CODE (XEXP (x, 0)) == AND
	      && XEXP (XEXP (x, 0), 1) == const1_rtx)
	    {
	      // Try to defeat PR118012.  The MUL variant is actually very
	      // expensive, but combine is given a pattern to transform this
	      // into something less toxic.  Though this might not work
	      // for SImode, and we still have a completely ridiculous
	      // 32-bit multiplication instead of a simple bit test on
	      // devices that don't even have MUL.  This is because on
	      // AVR_TINY, we'll get a libcall which we cannot undo.
	      // (On other devices that don't have MUL, the libcall is
	      // bypassed by providing mulsi3, cf. insn mulsi3_[call_]pr118012.
	      *total = 0;
	      return true;
	    }
	} // PR118012

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
	      rtx_code code0 = GET_CODE (op0);
	      rtx_code code1 = GET_CODE (op1);
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
	*total = COSTS_N_INSNS (15 * n_bytes);
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      /* For div/mod with const-int divisor we have at least the cost of
	 loading the divisor. */
      if (CONST_INT_P (XEXP (x, 1)))
	*total += COSTS_N_INSNS (n_bytes);
      /* Add some overall penaly for clobbering and moving around registers */
      *total += COSTS_N_INSNS (2);
      return true;

    case ROTATE:
      switch (mode)
	{
	case E_QImode:
	  if (val1 == 4)
	    *total = COSTS_N_INSNS (1);
	  break;

	case E_HImode:
	  if (val1 == 8)
	    *total = COSTS_N_INSNS (3);
	  break;

	case E_SImode:
	  if (CONST_INT_P (XEXP (x, 1)))
	    switch (val1)
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
	case E_QImode: // ashlqi3
	  if (speed
	      && XEXP (x, 0) == const1_rtx
	      && GET_CODE (XEXP (x, 1)) == AND)
	    {
	      // "*mask1_0x01"
	      // Leave the space costs alone as they are smaller than 7 here.
	      *total = COSTS_N_INSNS (7);
	      return true;
	    }

	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 4 : 17);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    {
	      if (val1 == 7)
		*total = COSTS_N_INSNS (3);
	      else if (val1 == 6)
		*total = COSTS_N_INSNS (5 - AVR_HAVE_MUL);
	      else if (val1 >= 0 && val1 <= 5)
		*total = COSTS_N_INSNS (val1);
	      else
		*total = COSTS_N_INSNS (1);
	    }
	  break;

	case E_HImode: // ashlhi3
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
	    switch (val1)
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
		*total = avr_default_shift_costs (n_bytes, val1, speed);
		break;
	      }
	  break;

	case E_PSImode: // ashlpsi3
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 6 : 73);
	    }
	  else
	    switch (val1)
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
	      case 8:
	      case 16:
		*total = COSTS_N_INSNS (3);
		break;
	      case 9:
	      case 15:
		*total = COSTS_N_INSNS (6);
		break;
	      case 23:
		*total = COSTS_N_INSNS (5);
		break;
	      default:
		*total = avr_default_shift_costs (n_bytes, val1, speed);
		break;
	      }
	  break;

	case E_SImode: // ashlsi3
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 7 : 113);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (val1)
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
	      case 8:
		*total = COSTS_N_INSNS (4);
		break;
	      case 15:
		*total = COSTS_N_INSNS (8 - AVR_HAVE_MOVW);
		break;
	      case 16:
		*total = COSTS_N_INSNS (4 - AVR_HAVE_MOVW);
		break;
	      case 24:
	      case 25:
	      case 26:
	      case 27:
		*total = COSTS_N_INSNS (4 + val1 - 24);
		break;
	      case 28:
	      case 29:
		*total = COSTS_N_INSNS (6 + val1 - 28);
		break;
	      case 30:
		*total = COSTS_N_INSNS (!speed && AVR_HAVE_MUL ? 7 : 8);
		break;
	      case 31:
		*total = COSTS_N_INSNS (6);
		break;
	      case 2:
		*total = COSTS_N_INSNS (!speed ? 7 : 8);
		break;
	      default:
		*total = avr_default_shift_costs (n_bytes, val1, speed);
		break;
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
	case E_QImode: // ashrqi3
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 4 : 17);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    {
	      if (val1 == 6)
		*total = COSTS_N_INSNS (4);
	      else if (val1 == 7)
		*total = COSTS_N_INSNS (2);
	      else if (val1 >= 0 && val1 <= 5)
		*total = COSTS_N_INSNS (val1);
	      else
		*total = COSTS_N_INSNS (1);
	    }
	  break;

	case E_HImode: // ashrhi3
	  if (CONST_INT_P (XEXP (x, 0))
	      && INTVAL (XEXP (x, 0)) == 128
	      && GET_CODE (XEXP (x, 1)) == AND)
	    {
	      // "*mask1_0x80"
	      *total = COSTS_N_INSNS (7);
	      return true;
	    }

	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 5 : 41);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (val1)
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
		*total = avr_default_shift_costs (n_bytes, val1, speed);
		break;
	      }
	  break;

	case E_PSImode: // ashrpsi3
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 6 : 73);
	    }
	  else
	    switch (val1)
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
		*total = COSTS_N_INSNS (3);
		break;
	      case 8:
	      case 15:
		*total = COSTS_N_INSNS (5);
		break;
	      case 16:
		*total = COSTS_N_INSNS (4);
		break;
	      case 22:
		*total = COSTS_N_INSNS (6);
		break;
	      case 23:
		*total = COSTS_N_INSNS (4);
		break;
	      default:
		*total = avr_default_shift_costs (n_bytes, val1, speed);
		break;
	      }
	  break;

	case E_SImode: // ashrsi3
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 7 : 113);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (val1)
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
		*total = COSTS_N_INSNS (4);
		break;
	      case 8:
		*total = COSTS_N_INSNS (6);
		break;
	      case 15:
		*total = COSTS_N_INSNS (6 - AVR_HAVE_MOVW);
		break;
	      case 16:
		*total = COSTS_N_INSNS (4 - AVR_HAVE_MOVW);
		break;
	      case 24:
		*total = COSTS_N_INSNS (5);
		break;
	      case 2:
		*total = COSTS_N_INSNS (!speed ? 7 : 8);
		break;
	      case 30:
		*total = COSTS_N_INSNS (7 - AVR_HAVE_MOVW);
		break;
	      case 31:
		*total = COSTS_N_INSNS (AVR_HAVE_MOVW ? 4 : 5);
		break;
	      default:
		*total = avr_default_shift_costs (n_bytes, val1, speed);
		break;
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
	case E_QImode: // lshrqi3
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 4 : 17);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    {
	      if (val1 == 7)
		*total = COSTS_N_INSNS (3);
	      else if (val1 == 6)
		*total = COSTS_N_INSNS (5 - AVR_HAVE_MUL);
	      else if (val1 >= 0 && val1 <= 5)
		*total = COSTS_N_INSNS (val1);
	      else
		*total = COSTS_N_INSNS (1);
	    }
	  break;

	case E_HImode: // lshrhi3
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 5 : 41);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (val1)
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
		*total = avr_default_shift_costs (n_bytes, val1, speed);
		break;
	      }
	  break;

	case E_PSImode: // lshrpsi3
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 6 : 73);
	    }
	  else
	    switch (val1)
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
	      case 8:
	      case 16:
		*total = COSTS_N_INSNS (3);
		break;
	      case 15:
		*total = COSTS_N_INSNS (6);
		break;
	      case 23:
		*total = COSTS_N_INSNS (5);
		break;
	      default:
		*total = avr_default_shift_costs (n_bytes, val1, speed);
		break;
	      }
	  break;

	case E_SImode: // lshrsi3
	  if (!CONST_INT_P (XEXP (x, 1)))
	    {
	      *total = COSTS_N_INSNS (!speed ? 7 : 113);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else
	    switch (val1)
	      {
	      case 0:
		*total = 0;
		break;
	      case 1:
	      case 8:
		*total = COSTS_N_INSNS (4);
		break;
	      case 2:
		*total = COSTS_N_INSNS (!speed ? 7 : 8);
		break;
	      case 15:
		*total = COSTS_N_INSNS (8 - AVR_HAVE_MOVW);
		break;
	      case 16:
		*total = COSTS_N_INSNS (4 - AVR_HAVE_MOVW);
		break;
	      case 24:
	      case 25:
	      case 26:
	      case 27:
		*total = COSTS_N_INSNS (4 + val1 - 24);
		break;
	      case 28:
	      case 29:
		*total = COSTS_N_INSNS (6 + val1 - 28);
		break;
	      case 30:
		*total = COSTS_N_INSNS (!speed && AVR_HAVE_MUL ? 7 : 8);
		break;
	      case 31:
		*total = COSTS_N_INSNS (6);
		break;
	      default:
		*total = avr_default_shift_costs (n_bytes, val1, speed);
		break;
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

    case IF_THEN_ELSE:
      if (outer_code == SET
	  && XEXP (x, 2) == pc_rtx
	  && ordered_comparison_operator (XEXP (x, 0), VOIDmode))
	{
	  *total = avr_cbranch_cost (XEXP (x, 0));
	  return true;
	}

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


/* Implement `TARGET_INSN_COST'.  */
/* For some insns, it is not enough to look at the cost of the SET_SRC.
   In that case, have a look at the entire insn, e.g. during insn combine.  */

static int
avr_insn_cost (rtx_insn *insn, bool speed)
{
  const int unknown_cost = -1;
  int cost = unknown_cost;

  rtx set = single_set (insn);

  if (set
      && ZERO_EXTRACT == GET_CODE (SET_DEST (set)))
    {
      // Try find anything that would flip the extracted bit.
      bool not_bit_p = false;

      subrtx_iterator::array_type array;
      FOR_EACH_SUBRTX (iter, array, SET_SRC (set), NONCONST)
	{
	  rtx_code code = GET_CODE (*iter);
	  not_bit_p |= code == NOT || code == XOR || code == GE;
	}

      // Don't go too deep into the analysis.  In almost all cases,
      // using BLD/BST is the best we can do for single-bit moves,
      // even considering CSE.
      cost = COSTS_N_INSNS (2 + not_bit_p);
    }

  if (cost != unknown_cost)
    {
      if (avr_log.rtx_costs)
	avr_edump ("\n%? (%s) insn_cost=%d\n%r\n",
		   speed ? "speed" : "size", cost, insn);
      return cost;
    }

  // Resort to what rtlanal.cc::insn_cost() implements as a default
  // when targetm.insn_cost() is not implemented.

  return pattern_cost (PATTERN (insn), speed);
}


/* Implement `TARGET_ADDRESS_COST'.  */

static int
avr_address_cost (rtx x, machine_mode mode, addr_space_t /*as*/,
		  bool /*speed*/)
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

bool
extra_constraint_Q (rtx x)
{
  bool ok = false;
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
	avr_edump ("\n%?=%d reload_completed=%d ra_in_progress=%d\n %r\n",
		   ok, reload_completed, ra_in_progress (), x);
    }

  return ok;
}


/* Convert condition code CONDITION to the valid AVR condition code.  */

rtx_code
avr_normalize_condition (rtx_code condition)
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


/* Returns register number for function return value.*/

static inline unsigned int
avr_ret_register (void)
{
  return REG_24;
}


/* Implement `TARGET_FUNCTION_VALUE_REGNO_P'.  */

static bool
avr_function_value_regno_p (const unsigned int regno)
{
  return regno == avr_ret_register ();
}


/* Implement `TARGET_LIBCALL_VALUE'.  */
/* Create an RTX representing the place where a
   library function returns a value of mode MODE.  */

static rtx
avr_libcall_value (machine_mode mode, const_rtx /*func*/)
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
avr_function_value (const_tree type, const_tree /*fn_decl_or_type*/,
		    bool /*outgoing*/)
{
  if (TYPE_MODE (type) != BLKmode)
    return avr_libcall_value (TYPE_MODE (type), NULL_RTX);

  unsigned int offs = int_size_in_bytes (type);
  if (offs < 2)
    offs = 2;
  if (offs > 2 && offs < GET_MODE_SIZE (SImode))
    offs = GET_MODE_SIZE (SImode);
  else if (offs > GET_MODE_SIZE (SImode) && offs < GET_MODE_SIZE (DImode))
    offs = GET_MODE_SIZE (DImode);

  return gen_rtx_REG (BLKmode, avr_ret_register () + 2 - offs);
}


bool
test_hard_reg_class (reg_class rclass, rtx x)
{
  int regno = true_regnum (x);
  if (regno < 0)
    return false;

  if (TEST_HARD_REG_CLASS (rclass, regno))
    return true;

  return false;
}


/* Helper for `jump_over_one_insn_p':  Test if INSN is a 2-word instruction
   and thus is suitable to be skipped by CPSE, SBRC, etc.  */

static bool
avr_2word_insn_p (rtx_insn *insn)
{
  if (TARGET_SKIP_BUG || !insn || get_attr_length (insn) != 2)
    {
      return false;
    }

  switch (INSN_CODE (insn))
    {
    default:
      return (recog_memoized (insn) >= 0
	      // Transparent calls may be skipped.
	      && (get_attr_type (insn) == TYPE_XCALL
		  || get_attr_adjust_len (insn) == ADJUST_LEN_CALL));

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


bool
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


/* Implement `TARGET_HARD_REGNO_NREGS'.  CCmode is four units for historical
   reasons. If this hook is not defined, TARGET_HARD_REGNO_NREGS
   reports that CCmode requires four registers.
   Define this hook to allow CCmode to fit in a single REG_CC. For
   other modes and regs, return the number of words in mode (i.e whatever
   the default implementation of the hook returned). */

static unsigned int
avr_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  if (regno == REG_CC && GET_MODE_CLASS (mode) == MODE_CC)
    return 1;

  return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
}


/* Implement `TARGET_HARD_REGNO_MODE_OK'.  On the enhanced core, anything
   larger than 1 byte must start in even numbered register for "movw" to
   work (this way we don't have to check for odd registers everywhere).  */

static bool
avr_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (regno == REG_CC)
    return GET_MODE_CLASS (mode) == MODE_CC;

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
      && regno >= REG_X
      // This problem only concerned the old reload.
      && ! avropt_lra_p)
    return false;

  /* All modes larger than 8 bits should start in an even register.  */

  return !(regno & 1);
}


/* Implement `TARGET_HARD_REGNO_CALL_PART_CLOBBERED'.  */

static bool
avr_hard_regno_call_part_clobbered (unsigned, unsigned regno,
				    machine_mode mode)
{
  /* FIXME: This hook gets called with MODE:REGNO combinations that don't
	represent valid hard registers like, e.g. HI:29.  Returning TRUE
	for such registers can lead to performance degradation as mentioned
	in PR53595.  Thus, report invalid hard registers as FALSE.  */

  if (!avr_hard_regno_mode_ok (regno, mode))
    return false;

  /* Return true if any of the following boundaries is crossed:
     17/18 or 19/20 (if AVR_TINY), 27/28 and 29/30.  */

  return ((regno <= LAST_CALLEE_SAVED_REG
	   && regno + GET_MODE_SIZE (mode) > 1 + LAST_CALLEE_SAVED_REG)
	  || (regno < REG_Y && regno + GET_MODE_SIZE (mode) > REG_Y)
	  || (regno < REG_Z && regno + GET_MODE_SIZE (mode) > REG_Z));
}


/* Implement `MODE_CODE_BASE_REG_CLASS'.  */

reg_class
avr_mode_code_base_reg_class (machine_mode /*mode*/, addr_space_t as,
			      rtx_code outer_code, rtx_code /*index_code*/)
{
  if (!ADDR_SPACE_GENERIC_P (as))
    {
      return POINTER_Z_REGS;
    }

  if (AVR_TINY)
    // We allow all offsets for all pointer regs.  Pass .avr-fuse-add
    // will rectify it (register allocation cannot do it).
    return POINTER_REGS;

  if (!avropt_strict_X)
    return reload_completed ? BASE_POINTER_REGS : POINTER_REGS;

  return PLUS == outer_code ? BASE_POINTER_REGS : POINTER_REGS;
}


/* Implement `REGNO_MODE_CODE_OK_FOR_BASE_P'.  */

bool
avr_regno_mode_code_ok_for_base_p (int regno, machine_mode /*mode*/,
				   addr_space_t as, rtx_code outer_code,
				   rtx_code /*index_code*/)
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

  if (avropt_strict_X
      // On Reduced Tiny, all registers are equal in that they do not
      // support PLUS addressing; respective addresses will be fake,
      // even for the frame pointer.  They must be handled in the
      // printers by add-store-sub sequences -- or may be split after
      // reload by `avr_split_tiny_move'.
      && ! AVR_TINY
      && PLUS == outer_code
      && regno == REG_X)
    {
      ok = false;
    }

  return ok;
}


/* Reload the constant OP[1] into the HI register OP[0].
   CLOBBER_REG is a QI clobber reg needed to move vast majority of consts
   into a NO_LD_REGS register.  If CLOBBER_REG is NULL_RTX we either don't
   need a clobber reg or have to cook one up.

   PLEN == NULL: Output instructions.
   PLEN != NULL: Output nothing.  Set *PLEN to number of words occupied
		 by the insns printed.

   Return "".  */

const char *
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
      /* In some cases it is better to clear the destination beforehand, e.g.

	     CLR R2   CLR R3   MOVW R4,R2   INC R2

	 is shorther than

	     CLR R2   INC R2   CLR  R3      CLR R4   CLR R5

	 We find it too tedious to work that out in the print function.
	 Instead, we call the print function twice to get the lengths of
	 both methods and use the shortest one.  */

      int len_clr, len_noclr;
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


const char *
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

  app_disable ();

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
      sec_name = ACONCAT ((sec_name, ".", fname, nullptr));

      fprintf (stream, "\t.section\t%s,\"%s\",@progbits\n", sec_name,
	       AVR_HAVE_JMP_CALL ? "a" : "ax");
    }

  // Output the label that precedes the table.

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

  in_section = nullptr;
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

      for (size_t i = REG_0; i <= REG_17;  i++)
	{
	  fixed_regs[i] = 1;
	  call_used_regs[i] = 1;
	}

      /* Set R18 to R21 as callee saved registers
	 - R18, R19, R20 and R21 are the callee saved registers in
	   Tiny Core devices  */

      for (size_t i = REG_18; i <= LAST_CALLEE_SAVED_REG; i++)
	{
	  call_used_regs[i] = 0;
	}

      /* Update register allocation order for Tiny Core devices */

      for (size_t i = 0; i < ARRAY_SIZE (tiny_reg_alloc_order); i++)
	{
	  reg_alloc_order[i] = tiny_reg_alloc_order[i];
	}

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

bool
avr_hard_regno_rename_ok (unsigned int old_reg, unsigned int new_reg)
{
  /* Interrupt functions can only use registers that have already been
     saved by the prologue, even if they would normally be
     call-clobbered.  */

  if ((cfun->machine->is_interrupt || cfun->machine->is_signal)
      && !df_regs_ever_live_p (new_reg))
    return false;

  /* Don't allow hard registers that might be part of the frame pointer.
     Some places in the compiler just test for [HARD_]FRAME_POINTER_REGNUM
     and don't care for a frame pointer that spans more than one register.  */

  if ((!reload_completed || frame_pointer_needed)
      && (old_reg == REG_Y || old_reg == REG_Y + 1
	  || new_reg == REG_Y || new_reg == REG_Y + 1))
    {
      return false;
    }

  return true;
}


/* Output a branch that tests a single bit of a register (QI, HI, SI or DImode)
   or memory location in the I/O space (QImode only).

   Operand 0: comparison operator (must be EQ or NE, compare bit to zero).
   Operand 1: register operand to test, or CONST_INT memory address.
   Operand 2: bit number.
   Operand 3: label to jump to if the test is true.  */

const char *
avr_out_sbxx_branch (rtx_insn *insn, rtx operands[])
{
  rtx_code comp = GET_CODE (operands[0]);
  bool long_jump = get_attr_length (insn) >= 4;
  bool reverse = long_jump || jump_over_one_insn_p (insn, operands[3]);

  // PR116953: jump_over_one_insn_p may call extract on the next insn,
  // clobbering recog_data.operand.  Thus, restore recog_data.
  extract_constrain_insn_cached (insn);

  if (comp == GE)
    comp = EQ;
  else if (comp == LT)
    comp = NE;

  if (reverse)
    comp = reverse_condition (comp);

  switch (GET_CODE (operands[1]))
    {
    default:
      gcc_unreachable ();

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
    } /* switch */

  if (long_jump)
    return ("rjmp .+4" CR_TAB
	    "jmp %x3");

  if (!reverse)
    return "rjmp %x3";

  return "";
}


/* Output code for  XOP[0] = sign_extract (XOP[1].0)  and return "".
   PLEN == 0: Output instructions.
   PLEN != 0: Set *PLEN to the length of the sequence in words.  */

const char *
avr_out_sextr (rtx_insn *insn, rtx *xop, int *plen)
{
  rtx dest = xop[0];
  rtx src = xop[1];
  int bit = INTVAL (xop[2]);
  int n_bytes = GET_MODE_SIZE (GET_MODE (dest));

  gcc_assert (bit == 0);

  if (reg_unused_after (insn, src))
    avr_asm_len ("lsr %1", xop, plen, -1);
  else
    avr_asm_len ("mov %0,%1"  CR_TAB
		 "lsr %0", xop, plen, -2);

  for (int i = 0; i < n_bytes; ++i)
    {
      rtx b = avr_byte (dest, i);
      avr_asm_len ("sbc %0,%0", &b, plen, 1);
      if (i == 1 && n_bytes == 4 && AVR_HAVE_MOVW)
	return avr_asm_len ("movw %C0,%A0", xop, plen, 1);
    }

  return "";
}


/*
   if (bits.bitno <eqne> 0)
     dest = op0;
   else
     dest = op0 <pix> op1;

   Performed as:

   dest = op0;
   if (bits.bitno <eqne> 0)
     goto LL;
   dest o= op1;
LL:;  */

void
avr_emit_skip_pixop (rtx_code pix, rtx dest, rtx op0, rtx op1,
		     rtx_code eqne, rtx bits, int bitno)
{
  gcc_assert (eqne == EQ);

  const machine_mode mode = GET_MODE (dest);

  // Get rid of early-clobbers.

  if (reg_overlap_mentioned_p (dest, bits))
    bits = copy_to_mode_reg (GET_MODE (bits), bits);

  if (reg_overlap_mentioned_p (dest, op1))
    op1 = copy_to_mode_reg (mode, op1);

  // xorqi3 has "register_operand" for op1.
  if (mode == QImode && pix == XOR)
    op1 = force_reg (QImode, op1);

  emit_move_insn (dest, op0);

  // Skip if bits.bitno <eqne> bitno.
  rtx xlabel = gen_label_rtx ();
  rtx zerox = gen_rtx_ZERO_EXTRACT (QImode, bits, const1_rtx, GEN_INT (bitno));
  rtx cond = gen_rtx_fmt_ee (eqne, VOIDmode, zerox, const0_rtx);
  emit (gen_sbrx_branchqi_split (cond, bits, const0_rtx, xlabel));

  // Payload: plus, ior, xor for HI, PSI, SI have a scratch:QI;
  // QI and plus:HI don't.
  rtx src = gen_rtx_fmt_ee (pix, mode, dest, op1);
  rtx set = gen_rtx_SET (dest, src);
  rtx clobber = gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (QImode));
  bool no_scratch = mode == QImode || (mode == HImode && pix == PLUS);
  emit (no_scratch
	? set
	: gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber)));

  emit_label (xlabel);
}


/*
   if (bits.bitno <eqne> 0)
     dest = src;
   else
     dest = 0;

   Performed as:

   dest = src;
   if (bits.bitno <eqne> 0)
     goto LL;
   dest = 0;
LL:;  */

void
avr_emit_skip_clear (rtx dest, rtx src, rtx_code eqne, rtx bits, int bitno)
{
  const machine_mode mode = GET_MODE (dest);

  // Get rid of early-clobber.
  if (reg_overlap_mentioned_p (dest, bits))
    bits = copy_to_mode_reg (GET_MODE (bits), bits);

  emit_move_insn (dest, src);

  // Skip if bits.bitno <eqne> bitno.
  rtx xlabel = gen_label_rtx ();
  rtx zerox = gen_rtx_ZERO_EXTRACT (QImode, bits, const1_rtx, GEN_INT (bitno));
  rtx cond = gen_rtx_fmt_ee (eqne, VOIDmode, zerox, const0_rtx);
  emit (gen_sbrx_branchqi_split (cond, bits, const0_rtx, xlabel));

  // Payload: dest = 0;
  emit_move_insn (dest, CONST0_RTX (mode));

  emit_label (xlabel);
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


/* Implement `TARGET_RETURN_IN_MEMORY'.  */

static bool
avr_return_in_memory (const_tree type, const_tree /*fntype*/)
{
  HOST_WIDE_INT size = int_size_in_bytes (type);
  HOST_WIDE_INT ret_size_limit = AVR_TINY ? 4 : 8;

  /* In avr, there are 8 return registers. But, for Tiny Core
     (ATtiny4/5/9/10/20/40) devices, only 4 registers are available.
     Return true if size is unknown or greater than the limit.  */

  return size == -1 || size > ret_size_limit;
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

  if (can_create_pseudo_p ()
      && REGNO (reg) < REG_Z)
    {
      return false;
    }

  return true;
}


/* Implement `TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P'.  */

static bool
avr_addr_space_legitimate_address_p (machine_mode mode, rtx x, bool strict,
				     addr_space_t as, code_helper = ERROR_MARK)
{
  bool ok = false;

  switch (as)
    {
    default:
      gcc_unreachable ();

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
    case ADDR_SPACE_FLASHX:
      if (REG_P (x))
	ok = (!strict
	      && can_create_pseudo_p ());

      if (LO_SUM == GET_CODE (x))
	{
	  rtx hi = XEXP (x, 0);
	  rtx lo = XEXP (x, 1);

	  ok = (REG_P (hi)
		&& (!strict || REGNO (hi) < FIRST_PSEUDO_REGISTER)
		&& REG_P (lo)
		&& REGNO (lo) == REG_Z);
	}

      break; /* MEMX, FLASHX */
    }

  if (avr_log.legitimate_address_p)
    {
      avr_edump ("\n%?: ret=%b, mode=%m strict=%d "
		 "reload_completed=%d ra_in_progress=%d %s:",
		 ok, mode, strict, reload_completed, ra_in_progress (),
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
avr_addr_space_convert (rtx src, tree type_old, tree type_new)
{
  addr_space_t as_old = TYPE_ADDR_SPACE (TREE_TYPE (type_old));
  addr_space_t as_new = TYPE_ADDR_SPACE (TREE_TYPE (type_new));
  int size_old = GET_MODE_SIZE (targetm.addr_space.pointer_mode (as_old));
  int size_new = GET_MODE_SIZE (targetm.addr_space.pointer_mode (as_new));

  if (avr_log.progmem)
    avr_edump ("\n%!: op = %r\nfrom = %t\nto = %t\n",
	       src, type_old, type_new);

  /* Up-casting from 16-bit to 24-bit pointer.  */

  if (size_old == 2 && size_new == 3)
    {
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
	  as_old = ADDR_SPACE_FLASH;
	}

      /* Linearize memory: RAM has bit 23 set.  When as_new = __flashx then
	 this is basically UB since __flashx mistreats RAM addresses, but there
	 is no way to bail out.  (Though -Waddr-space-convert will tell.)  */

      int msb = ADDR_SPACE_GENERIC_P (as_old)
	? 0x80
	: avr_addrspace[as_old].segment;

      src = force_reg (Pmode, src);

      emit_insn (msb == 0
		 ? gen_zero_extendhipsi2 (reg, src)
		 : gen_n_extendhipsi2 (reg, gen_int_mode (msb, QImode), src));

      return reg;
    }

  /* Down-casting from 24-bit to 16-bit throws away the high byte.  */

  if (size_old == 3 && size_new == 2)
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
avr_addr_space_subset_p (addr_space_t /*subset*/, addr_space_t /*superset*/)
{
  /* Allow any kind of pointer mess.  */

  return true;
}


/* Helps the next function.  */

static bool
avr_addr_space_contains (addr_space_t super, addr_space_t sub)
{
  return (super == sub
	  || super == ADDR_SPACE_MEMX
	  || (super == ADDR_SPACE_FLASHX
	      && sub != ADDR_SPACE_MEMX && ! ADDR_SPACE_GENERIC_P (sub)));
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

  if (avropt_warn_addr_space_convert
      && expr != error_mark_node
      && POINTER_TYPE_P (type)
      && POINTER_TYPE_P (TREE_TYPE (expr)))
    {
      addr_space_t as_old = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (expr)));
      addr_space_t as_new = TYPE_ADDR_SPACE (TREE_TYPE (type));

      if (avr_log.progmem)
	avr_edump ("%?: type = %t\nexpr = %t\n\n", type, expr);

      if (! avr_addr_space_contains (as_new, as_old))
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
	  if (hreg == nullptr)
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
  avr_fix_operands (op, nullptr, opmask, rmask);
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


/* Worker function for cpymemhi expander.
   XOP[0]  Destination as MEM:BLK
   XOP[1]  Source      "     "
   XOP[2]  # Bytes to copy

   Return TRUE  if the expansion is accomplished.
   Return FALSE if the operand compination is not supported.  */

bool
avr_emit_cpymemhi (rtx *xop)
{
  machine_mode loop_mode;
  addr_space_t as = MEM_ADDR_SPACE (xop[1]);
  rtx loop_reg, addr1, insn;
  rtx a_hi8 = NULL_RTX;

  if (avr_mem_flash_p (xop[0]))
    return false;

  if (!CONST_INT_P (xop[2]))
    return false;

  HOST_WIDE_INT count = INTVAL (xop[2]);
  if (count <= 0)
    return false;

  rtx a_src  = XEXP (xop[1], 0);
  rtx a_dest = XEXP (xop[0], 0);

  if (as == ADDR_SPACE_FLASHX
      && ! AVR_HAVE_ELPM)
    {
      a_src = copy_to_mode_reg (Pmode, avr_word (a_src, 0));
      as = ADDR_SPACE_FLASH;
    }

  machine_mode addr_mode = GET_MODE (a_src);

  if (addr_mode == PSImode)
    {
      gcc_assert (as == ADDR_SPACE_MEMX || as == ADDR_SPACE_FLASHX);

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
	  && avropt_n_flash > 1)
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

  rtx xas = GEN_INT (as);

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

  if (addr_mode == HImode)
    {
      /* Load instruction ([E]LPM or LD) is known at compile time:
	 Do the copy-loop inline.  */

      rtx (*fun) (rtx, rtx, rtx)
	= QImode == loop_mode ? gen_cpymem_qi : gen_cpymem_hi;

      insn = fun (xas, loop_reg, loop_reg);
    }
  else
    {
      rtx (*fun) (rtx, rtx)
	= QImode == loop_mode ? gen_cpymemx_qi : gen_cpymemx_hi;

      emit_move_insn (gen_rtx_REG (QImode, REG_23), a_hi8);

      insn = fun (xas, GEN_INT (avr_addr.rampz));
    }

  set_mem_addr_space (SET_SRC (XVECEXP (insn, 0, 0)), as);
  emit_insn (insn);

  return true;
}


/* Print assembler for cpymem_qi, cpymem_hi insns...
       $0     : Address Space
       $1, $2 : Loop register
       Z      : Source address
       X      : Destination address
*/

const char *
avr_out_cpymem (rtx_insn * /*insn*/, rtx *op, int *plen)
{
  addr_space_t as = (addr_space_t) INTVAL (op[0]);
  machine_mode loop_mode = GET_MODE (op[1]);
  bool sbiw_p = avr_adiw_reg_p (op[1]);
  rtx xop[3] = { op[0], op[1], tmp_reg_rtx };

  if (plen)
    *plen = 0;

  /* Loop label */

  avr_asm_len ("0:", xop, plen, 0);

  /* Load with post-increment */

  switch (as)
    {
    default:
      gcc_unreachable ();

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

  // Loop until zero.
  avr_asm_len ("brne 0b", xop, plen, 1);


  // Restore RAMPZ on EBI devices.
  if (as >= ADDR_SPACE_FLASH1
      && AVR_HAVE_ELPM && AVR_HAVE_RAMPD)
    avr_asm_len ("out %i0,__zero_reg__", &rampz_rtx, plen, 1);

  return "";
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
				     avr_mem_clobber ()));
      cycles -= cycles_used;
    }

  if (IN_RANGE (cycles, 262145, 83886081))
    {
      loop_count = ((cycles - 7) / 5) + 1;
      if (loop_count > 0xFFFFFF)
	loop_count = 0xFFFFFF;
      cycles_used = ((loop_count - 1) * 5) + 7;
      emit_insn (gen_delay_cycles_3 (gen_int_mode (loop_count, SImode),
				     avr_mem_clobber ()));
      cycles -= cycles_used;
    }

  if (IN_RANGE (cycles, 768, 262144))
    {
      loop_count = ((cycles - 5) / 4) + 1;
      if (loop_count > 0xFFFF)
	loop_count = 0xFFFF;
      cycles_used = ((loop_count - 1) * 4) + 5;
      emit_insn (gen_delay_cycles_2 (gen_int_mode (loop_count, HImode),
				     avr_mem_clobber ()));
      cycles -= cycles_used;
    }

  if (IN_RANGE (cycles, 6, 767))
    {
      loop_count = cycles / 3;
      if (loop_count > 255)
	loop_count = 255;
      cycles_used = loop_count * 3;
      emit_insn (gen_delay_cycles_1 (gen_int_mode (loop_count, QImode),
				     avr_mem_clobber ()));
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
	gcc_unreachable ();
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
  return avr_map_metric (map, MAP_MASK_PREIMAGE_F) != 0;
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
  tree_code code;

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
  bool val_used_p = avr_map_metric (f, MAP_MASK_PREIMAGE_F) != 0;
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

  if (avr_map_metric (f_ginv.map, MAP_NONFIXED_0_7) == 0)
    /* The mapping consists only of fixed points and can be folded
       to AND/OR logic in the remainder.  Reasonable cost is 3. */
    f_ginv.cost = 2 + (val_used_p && !val_const_p);
  else
    {
      rtx xop[4];

      /* Get the cost of the insn by calling the output worker with some
	 fake values.  Mimic effect of reloading xop[3]: Unused operands
	 are mapped to 0 and used operands are reloaded to xop[0].  */

      xop[0] = all_regs_rtx[REG_24];
      xop[1] = gen_int_mode (f_ginv.map, SImode);
      xop[2] = all_regs_rtx[REG_25];
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

const char *
avr_out_insert_bits (rtx *op, int *plen)
{
  unsigned int map = UINTVAL (op[1]) & GET_MODE_MASK (SImode);
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

  unsigned mask_fixed = avr_map_metric (map, MAP_MASK_FIXED_0_7);

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
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, CODE, LIBNAME, ATTRS) \
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

static GTY(()) avr_builtin_description
avr_bdesc[AVR_BUILTIN_COUNT] =
  {
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, ICODE, LIBNAME, ATTRS) \
    { (enum insn_code) CODE_FOR_ ## ICODE, N_ARGS, NULL_TREE },
#include "builtins.def"
#undef DEF_BUILTIN
  };


/* Implement `TARGET_BUILTIN_DECL'.  */

static tree
avr_builtin_decl (unsigned id, bool /*initialize_p*/)
{
  if (id < AVR_BUILTIN_COUNT)
    return avr_bdesc[id].fndecl;

  return error_mark_node;
}


static void
avr_init_builtin_int24 (void)
{
  for (int i = 0; i < NUM_INT_N_ENTS; ++i)
    if (int_n_data[i].bitsize == 24)
      {
	tree uint24_type = int_n_trees[i].unsigned_type;
	lang_hooks.types.register_builtin_type (uint24_type, "__uint24");
	break;
      }
}


/* Return a function signature type similar to strlen, but where
   the address is qualified by named address-space AS.  */

static tree
avr_ftype_strlen (addr_space_t as)
{
  tree const_AS_char_node
    = build_qualified_type (char_type_node,
			    TYPE_QUAL_CONST | ENCODE_QUAL_ADDR_SPACE (as));
  tree const_AS_ptr_type_node
    = build_pointer_type_for_mode (const_AS_char_node,
				   avr_addr_space_pointer_mode (as), false);
  tree size_ftype_const_AS_char_ptr
    = build_function_type_list (size_type_node, const_AS_ptr_type_node, NULL);

  return size_ftype_const_AS_char_ptr;
}


/* Implement `TARGET_INIT_BUILTINS' */
/* Set up all builtin functions for this target.  */

static void
avr_init_builtins (void)
{
  tree void_ftype_void
    = build_function_type_list (void_type_node, NULL_TREE);
  tree uintQI_ftype_uintQI
    = build_function_type_list (unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);
  tree uintQI_ftype_uintQI_uintQI
    = build_function_type_list (unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);
  tree uintHI_ftype_uintQI_uintQI
    = build_function_type_list (unsigned_intHI_type_node,
				unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);
  tree intHI_ftype_intQI_intQI
    = build_function_type_list (intHI_type_node,
				intQI_type_node,
				intQI_type_node,
				NULL_TREE);
  tree intHI_ftype_intQI_uintQI
    = build_function_type_list (intHI_type_node,
				intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);
  tree void_ftype_uintSI
    = build_function_type_list (void_type_node,
				unsigned_intSI_type_node,
				NULL_TREE);

  tree uintQI_ftype_uintSI_uintQI_uintQI
    = build_function_type_list (unsigned_intQI_type_node,
				unsigned_intSI_type_node,
				unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);

  tree const_memx_void_node
    = build_qualified_type (void_type_node,
			    TYPE_QUAL_CONST
			    | ENCODE_QUAL_ADDR_SPACE (ADDR_SPACE_MEMX));

  tree const_memx_ptr_type_node
    = build_pointer_type_for_mode (const_memx_void_node, PSImode, false);

  tree intQI_ftype_const_memx_ptr
    = build_function_type_list (intQI_type_node,
				const_memx_ptr_type_node,
				NULL);

  tree strlen_flash_node = avr_ftype_strlen (ADDR_SPACE_FLASH);
  tree strlen_flashx_node = avr_ftype_strlen (ADDR_SPACE_FLASHX);
  tree strlen_memx_node = avr_ftype_strlen (ADDR_SPACE_MEMX);

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

  tree attr_const = tree_cons (get_identifier ("const"), NULL, NULL);

#define DEF_BUILTIN(NAME, N_ARGS, TYPE, CODE, LIBNAME, ATTRS)		\
  {									\
    int id = AVR_BUILTIN_ ## NAME;					\
    const char *Name = "__builtin_avr_" #NAME;				\
    char *name = (char *) alloca (1 + strlen (Name));			\
									\
    gcc_assert (id < AVR_BUILTIN_COUNT);				\
    avr_bdesc[id].fndecl						\
      = add_builtin_function (avr_tolower (name, Name), TYPE, id,	\
			      BUILT_IN_MD, LIBNAME, ATTRS);		\
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
      gcc_unreachable ();
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
avr_expand_builtin (tree exp, rtx target, rtx /*subtarget*/,
		    machine_mode mode, int ignore)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  const char *bname = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  unsigned int id = DECL_MD_FUNCTION_CODE (fndecl);
  const avr_builtin_description *d = &avr_bdesc[id];
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

    case AVR_BUILTIN_MASK1:
      {
	arg0 = CALL_EXPR_ARG (exp, 0);
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
	int ival = CONST_INT_P (op0) ? 0xff & INTVAL (op0) : 0;

	if (ival != 0x01 && ival != 0x7f && ival != 0x80 && ival != 0xfe)
	  {
	    error ("%s expects a compile time integer constant of 0x01, "
		   "0x7f, 0x80 or 0xfe as first argument", bname);
	    return target;
	  }

	break;
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

  if (!ival.is_negative ())
    return tval;

  /* ISO/IEC TR 18037, 7.18a.6.2:  The absfx functions are saturating.  */

  fval.data = (ival == double_int::min_value (bits, false).sext (bits))
    ? double_int::max_value (bits, false)
    : -ival;

  return build_fixed (TREE_TYPE (tval), fval);
}


/* Implement `TARGET_FOLD_BUILTIN'.  */

static tree
avr_fold_builtin (tree fndecl, int /*n_args*/, tree *arg, bool /*ignore*/)
{
  unsigned int fcode = DECL_MD_FUNCTION_CODE (fndecl);
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

    case AVR_BUILTIN_STRLEN_FLASH:
    case AVR_BUILTIN_STRLEN_FLASHX:
    case AVR_BUILTIN_STRLEN_MEMX:
      if (tree len = c_strlen (arg[0], 0))
	return len;
      break;

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

    case AVR_BUILTIN_MASK1:
      {
	tree tmask = arg[0];
	tree toffs = arg[1];

	if (TREE_CODE (tmask) == INTEGER_CST
	    && TREE_CODE (toffs) == INTEGER_CST)
	  {
	    switch (0xff & TREE_INT_CST_LOW (tmask))
	      {
	      case 0x01:
	      case 0xfe:
		return fold_build2 (LROTATE_EXPR, val_type, arg[0], toffs);

	      case 0x80:
	      case 0x7f:
		return fold_build2 (RROTATE_EXPR, val_type, arg[0], toffs);
	      }
	  }
	break;
      } // AVR_BUILTIN_MASK1

    case AVR_BUILTIN_INSERT_BITS:
      {
	tree tbits = arg[1];
	tree tval = arg[2];
	tree map_type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
	bool changed = false;
	avr_map_op_t best_g;

	if (TREE_CODE (arg[0]) != INTEGER_CST)
	  {
	    /* No constant as first argument: Don't fold this and run into
	       error in avr_expand_builtin.  */

	    break;
	  }

	tree tmap = wide_int_to_tree (map_type, wi::to_wide (arg[0]));
	unsigned int map = TREE_INT_CST_LOW (tmap);

	if (TREE_CODE (tval) != INTEGER_CST
	    && avr_map_metric (map, MAP_MASK_PREIMAGE_F) == 0)
	  {
	    /* There are no F in the map, i.e. 3rd operand is unused.
	       Replace that argument with some constant to render
	       respective input unused.  */

	    tval = build_int_cst (val_type, 0);
	    changed = true;
	  }

	if (TREE_CODE (tbits) != INTEGER_CST
	    && avr_map_metric (map, MAP_PREIMAGE_0_7) == 0)
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
		    else		      mask_and &= ~(1 << i);
		  }
	      }

	    tval = fold_build2 (BIT_IOR_EXPR, val_type, tval,
				build_int_cst (val_type, mask_ior));
	    return fold_build2 (BIT_AND_EXPR, val_type, tval,
				build_int_cst (val_type, mask_and));
	  }

	if (changed)
	  return build_call_expr (fndecl, 3, tmap, tbits, tval);

	/* If bits don't change their position, we can use vanilla logic
	   to merge the two arguments...  */

	if (avr_map_metric (map, MAP_NONFIXED_0_7) == 0
	    // ...except when we are copying just one bit. In that
	    // case, BLD/BST is better than XOR/AND/XOR, see PR90622.
	    && avr_map_metric (map, MAP_FIXED_0_7) != 1)
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


/* Implement `TARGET_MD_ASM_ADJUST'.  */
/* Prepend to CLOBBERS hard registers that are automatically clobbered
   for an asm. We do this for CC_REGNUM to maintain source compatibility
   with the original cc0-based compiler.  */

static rtx_insn *
avr_md_asm_adjust (vec<rtx> & /*outputs*/, vec<rtx> & /*inputs*/,
		   vec<machine_mode> & /*input_modes*/,
		   vec<const char *> & /*constraints*/,
		   vec<rtx> & /*uses*/,
		   vec<rtx> &clobbers, HARD_REG_SET &clobbered_regs,
		   location_t /*loc*/)
{
  clobbers.safe_push (cc_reg_rtx);
  SET_HARD_REG_BIT (clobbered_regs, REG_CC);
  return nullptr;
}


/* Implement `TARGET_C_MODE_FOR_FLOATING_TYPE'.  Return SFmode or DFmode
   for TI_{LONG_,}DOUBLE_TYPE which is for {long,} double type, go with
   the default one for the others.  */

static machine_mode
avr_c_mode_for_floating_type (tree_index ti)
{
  if (ti == TI_DOUBLE_TYPE)
    return avropt_double == 32 ? SFmode : DFmode;
  if (ti == TI_LONG_DOUBLE_TYPE)
    return avropt_long_double == 32 ? SFmode : DFmode;
  return default_mode_for_floating_type (ti);
}


/* Worker function for `FLOAT_LIB_COMPARE_RETURNS_BOOL'.  */

bool
avr_float_lib_compare_returns_bool (machine_mode mode, rtx_code)
{
  if (mode == DFmode)
    {
#if WITH_DOUBLE_COMPARISON == 2
      return true;
#endif
    }

  // This is the GCC default and also what AVR-LibC implements.
  return false;
}


/* Implement `TARGET_UNWIND_WORD_MODE'.  */

static scalar_int_mode
avr_unwind_word_mode ()
{
  return Pmode;
}


/* Implement `TARGET_LRA_P'.  */

static bool
avr_use_lra_p ()
{
  return avropt_lra_p;
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

#undef  TARGET_INSN_COST
#define TARGET_INSN_COST avr_insn_cost
#undef  TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST avr_register_move_cost
#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST avr_memory_move_cost
#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS avr_rtx_costs
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST avr_address_cost
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

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE avr_conditional_register_usage

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS avr_hard_regno_nregs

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

#undef  TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS avr_class_max_nregs

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
#define TARGET_LRA_P avr_use_lra_p

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

#undef  TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID
#define TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID avr_addr_space_zero_address_valid

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

#undef  TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST avr_md_asm_adjust

#undef  TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P avr_can_inline_p

#undef  TARGET_CANONICALIZE_COMPARISON
#define TARGET_CANONICALIZE_COMPARISON avr_canonicalize_comparison

#undef  TARGET_UNWIND_WORD_MODE
#define TARGET_UNWIND_WORD_MODE avr_unwind_word_mode

/* According to the opening comment in PR86772, the following applies:
  "If the port does not (and never will in the future) need to mitigate
   against unsafe speculation."  */
#undef  TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

#undef TARGET_C_MODE_FOR_FLOATING_TYPE
#define TARGET_C_MODE_FOR_FLOATING_TYPE avr_c_mode_for_floating_type

gcc_target targetm = TARGET_INITIALIZER;


#include "gt-avr.h"
