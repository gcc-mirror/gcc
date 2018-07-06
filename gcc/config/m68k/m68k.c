/* Subroutines for insn-output.c for Motorola 68000 family.
   Copyright (C) 1987-2018 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "backend.h"
#include "cfghooks.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "rtl.h"
#include "df.h"
#include "alias.h"
#include "fold-const.h"
#include "calls.h"
#include "stor-layout.h"
#include "varasm.h"
#include "regs.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "stmt.h"
#include "expr.h"
#include "reload.h"
#include "tm_p.h"
#include "target.h"
#include "debug.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
/* ??? Need to add a dependency between m68k.o and sched-int.h.  */
#include "sched-int.h"
#include "insn-codes.h"
#include "opts.h"
#include "optabs.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "toplev.h"

/* This file should be included last.  */
#include "target-def.h"

enum reg_class regno_reg_class[] =
{
  DATA_REGS, DATA_REGS, DATA_REGS, DATA_REGS,
  DATA_REGS, DATA_REGS, DATA_REGS, DATA_REGS,
  ADDR_REGS, ADDR_REGS, ADDR_REGS, ADDR_REGS,
  ADDR_REGS, ADDR_REGS, ADDR_REGS, ADDR_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  ADDR_REGS
};


/* The minimum number of integer registers that we want to save with the
   movem instruction.  Using two movel instructions instead of a single
   moveml is about 15% faster for the 68020 and 68030 at no expense in
   code size.  */
#define MIN_MOVEM_REGS 3

/* The minimum number of floating point registers that we want to save
   with the fmovem instruction.  */
#define MIN_FMOVEM_REGS 1

/* Structure describing stack frame layout.  */
struct m68k_frame
{
  /* Stack pointer to frame pointer offset.  */
  HOST_WIDE_INT offset;

  /* Offset of FPU registers.  */
  HOST_WIDE_INT foffset;

  /* Frame size in bytes (rounded up).  */
  HOST_WIDE_INT size;

  /* Data and address register.  */
  int reg_no;
  unsigned int reg_mask;

  /* FPU registers.  */
  int fpu_no;
  unsigned int fpu_mask;

  /* Offsets relative to ARG_POINTER.  */
  HOST_WIDE_INT frame_pointer_offset;
  HOST_WIDE_INT stack_pointer_offset;

  /* Function which the above information refers to.  */
  int funcdef_no;
};

/* Current frame information calculated by m68k_compute_frame_layout().  */
static struct m68k_frame current_frame;

/* Structure describing an m68k address.

   If CODE is UNKNOWN, the address is BASE + INDEX * SCALE + OFFSET,
   with null fields evaluating to 0.  Here:

   - BASE satisfies m68k_legitimate_base_reg_p
   - INDEX satisfies m68k_legitimate_index_reg_p
   - OFFSET satisfies m68k_legitimate_constant_address_p

   INDEX is either HImode or SImode.  The other fields are SImode.

   If CODE is PRE_DEC, the address is -(BASE).  If CODE is POST_INC,
   the address is (BASE)+.  */
struct m68k_address {
  enum rtx_code code;
  rtx base;
  rtx index;
  rtx offset;
  int scale;
};

static int m68k_sched_adjust_cost (rtx_insn *, int, rtx_insn *, int,
				   unsigned int);
static int m68k_sched_issue_rate (void);
static int m68k_sched_variable_issue (FILE *, int, rtx_insn *, int);
static void m68k_sched_md_init_global (FILE *, int, int);
static void m68k_sched_md_finish_global (FILE *, int);
static void m68k_sched_md_init (FILE *, int, int);
static void m68k_sched_dfa_pre_advance_cycle (void);
static void m68k_sched_dfa_post_advance_cycle (void);
static int m68k_sched_first_cycle_multipass_dfa_lookahead (void);

static bool m68k_can_eliminate (const int, const int);
static void m68k_conditional_register_usage (void);
static bool m68k_legitimate_address_p (machine_mode, rtx, bool);
static void m68k_option_override (void);
static void m68k_override_options_after_change (void);
static rtx find_addr_reg (rtx);
static const char *singlemove_string (rtx *);
static void m68k_output_mi_thunk (FILE *, tree, HOST_WIDE_INT,
					  HOST_WIDE_INT, tree);
static rtx m68k_struct_value_rtx (tree, int);
static tree m68k_handle_fndecl_attribute (tree *node, tree name,
					  tree args, int flags,
					  bool *no_add_attrs);
static void m68k_compute_frame_layout (void);
static bool m68k_save_reg (unsigned int regno, bool interrupt_handler);
static bool m68k_ok_for_sibcall_p (tree, tree);
static bool m68k_tls_symbol_p (rtx);
static rtx m68k_legitimize_address (rtx, rtx, machine_mode);
static bool m68k_rtx_costs (rtx, machine_mode, int, int, int *, bool);
#if M68K_HONOR_TARGET_STRICT_ALIGNMENT
static bool m68k_return_in_memory (const_tree, const_tree);
#endif
static void m68k_output_dwarf_dtprel (FILE *, int, rtx) ATTRIBUTE_UNUSED;
static void m68k_trampoline_init (rtx, tree, rtx);
static poly_int64 m68k_return_pops_args (tree, tree, poly_int64);
static rtx m68k_delegitimize_address (rtx);
static void m68k_function_arg_advance (cumulative_args_t, machine_mode,
				       const_tree, bool);
static rtx m68k_function_arg (cumulative_args_t, machine_mode,
			      const_tree, bool);
static bool m68k_cannot_force_const_mem (machine_mode mode, rtx x);
static bool m68k_output_addr_const_extra (FILE *, rtx);
static void m68k_init_sync_libfuncs (void) ATTRIBUTE_UNUSED;
static enum flt_eval_method
m68k_excess_precision (enum excess_precision_type);
static unsigned int m68k_hard_regno_nregs (unsigned int, machine_mode);
static bool m68k_hard_regno_mode_ok (unsigned int, machine_mode);
static bool m68k_modes_tieable_p (machine_mode, machine_mode);
static machine_mode m68k_promote_function_mode (const_tree, machine_mode,
						int *, const_tree, int);

/* Initialize the GCC target structure.  */

#if INT_OP_GROUP == INT_OP_DOT_WORD
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#endif

#if INT_OP_GROUP == INT_OP_NO_DOT
#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\tbyte\t"
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\tshort\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\tlong\t"
#endif

#if INT_OP_GROUP == INT_OP_DC
#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\tdc.b\t"
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\tdc.w\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\tdc.l\t"
#endif

#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP TARGET_ASM_ALIGNED_HI_OP
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP TARGET_ASM_ALIGNED_SI_OP

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK m68k_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_ASM_FILE_START_APP_OFF
#define TARGET_ASM_FILE_START_APP_OFF true

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS m68k_legitimize_address

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST m68k_sched_adjust_cost

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE m68k_sched_issue_rate

#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE m68k_sched_variable_issue

#undef TARGET_SCHED_INIT_GLOBAL
#define TARGET_SCHED_INIT_GLOBAL m68k_sched_md_init_global

#undef TARGET_SCHED_FINISH_GLOBAL
#define TARGET_SCHED_FINISH_GLOBAL m68k_sched_md_finish_global

#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT m68k_sched_md_init

#undef TARGET_SCHED_DFA_PRE_ADVANCE_CYCLE
#define TARGET_SCHED_DFA_PRE_ADVANCE_CYCLE m68k_sched_dfa_pre_advance_cycle

#undef TARGET_SCHED_DFA_POST_ADVANCE_CYCLE
#define TARGET_SCHED_DFA_POST_ADVANCE_CYCLE m68k_sched_dfa_post_advance_cycle

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD	\
  m68k_sched_first_cycle_multipass_dfa_lookahead

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE m68k_option_override

#undef TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE m68k_override_options_after_change

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS m68k_rtx_costs

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE m68k_attribute_table

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX m68k_struct_value_rtx

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM m68k_cannot_force_const_mem

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL m68k_ok_for_sibcall_p

#if M68K_HONOR_TARGET_STRICT_ALIGNMENT
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY m68k_return_in_memory
#endif

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS (true)

#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL m68k_output_dwarf_dtprel
#endif

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	m68k_legitimate_address_p

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE m68k_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE m68k_conditional_register_usage

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT m68k_trampoline_init

#undef TARGET_RETURN_POPS_ARGS
#define TARGET_RETURN_POPS_ARGS m68k_return_pops_args

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS m68k_delegitimize_address

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG m68k_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE m68k_function_arg_advance

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P m68k_legitimate_constant_p

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA m68k_output_addr_const_extra

#undef TARGET_C_EXCESS_PRECISION
#define TARGET_C_EXCESS_PRECISION m68k_excess_precision

/* The value stored by TAS.  */
#undef TARGET_ATOMIC_TEST_AND_SET_TRUEVAL
#define TARGET_ATOMIC_TEST_AND_SET_TRUEVAL 128

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS m68k_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK m68k_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P m68k_modes_tieable_p

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE m68k_promote_function_mode

static const struct attribute_spec m68k_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "interrupt", 0, 0, true,  false, false, false,
    m68k_handle_fndecl_attribute, NULL },
  { "interrupt_handler", 0, 0, true,  false, false, false,
    m68k_handle_fndecl_attribute, NULL },
  { "interrupt_thread", 0, 0, true,  false, false, false,
    m68k_handle_fndecl_attribute, NULL },
  { NULL, 0, 0, false, false, false, false, NULL, NULL }
};

struct gcc_target targetm = TARGET_INITIALIZER;

/* Base flags for 68k ISAs.  */
#define FL_FOR_isa_00    FL_ISA_68000
#define FL_FOR_isa_10    (FL_FOR_isa_00 | FL_ISA_68010)
/* FL_68881 controls the default setting of -m68881.  gcc has traditionally
   generated 68881 code for 68020 and 68030 targets unless explicitly told
   not to.  */
#define FL_FOR_isa_20    (FL_FOR_isa_10 | FL_ISA_68020 \
			  | FL_BITFIELD | FL_68881 | FL_CAS)
#define FL_FOR_isa_40    (FL_FOR_isa_20 | FL_ISA_68040)
#define FL_FOR_isa_cpu32 (FL_FOR_isa_10 | FL_ISA_68020)

/* Base flags for ColdFire ISAs.  */
#define FL_FOR_isa_a     (FL_COLDFIRE | FL_ISA_A)
#define FL_FOR_isa_aplus (FL_FOR_isa_a | FL_ISA_APLUS | FL_CF_USP)
/* Note ISA_B doesn't necessarily include USP (user stack pointer) support.  */
#define FL_FOR_isa_b     (FL_FOR_isa_a | FL_ISA_B | FL_CF_HWDIV)
/* ISA_C is not upwardly compatible with ISA_B.  */
#define FL_FOR_isa_c     (FL_FOR_isa_a | FL_ISA_C | FL_CF_USP)

enum m68k_isa
{
  /* Traditional 68000 instruction sets.  */
  isa_00,
  isa_10,
  isa_20,
  isa_40,
  isa_cpu32,
  /* ColdFire instruction set variants.  */
  isa_a,
  isa_aplus,
  isa_b,
  isa_c,
  isa_max
};

/* Information about one of the -march, -mcpu or -mtune arguments.  */
struct m68k_target_selection
{
  /* The argument being described.  */
  const char *name;

  /* For -mcpu, this is the device selected by the option.
     For -mtune and -march, it is a representative device
     for the microarchitecture or ISA respectively.  */
  enum target_device device;

  /* The M68K_DEVICE fields associated with DEVICE.  See the comment
     in m68k-devices.def for details.  FAMILY is only valid for -mcpu.  */
  const char *family;
  enum uarch_type microarch;
  enum m68k_isa isa;
  unsigned long flags;
};

/* A list of all devices in m68k-devices.def.  Used for -mcpu selection.  */
static const struct m68k_target_selection all_devices[] =
{
#define M68K_DEVICE(NAME,ENUM_VALUE,FAMILY,MULTILIB,MICROARCH,ISA,FLAGS) \
  { NAME, ENUM_VALUE, FAMILY, u##MICROARCH, ISA, FLAGS | FL_FOR_##ISA },
#include "m68k-devices.def"
#undef M68K_DEVICE
  { NULL, unk_device, NULL, unk_arch, isa_max, 0 }
};

/* A list of all ISAs, mapping each one to a representative device.
   Used for -march selection.  */
static const struct m68k_target_selection all_isas[] =
{
#define M68K_ISA(NAME,DEVICE,MICROARCH,ISA,FLAGS) \
  { NAME, DEVICE, NULL, u##MICROARCH, ISA, FLAGS },
#include "m68k-isas.def"
#undef M68K_ISA
  { NULL,       unk_device, NULL,  unk_arch, isa_max,   0 }
};

/* A list of all microarchitectures, mapping each one to a representative
   device.  Used for -mtune selection.  */
static const struct m68k_target_selection all_microarchs[] =
{
#define M68K_MICROARCH(NAME,DEVICE,MICROARCH,ISA,FLAGS) \
  { NAME, DEVICE, NULL, u##MICROARCH, ISA, FLAGS },
#include "m68k-microarchs.def"
#undef M68K_MICROARCH
  { NULL,       unk_device, NULL,  unk_arch,  isa_max, 0 }
};

/* The entries associated with the -mcpu, -march and -mtune settings,
   or null for options that have not been used.  */
const struct m68k_target_selection *m68k_cpu_entry;
const struct m68k_target_selection *m68k_arch_entry;
const struct m68k_target_selection *m68k_tune_entry;

/* Which CPU we are generating code for.  */
enum target_device m68k_cpu;

/* Which microarchitecture to tune for.  */
enum uarch_type m68k_tune;

/* Which FPU to use.  */
enum fpu_type m68k_fpu;

/* The set of FL_* flags that apply to the target processor.  */
unsigned int m68k_cpu_flags;

/* The set of FL_* flags that apply to the processor to be tuned for.  */
unsigned int m68k_tune_flags;

/* Asm templates for calling or jumping to an arbitrary symbolic address,
   or NULL if such calls or jumps are not supported.  The address is held
   in operand 0.  */
const char *m68k_symbolic_call;
const char *m68k_symbolic_jump;

/* Enum variable that corresponds to m68k_symbolic_call values.  */
enum M68K_SYMBOLIC_CALL m68k_symbolic_call_var;


/* Implement TARGET_OPTION_OVERRIDE.  */

static void
m68k_option_override (void)
{
  const struct m68k_target_selection *entry;
  unsigned long target_mask;

  if (global_options_set.x_m68k_arch_option)
    m68k_arch_entry = &all_isas[m68k_arch_option];

  if (global_options_set.x_m68k_cpu_option)
    m68k_cpu_entry = &all_devices[(int) m68k_cpu_option];

  if (global_options_set.x_m68k_tune_option)
    m68k_tune_entry = &all_microarchs[(int) m68k_tune_option];

  /* User can choose:

     -mcpu=
     -march=
     -mtune=

     -march=ARCH should generate code that runs any processor
     implementing architecture ARCH.  -mcpu=CPU should override -march
     and should generate code that runs on processor CPU, making free
     use of any instructions that CPU understands.  -mtune=UARCH applies
     on top of -mcpu or -march and optimizes the code for UARCH.  It does
     not change the target architecture.  */
  if (m68k_cpu_entry)
    {
      /* Complain if the -march setting is for a different microarchitecture,
	 or includes flags that the -mcpu setting doesn't.  */
      if (m68k_arch_entry
	  && (m68k_arch_entry->microarch != m68k_cpu_entry->microarch
	      || (m68k_arch_entry->flags & ~m68k_cpu_entry->flags) != 0))
	warning (0, "-mcpu=%s conflicts with -march=%s",
		 m68k_cpu_entry->name, m68k_arch_entry->name);

      entry = m68k_cpu_entry;
    }
  else
    entry = m68k_arch_entry;

  if (!entry)
    entry = all_devices + TARGET_CPU_DEFAULT;

  m68k_cpu_flags = entry->flags;

  /* Use the architecture setting to derive default values for
     certain flags.  */
  target_mask = 0;

  /* ColdFire is lenient about alignment.  */
  if (!TARGET_COLDFIRE)
    target_mask |= MASK_STRICT_ALIGNMENT;

  if ((m68k_cpu_flags & FL_BITFIELD) != 0)
    target_mask |= MASK_BITFIELD;
  if ((m68k_cpu_flags & FL_CF_HWDIV) != 0)
    target_mask |= MASK_CF_HWDIV;
  if ((m68k_cpu_flags & (FL_68881 | FL_CF_FPU)) != 0)
    target_mask |= MASK_HARD_FLOAT;
  target_flags |= target_mask & ~target_flags_explicit;

  /* Set the directly-usable versions of the -mcpu and -mtune settings.  */
  m68k_cpu = entry->device;
  if (m68k_tune_entry)
    {
      m68k_tune = m68k_tune_entry->microarch;
      m68k_tune_flags = m68k_tune_entry->flags;
    }
#ifdef M68K_DEFAULT_TUNE
  else if (!m68k_cpu_entry && !m68k_arch_entry)
    {
      enum target_device dev;
      dev = all_microarchs[M68K_DEFAULT_TUNE].device;
      m68k_tune_flags = all_devices[dev].flags;
    }
#endif
  else
    {
      m68k_tune = entry->microarch;
      m68k_tune_flags = entry->flags;
    }

  /* Set the type of FPU.  */
  m68k_fpu = (!TARGET_HARD_FLOAT ? FPUTYPE_NONE
	      : (m68k_cpu_flags & FL_COLDFIRE) != 0 ? FPUTYPE_COLDFIRE
	      : FPUTYPE_68881);

  /* Sanity check to ensure that msep-data and mid-sahred-library are not
   * both specified together.  Doing so simply doesn't make sense.
   */
  if (TARGET_SEP_DATA && TARGET_ID_SHARED_LIBRARY)
    error ("cannot specify both -msep-data and -mid-shared-library");

  /* If we're generating code for a separate A5 relative data segment,
   * we've got to enable -fPIC as well.  This might be relaxable to
   * -fpic but it hasn't been tested properly.
   */
  if (TARGET_SEP_DATA || TARGET_ID_SHARED_LIBRARY)
    flag_pic = 2;

  /* -mpcrel -fPIC uses 32-bit pc-relative displacements.  Raise an
     error if the target does not support them.  */
  if (TARGET_PCREL && !TARGET_68020 && flag_pic == 2)
    error ("-mpcrel -fPIC is not currently supported on selected cpu");

  /* ??? A historic way of turning on pic, or is this intended to
     be an embedded thing that doesn't have the same name binding
     significance that it does on hosted ELF systems?  */
  if (TARGET_PCREL && flag_pic == 0)
    flag_pic = 1;

  if (!flag_pic)
    {
      m68k_symbolic_call_var = M68K_SYMBOLIC_CALL_JSR;

      m68k_symbolic_jump = "jra %a0";
    }
  else if (TARGET_ID_SHARED_LIBRARY)
    /* All addresses must be loaded from the GOT.  */
    ;
  else if (TARGET_68020 || TARGET_ISAB || TARGET_ISAC)
    {
      if (TARGET_PCREL)
	m68k_symbolic_call_var = M68K_SYMBOLIC_CALL_BSR_C;
      else
	m68k_symbolic_call_var = M68K_SYMBOLIC_CALL_BSR_P;

      if (TARGET_ISAC)
	/* No unconditional long branch */;
      else if (TARGET_PCREL)
	m68k_symbolic_jump = "bra%.l %c0";
      else
	m68k_symbolic_jump = "bra%.l %p0";
      /* Turn off function cse if we are doing PIC.  We always want
	 function call to be done as `bsr foo@PLTPC'.  */
      /* ??? It's traditional to do this for -mpcrel too, but it isn't
	 clear how intentional that is.  */
      flag_no_function_cse = 1;
    }

  switch (m68k_symbolic_call_var)
    {
    case M68K_SYMBOLIC_CALL_JSR:
      m68k_symbolic_call = "jsr %a0";
      break;

    case M68K_SYMBOLIC_CALL_BSR_C:
      m68k_symbolic_call = "bsr%.l %c0";
      break;

    case M68K_SYMBOLIC_CALL_BSR_P:
      m68k_symbolic_call = "bsr%.l %p0";
      break;

    case M68K_SYMBOLIC_CALL_NONE:
      gcc_assert (m68k_symbolic_call == NULL);
      break;

    default:
      gcc_unreachable ();
    }

#ifndef ASM_OUTPUT_ALIGN_WITH_NOP
  parse_alignment_opts ();
  if (align_labels_value > 2)
    {
      warning (0, "-falign-labels=%d is not supported", align_labels_value);
      str_align_labels = "1";
    }

  if (align_loops_value > 2)
    {
      warning (0, "-falign-loops=%d is not supported", align_loops_value);
      str_align_loops = "1";
    }
#endif

  if ((opt_fstack_limit_symbol_arg != NULL || opt_fstack_limit_register_no >= 0)
      && !TARGET_68020)
    {
      warning (0, "-fstack-limit- options are not supported on this cpu");
      opt_fstack_limit_symbol_arg = NULL;
      opt_fstack_limit_register_no = -1;
    }

  SUBTARGET_OVERRIDE_OPTIONS;

  /* Setup scheduling options.  */
  if (TUNE_CFV1)
    m68k_sched_cpu = CPU_CFV1;
  else if (TUNE_CFV2)
    m68k_sched_cpu = CPU_CFV2;
  else if (TUNE_CFV3)
    m68k_sched_cpu = CPU_CFV3;
  else if (TUNE_CFV4)
    m68k_sched_cpu = CPU_CFV4;
  else
    {
      m68k_sched_cpu = CPU_UNKNOWN;
      flag_schedule_insns = 0;
      flag_schedule_insns_after_reload = 0;
      flag_modulo_sched = 0;
      flag_live_range_shrinkage = 0;
    }

  if (m68k_sched_cpu != CPU_UNKNOWN)
    {
      if ((m68k_cpu_flags & (FL_CF_EMAC | FL_CF_EMAC_B)) != 0)
	m68k_sched_mac = MAC_CF_EMAC;
      else if ((m68k_cpu_flags & FL_CF_MAC) != 0)
	m68k_sched_mac = MAC_CF_MAC;
      else
	m68k_sched_mac = MAC_NO;
    }
}

/* Implement TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE.  */

static void
m68k_override_options_after_change (void)
{
  if (m68k_sched_cpu == CPU_UNKNOWN)
    {
      flag_schedule_insns = 0;
      flag_schedule_insns_after_reload = 0;
      flag_modulo_sched = 0;
      flag_live_range_shrinkage = 0;
    }
}

/* Generate a macro of the form __mPREFIX_cpu_NAME, where PREFIX is the
   given argument and NAME is the argument passed to -mcpu.  Return NULL
   if -mcpu was not passed.  */

const char *
m68k_cpp_cpu_ident (const char *prefix)
{
  if (!m68k_cpu_entry)
    return NULL;
  return concat ("__m", prefix, "_cpu_", m68k_cpu_entry->name, NULL);
}

/* Generate a macro of the form __mPREFIX_family_NAME, where PREFIX is the
   given argument and NAME is the name of the representative device for
   the -mcpu argument's family.  Return NULL if -mcpu was not passed.  */

const char *
m68k_cpp_cpu_family (const char *prefix)
{
  if (!m68k_cpu_entry)
    return NULL;
  return concat ("__m", prefix, "_family_", m68k_cpu_entry->family, NULL);
}

/* Return m68k_fk_interrupt_handler if FUNC has an "interrupt" or
   "interrupt_handler" attribute and interrupt_thread if FUNC has an
   "interrupt_thread" attribute.  Otherwise, return
   m68k_fk_normal_function.  */

enum m68k_function_kind
m68k_get_function_kind (tree func)
{
  tree a;

  gcc_assert (TREE_CODE (func) == FUNCTION_DECL);
  
  a = lookup_attribute ("interrupt", DECL_ATTRIBUTES (func));
  if (a != NULL_TREE)
    return m68k_fk_interrupt_handler;

  a = lookup_attribute ("interrupt_handler", DECL_ATTRIBUTES (func));
  if (a != NULL_TREE)
    return m68k_fk_interrupt_handler;

  a = lookup_attribute ("interrupt_thread", DECL_ATTRIBUTES (func));
  if (a != NULL_TREE)
    return m68k_fk_interrupt_thread;

  return m68k_fk_normal_function;
}

/* Handle an attribute requiring a FUNCTION_DECL; arguments as in
   struct attribute_spec.handler.  */
static tree
m68k_handle_fndecl_attribute (tree *node, tree name,
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

  if (m68k_get_function_kind (*node) != m68k_fk_normal_function)
    {
      error ("multiple interrupt attributes not allowed");
      *no_add_attrs = true;
    }

  if (!TARGET_FIDOA
      && !strcmp (IDENTIFIER_POINTER (name), "interrupt_thread"))
    {
      error ("interrupt_thread is available only on fido");
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static void
m68k_compute_frame_layout (void)
{
  int regno, saved;
  unsigned int mask;
  enum m68k_function_kind func_kind =
    m68k_get_function_kind (current_function_decl);
  bool interrupt_handler = func_kind == m68k_fk_interrupt_handler;
  bool interrupt_thread = func_kind == m68k_fk_interrupt_thread;

  /* Only compute the frame once per function.
     Don't cache information until reload has been completed.  */
  if (current_frame.funcdef_no == current_function_funcdef_no
      && reload_completed)
    return;

  current_frame.size = (get_frame_size () + 3) & -4;

  mask = saved = 0;

  /* Interrupt thread does not need to save any register.  */
  if (!interrupt_thread)
    for (regno = 0; regno < 16; regno++)
      if (m68k_save_reg (regno, interrupt_handler))
	{
	  mask |= 1 << (regno - D0_REG);
	  saved++;
	}
  current_frame.offset = saved * 4;
  current_frame.reg_no = saved;
  current_frame.reg_mask = mask;

  current_frame.foffset = 0;
  mask = saved = 0;
  if (TARGET_HARD_FLOAT)
    {
      /* Interrupt thread does not need to save any register.  */
      if (!interrupt_thread)
	for (regno = 16; regno < 24; regno++)
	  if (m68k_save_reg (regno, interrupt_handler))
	    {
	      mask |= 1 << (regno - FP0_REG);
	      saved++;
	    }
      current_frame.foffset = saved * TARGET_FP_REG_SIZE;
      current_frame.offset += current_frame.foffset;
    }
  current_frame.fpu_no = saved;
  current_frame.fpu_mask = mask;

  /* Remember what function this frame refers to.  */
  current_frame.funcdef_no = current_function_funcdef_no;
}

/* Worker function for TARGET_CAN_ELIMINATE.  */

bool
m68k_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == STACK_POINTER_REGNUM ? ! frame_pointer_needed : true);
}

HOST_WIDE_INT
m68k_initial_elimination_offset (int from, int to)
{
  int argptr_offset;
  /* The arg pointer points 8 bytes before the start of the arguments,
     as defined by FIRST_PARM_OFFSET.  This makes it coincident with the
     frame pointer in most frames.  */
  argptr_offset = frame_pointer_needed ? 0 : UNITS_PER_WORD;
  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    return argptr_offset;

  m68k_compute_frame_layout ();

  gcc_assert (to == STACK_POINTER_REGNUM);
  switch (from)
    {
    case ARG_POINTER_REGNUM:
      return current_frame.offset + current_frame.size - argptr_offset;
    case FRAME_POINTER_REGNUM:
      return current_frame.offset + current_frame.size;
    default:
      gcc_unreachable ();
    }
}

/* Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This function is responsible for
   knowing which registers should not be saved even if used.
   Return true if we need to save REGNO.  */

static bool
m68k_save_reg (unsigned int regno, bool interrupt_handler)
{
  if (flag_pic && regno == PIC_REG)
    {
      if (crtl->saves_all_registers)
	return true;
      if (crtl->uses_pic_offset_table)
	return true;
      /* Reload may introduce constant pool references into a function
	 that thitherto didn't need a PIC register.  Note that the test
	 above will not catch that case because we will only set
	 crtl->uses_pic_offset_table when emitting
	 the address reloads.  */
      if (crtl->uses_const_pool)
	return true;
    }

  if (crtl->calls_eh_return)
    {
      unsigned int i;
      for (i = 0; ; i++)
	{
	  unsigned int test = EH_RETURN_DATA_REGNO (i);
	  if (test == INVALID_REGNUM)
	    break;
	  if (test == regno)
	    return true;
	}
    }

  /* Fixed regs we never touch.  */
  if (fixed_regs[regno])
    return false;

  /* The frame pointer (if it is such) is handled specially.  */
  if (regno == FRAME_POINTER_REGNUM && frame_pointer_needed)
    return false;

  /* Interrupt handlers must also save call_used_regs
     if they are live or when calling nested functions.  */
  if (interrupt_handler)
    {
      if (df_regs_ever_live_p (regno))
	return true;

      if (!crtl->is_leaf && call_used_regs[regno])
	return true;
    }

  /* Never need to save registers that aren't touched.  */
  if (!df_regs_ever_live_p (regno))
    return false;

  /* Otherwise save everything that isn't call-clobbered.  */
  return !call_used_regs[regno];
}

/* Emit RTL for a MOVEM or FMOVEM instruction.  BASE + OFFSET represents
   the lowest memory address.  COUNT is the number of registers to be
   moved, with register REGNO + I being moved if bit I of MASK is set.
   STORE_P specifies the direction of the move and ADJUST_STACK_P says
   whether or not this is pre-decrement (if STORE_P) or post-increment
   (if !STORE_P) operation.  */

static rtx_insn *
m68k_emit_movem (rtx base, HOST_WIDE_INT offset,
		 unsigned int count, unsigned int regno,
		 unsigned int mask, bool store_p, bool adjust_stack_p)
{
  int i;
  rtx body, addr, src, operands[2];
  machine_mode mode;

  body = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (adjust_stack_p + count));
  mode = reg_raw_mode[regno];
  i = 0;

  if (adjust_stack_p)
    {
      src = plus_constant (Pmode, base,
			   (count
			    * GET_MODE_SIZE (mode)
			    * (HOST_WIDE_INT) (store_p ? -1 : 1)));
      XVECEXP (body, 0, i++) = gen_rtx_SET (base, src);
    }

  for (; mask != 0; mask >>= 1, regno++)
    if (mask & 1)
      {
	addr = plus_constant (Pmode, base, offset);
	operands[!store_p] = gen_frame_mem (mode, addr);
	operands[store_p] = gen_rtx_REG (mode, regno);
	XVECEXP (body, 0, i++)
	  = gen_rtx_SET (operands[0], operands[1]);
	offset += GET_MODE_SIZE (mode);
      }
  gcc_assert (i == XVECLEN (body, 0));

  return emit_insn (body);
}

/* Make INSN a frame-related instruction.  */

static void
m68k_set_frame_related (rtx_insn *insn)
{
  rtx body;
  int i;

  RTX_FRAME_RELATED_P (insn) = 1;
  body = PATTERN (insn);
  if (GET_CODE (body) == PARALLEL)
    for (i = 0; i < XVECLEN (body, 0); i++)
      RTX_FRAME_RELATED_P (XVECEXP (body, 0, i)) = 1;
}

/* Emit RTL for the "prologue" define_expand.  */

void
m68k_expand_prologue (void)
{
  HOST_WIDE_INT fsize_with_regs;
  rtx limit, src, dest;

  m68k_compute_frame_layout ();

  if (flag_stack_usage_info)
    current_function_static_stack_size
      = current_frame.size + current_frame.offset;

  /* If the stack limit is a symbol, we can check it here,
     before actually allocating the space.  */
  if (crtl->limit_stack
      && GET_CODE (stack_limit_rtx) == SYMBOL_REF)
    {
      limit = plus_constant (Pmode, stack_limit_rtx, current_frame.size + 4);
      if (!m68k_legitimate_constant_p (Pmode, limit))
	{
	  emit_move_insn (gen_rtx_REG (Pmode, D0_REG), limit);
	  limit = gen_rtx_REG (Pmode, D0_REG);
	}
      emit_insn (gen_ctrapsi4 (gen_rtx_LTU (VOIDmode,
					    stack_pointer_rtx, limit),
			       stack_pointer_rtx, limit,
			       const1_rtx));
    }

  fsize_with_regs = current_frame.size;
  if (TARGET_COLDFIRE)
    {
      /* ColdFire's move multiple instructions do not allow pre-decrement
	 addressing.  Add the size of movem saves to the initial stack
	 allocation instead.  */
      if (current_frame.reg_no >= MIN_MOVEM_REGS)
	fsize_with_regs += current_frame.reg_no * GET_MODE_SIZE (SImode);
      if (current_frame.fpu_no >= MIN_FMOVEM_REGS)
	fsize_with_regs += current_frame.fpu_no * GET_MODE_SIZE (DFmode);
    }

  if (frame_pointer_needed)
    {
      if (fsize_with_regs == 0 && TUNE_68040)
	{
	  /* On the 68040, two separate moves are faster than link.w 0.  */
	  dest = gen_frame_mem (Pmode,
				gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx));
	  m68k_set_frame_related (emit_move_insn (dest, frame_pointer_rtx));
	  m68k_set_frame_related (emit_move_insn (frame_pointer_rtx,
						  stack_pointer_rtx));
	}
      else if (fsize_with_regs < 0x8000 || TARGET_68020)
	m68k_set_frame_related
	  (emit_insn (gen_link (frame_pointer_rtx,
				GEN_INT (-4 - fsize_with_regs))));
      else
 	{
	  m68k_set_frame_related
	    (emit_insn (gen_link (frame_pointer_rtx, GEN_INT (-4))));
	  m68k_set_frame_related
	    (emit_insn (gen_addsi3 (stack_pointer_rtx,
				    stack_pointer_rtx,
				    GEN_INT (-fsize_with_regs))));
	}

      /* If the frame pointer is needed, emit a special barrier that
	 will prevent the scheduler from moving stores to the frame
	 before the stack adjustment.  */
      emit_insn (gen_stack_tie (stack_pointer_rtx, frame_pointer_rtx));
    }
  else if (fsize_with_regs != 0)
    m68k_set_frame_related
      (emit_insn (gen_addsi3 (stack_pointer_rtx,
			      stack_pointer_rtx,
			      GEN_INT (-fsize_with_regs))));

  if (current_frame.fpu_mask)
    {
      gcc_assert (current_frame.fpu_no >= MIN_FMOVEM_REGS);
      if (TARGET_68881)
	m68k_set_frame_related
	  (m68k_emit_movem (stack_pointer_rtx,
			    current_frame.fpu_no * -GET_MODE_SIZE (XFmode),
			    current_frame.fpu_no, FP0_REG,
			    current_frame.fpu_mask, true, true));
      else
	{
	  int offset;

	  /* If we're using moveml to save the integer registers,
	     the stack pointer will point to the bottom of the moveml
	     save area.  Find the stack offset of the first FP register.  */
	  if (current_frame.reg_no < MIN_MOVEM_REGS)
	    offset = 0;
	  else
	    offset = current_frame.reg_no * GET_MODE_SIZE (SImode);
	  m68k_set_frame_related
	    (m68k_emit_movem (stack_pointer_rtx, offset,
			      current_frame.fpu_no, FP0_REG,
			      current_frame.fpu_mask, true, false));
	}
    }

  /* If the stack limit is not a symbol, check it here.
     This has the disadvantage that it may be too late...  */
  if (crtl->limit_stack)
    {
      if (REG_P (stack_limit_rtx))
        emit_insn (gen_ctrapsi4 (gen_rtx_LTU (VOIDmode, stack_pointer_rtx,
					      stack_limit_rtx),
			         stack_pointer_rtx, stack_limit_rtx,
			         const1_rtx));

      else if (GET_CODE (stack_limit_rtx) != SYMBOL_REF)
	warning (0, "stack limit expression is not supported");
    }

  if (current_frame.reg_no < MIN_MOVEM_REGS)
    {
      /* Store each register separately in the same order moveml does.  */
      int i;

      for (i = 16; i-- > 0; )
	if (current_frame.reg_mask & (1 << i))
	  {
	    src = gen_rtx_REG (SImode, D0_REG + i);
	    dest = gen_frame_mem (SImode,
				  gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx));
	    m68k_set_frame_related (emit_insn (gen_movsi (dest, src)));
	  }
    }
  else
    {
      if (TARGET_COLDFIRE)
	/* The required register save space has already been allocated.
	   The first register should be stored at (%sp).  */
	m68k_set_frame_related
	  (m68k_emit_movem (stack_pointer_rtx, 0,
			    current_frame.reg_no, D0_REG,
			    current_frame.reg_mask, true, false));
      else
	m68k_set_frame_related
	  (m68k_emit_movem (stack_pointer_rtx,
			    current_frame.reg_no * -GET_MODE_SIZE (SImode),
			    current_frame.reg_no, D0_REG,
			    current_frame.reg_mask, true, true));
    }

  if (!TARGET_SEP_DATA
      && crtl->uses_pic_offset_table)
    emit_insn (gen_load_got (pic_offset_table_rtx));
}

/* Return true if a simple (return) instruction is sufficient for this
   instruction (i.e. if no epilogue is needed).  */

bool
m68k_use_return_insn (void)
{
  if (!reload_completed || frame_pointer_needed || get_frame_size () != 0)
    return false;

  m68k_compute_frame_layout ();
  return current_frame.offset == 0;
}

/* Emit RTL for the "epilogue" or "sibcall_epilogue" define_expand;
   SIBCALL_P says which.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only, if there is a frame pointer.
   This is mandatory because of alloca; we also take advantage of it to
   omit stack adjustments before returning.  */

void
m68k_expand_epilogue (bool sibcall_p)
{
  HOST_WIDE_INT fsize, fsize_with_regs;
  bool big, restore_from_sp;

  m68k_compute_frame_layout ();

  fsize = current_frame.size;
  big = false;
  restore_from_sp = false;

  /* FIXME : crtl->is_leaf below is too strong.
     What we really need to know there is if there could be pending
     stack adjustment needed at that point.  */
  restore_from_sp = (!frame_pointer_needed
		     || (!cfun->calls_alloca && crtl->is_leaf));

  /* fsize_with_regs is the size we need to adjust the sp when
     popping the frame.  */
  fsize_with_regs = fsize;
  if (TARGET_COLDFIRE && restore_from_sp)
    {
      /* ColdFire's move multiple instructions do not allow post-increment
	 addressing.  Add the size of movem loads to the final deallocation
	 instead.  */
      if (current_frame.reg_no >= MIN_MOVEM_REGS)
	fsize_with_regs += current_frame.reg_no * GET_MODE_SIZE (SImode);
      if (current_frame.fpu_no >= MIN_FMOVEM_REGS)
	fsize_with_regs += current_frame.fpu_no * GET_MODE_SIZE (DFmode);
    }

  if (current_frame.offset + fsize >= 0x8000
      && !restore_from_sp
      && (current_frame.reg_mask || current_frame.fpu_mask))
    {
      if (TARGET_COLDFIRE
	  && (current_frame.reg_no >= MIN_MOVEM_REGS
	      || current_frame.fpu_no >= MIN_FMOVEM_REGS))
	{
	  /* ColdFire's move multiple instructions do not support the
	     (d8,Ax,Xi) addressing mode, so we're as well using a normal
	     stack-based restore.  */
	  emit_move_insn (gen_rtx_REG (Pmode, A1_REG),
			  GEN_INT (-(current_frame.offset + fsize)));
	  emit_insn (gen_blockage ());
	  emit_insn (gen_addsi3 (stack_pointer_rtx,
				 gen_rtx_REG (Pmode, A1_REG),
				 frame_pointer_rtx));
	  restore_from_sp = true;
	}
      else
	{
	  emit_move_insn (gen_rtx_REG (Pmode, A1_REG), GEN_INT (-fsize));
	  fsize = 0;
	  big = true;
	}
    }

  if (current_frame.reg_no < MIN_MOVEM_REGS)
    {
      /* Restore each register separately in the same order moveml does.  */
      int i;
      HOST_WIDE_INT offset;

      offset = current_frame.offset + fsize;
      for (i = 0; i < 16; i++)
        if (current_frame.reg_mask & (1 << i))
          {
	    rtx addr;

	    if (big)
	      {
		/* Generate the address -OFFSET(%fp,%a1.l).  */
		addr = gen_rtx_REG (Pmode, A1_REG);
		addr = gen_rtx_PLUS (Pmode, addr, frame_pointer_rtx);
		addr = plus_constant (Pmode, addr, -offset);
	      }
	    else if (restore_from_sp)
	      addr = gen_rtx_POST_INC (Pmode, stack_pointer_rtx);
	    else
	      addr = plus_constant (Pmode, frame_pointer_rtx, -offset);
	    emit_move_insn (gen_rtx_REG (SImode, D0_REG + i),
			    gen_frame_mem (SImode, addr));
	    offset -= GET_MODE_SIZE (SImode);
	  }
    }
  else if (current_frame.reg_mask)
    {
      if (big)
	m68k_emit_movem (gen_rtx_PLUS (Pmode,
				       gen_rtx_REG (Pmode, A1_REG),
				       frame_pointer_rtx),
			 -(current_frame.offset + fsize),
			 current_frame.reg_no, D0_REG,
			 current_frame.reg_mask, false, false);
      else if (restore_from_sp)
	m68k_emit_movem (stack_pointer_rtx, 0,
			 current_frame.reg_no, D0_REG,
			 current_frame.reg_mask, false,
			 !TARGET_COLDFIRE);
      else
	m68k_emit_movem (frame_pointer_rtx,
			 -(current_frame.offset + fsize),
			 current_frame.reg_no, D0_REG,
			 current_frame.reg_mask, false, false);
    }

  if (current_frame.fpu_no > 0)
    {
      if (big)
	m68k_emit_movem (gen_rtx_PLUS (Pmode,
				       gen_rtx_REG (Pmode, A1_REG),
				       frame_pointer_rtx),
			 -(current_frame.foffset + fsize),
			 current_frame.fpu_no, FP0_REG,
			 current_frame.fpu_mask, false, false);
      else if (restore_from_sp)
	{
	  if (TARGET_COLDFIRE)
	    {
	      int offset;

	      /* If we used moveml to restore the integer registers, the
		 stack pointer will still point to the bottom of the moveml
		 save area.  Find the stack offset of the first FP
		 register.  */
	      if (current_frame.reg_no < MIN_MOVEM_REGS)
		offset = 0;
	      else
		offset = current_frame.reg_no * GET_MODE_SIZE (SImode);
	      m68k_emit_movem (stack_pointer_rtx, offset,
			       current_frame.fpu_no, FP0_REG,
			       current_frame.fpu_mask, false, false);
	    }
	  else
	    m68k_emit_movem (stack_pointer_rtx, 0,
			     current_frame.fpu_no, FP0_REG,
			     current_frame.fpu_mask, false, true);
	}
      else
	m68k_emit_movem (frame_pointer_rtx,
			 -(current_frame.foffset + fsize),
			 current_frame.fpu_no, FP0_REG,
			 current_frame.fpu_mask, false, false);
    }

  emit_insn (gen_blockage ());
  if (frame_pointer_needed)
    emit_insn (gen_unlink (frame_pointer_rtx));
  else if (fsize_with_regs)
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx,
			   GEN_INT (fsize_with_regs)));

  if (crtl->calls_eh_return)
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx,
			   EH_RETURN_STACKADJ_RTX));

  if (!sibcall_p)
    emit_jump_insn (ret_rtx);
}

/* Return true if X is a valid comparison operator for the dbcc 
   instruction.  

   Note it rejects floating point comparison operators.
   (In the future we could use Fdbcc).

   It also rejects some comparisons when CC_NO_OVERFLOW is set.  */
   
int
valid_dbcc_comparison_p_2 (rtx x, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (x))
    {
      case EQ: case NE: case GTU: case LTU:
      case GEU: case LEU:
        return 1;

      /* Reject some when CC_NO_OVERFLOW is set.  This may be over
         conservative */
      case GT: case LT: case GE: case LE:
        return ! (cc_prev_status.flags & CC_NO_OVERFLOW);
      default:
        return 0;
    }
}

/* Return nonzero if flags are currently in the 68881 flag register.  */
int
flags_in_68881 (void)
{
  /* We could add support for these in the future */
  return cc_status.flags & CC_IN_68881;
}

/* Return true if PARALLEL contains register REGNO.  */
static bool
m68k_reg_present_p (const_rtx parallel, unsigned int regno)
{
  int i;

  if (REG_P (parallel) && REGNO (parallel) == regno)
    return true;

  if (GET_CODE (parallel) != PARALLEL)
    return false;

  for (i = 0; i < XVECLEN (parallel, 0); ++i)
    {
      const_rtx x;

      x = XEXP (XVECEXP (parallel, 0, i), 0);
      if (REG_P (x) && REGNO (x) == regno)
	return true;
    }

  return false;
}

/* Implement TARGET_FUNCTION_OK_FOR_SIBCALL_P.  */

static bool
m68k_ok_for_sibcall_p (tree decl, tree exp)
{
  enum m68k_function_kind kind;
  
  /* We cannot use sibcalls for nested functions because we use the
     static chain register for indirect calls.  */
  if (CALL_EXPR_STATIC_CHAIN (exp))
    return false;

  if (!VOID_TYPE_P (TREE_TYPE (DECL_RESULT (cfun->decl))))
    {
      /* Check that the return value locations are the same.  For
	 example that we aren't returning a value from the sibling in
	 a D0 register but then need to transfer it to a A0 register.  */
      rtx cfun_value;
      rtx call_value;

      cfun_value = FUNCTION_VALUE (TREE_TYPE (DECL_RESULT (cfun->decl)),
				   cfun->decl);
      call_value = FUNCTION_VALUE (TREE_TYPE (exp), decl);

      /* Check that the values are equal or that the result the callee
	 function returns is superset of what the current function returns.  */
      if (!(rtx_equal_p (cfun_value, call_value)
	    || (REG_P (cfun_value)
		&& m68k_reg_present_p (call_value, REGNO (cfun_value)))))
	return false;
    }

  kind = m68k_get_function_kind (current_function_decl);
  if (kind == m68k_fk_normal_function)
    /* We can always sibcall from a normal function, because it's
       undefined if it is calling an interrupt function.  */
    return true;

  /* Otherwise we can only sibcall if the function kind is known to be
     the same.  */
  if (decl && m68k_get_function_kind (decl) == kind)
    return true;
  
  return false;
}

/* On the m68k all args are always pushed.  */

static rtx
m68k_function_arg (cumulative_args_t cum ATTRIBUTE_UNUSED,
		   machine_mode mode ATTRIBUTE_UNUSED,
		   const_tree type ATTRIBUTE_UNUSED,
		   bool named ATTRIBUTE_UNUSED)
{
  return NULL_RTX;
}

static void
m68k_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  *cum += (mode != BLKmode
	   ? (GET_MODE_SIZE (mode) + 3) & ~3
	   : (int_size_in_bytes (type) + 3) & ~3);
}

/* Convert X to a legitimate function call memory reference and return the
   result.  */

rtx
m68k_legitimize_call_address (rtx x)
{
  gcc_assert (MEM_P (x));
  if (call_operand (XEXP (x, 0), VOIDmode))
    return x;
  return replace_equiv_address (x, force_reg (Pmode, XEXP (x, 0)));
}

/* Likewise for sibling calls.  */

rtx
m68k_legitimize_sibcall_address (rtx x)
{
  gcc_assert (MEM_P (x));
  if (sibcall_operand (XEXP (x, 0), VOIDmode))
    return x;

  emit_move_insn (gen_rtx_REG (Pmode, STATIC_CHAIN_REGNUM), XEXP (x, 0));
  return replace_equiv_address (x, gen_rtx_REG (Pmode, STATIC_CHAIN_REGNUM));
}

/* Convert X to a legitimate address and return it if successful.  Otherwise
   return X.

   For the 68000, we handle X+REG by loading X into a register R and
   using R+REG.  R will go in an address reg and indexing will be used.
   However, if REG is a broken-out memory address or multiplication,
   nothing needs to be done because REG can certainly go in an address reg.  */

static rtx
m68k_legitimize_address (rtx x, rtx oldx, machine_mode mode)
{
  if (m68k_tls_symbol_p (x))
    return m68k_legitimize_tls_address (x);

  if (GET_CODE (x) == PLUS)
    {
      int ch = (x) != (oldx);
      int copied = 0;

#define COPY_ONCE(Y) if (!copied) { Y = copy_rtx (Y); copied = ch = 1; }

      if (GET_CODE (XEXP (x, 0)) == MULT)
	{
	  COPY_ONCE (x);
	  XEXP (x, 0) = force_operand (XEXP (x, 0), 0);
	}
      if (GET_CODE (XEXP (x, 1)) == MULT)
	{
	  COPY_ONCE (x);
	  XEXP (x, 1) = force_operand (XEXP (x, 1), 0);
	}
      if (ch)
	{
          if (GET_CODE (XEXP (x, 1)) == REG
	      && GET_CODE (XEXP (x, 0)) == REG)
	    {
	      if (TARGET_COLDFIRE_FPU && GET_MODE_CLASS (mode) == MODE_FLOAT)
	        {
	          COPY_ONCE (x);
	          x = force_operand (x, 0);
	        }
	      return x;
	    }
	  if (memory_address_p (mode, x))
	    return x;
	}
      if (GET_CODE (XEXP (x, 0)) == REG
	  || (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == REG
	      && GET_MODE (XEXP (XEXP (x, 0), 0)) == HImode))
	{
	  rtx temp = gen_reg_rtx (Pmode);
	  rtx val = force_operand (XEXP (x, 1), 0);
	  emit_move_insn (temp, val);
	  COPY_ONCE (x);
	  XEXP (x, 1) = temp;
	  if (TARGET_COLDFIRE_FPU && GET_MODE_CLASS (mode) == MODE_FLOAT
	      && GET_CODE (XEXP (x, 0)) == REG)
	    x = force_operand (x, 0);
	}
      else if (GET_CODE (XEXP (x, 1)) == REG
	       || (GET_CODE (XEXP (x, 1)) == SIGN_EXTEND
		   && GET_CODE (XEXP (XEXP (x, 1), 0)) == REG
		   && GET_MODE (XEXP (XEXP (x, 1), 0)) == HImode))
	{
	  rtx temp = gen_reg_rtx (Pmode);
	  rtx val = force_operand (XEXP (x, 0), 0);
	  emit_move_insn (temp, val);
	  COPY_ONCE (x);
	  XEXP (x, 0) = temp;
	  if (TARGET_COLDFIRE_FPU && GET_MODE_CLASS (mode) == MODE_FLOAT
	      && GET_CODE (XEXP (x, 1)) == REG)
	    x = force_operand (x, 0);
	}
    }

  return x;
}

 
/* Output a dbCC; jCC sequence.  Note we do not handle the 
   floating point version of this sequence (Fdbcc).  We also
   do not handle alternative conditions when CC_NO_OVERFLOW is
   set.  It is assumed that valid_dbcc_comparison_p and flags_in_68881 will
   kick those out before we get here.  */

void
output_dbcc_and_branch (rtx *operands)
{
  switch (GET_CODE (operands[3]))
    {
      case EQ:
	output_asm_insn ("dbeq %0,%l1\n\tjeq %l2", operands);
	break;

      case NE:
	output_asm_insn ("dbne %0,%l1\n\tjne %l2", operands);
	break;

      case GT:
	output_asm_insn ("dbgt %0,%l1\n\tjgt %l2", operands);
	break;

      case GTU:
	output_asm_insn ("dbhi %0,%l1\n\tjhi %l2", operands);
	break;

      case LT:
	output_asm_insn ("dblt %0,%l1\n\tjlt %l2", operands);
	break;

      case LTU:
	output_asm_insn ("dbcs %0,%l1\n\tjcs %l2", operands);
	break;

      case GE:
	output_asm_insn ("dbge %0,%l1\n\tjge %l2", operands);
	break;

      case GEU:
	output_asm_insn ("dbcc %0,%l1\n\tjcc %l2", operands);
	break;

      case LE:
	output_asm_insn ("dble %0,%l1\n\tjle %l2", operands);
	break;

      case LEU:
	output_asm_insn ("dbls %0,%l1\n\tjls %l2", operands);
	break;

      default:
	gcc_unreachable ();
    }

  /* If the decrement is to be done in SImode, then we have
     to compensate for the fact that dbcc decrements in HImode.  */
  switch (GET_MODE (operands[0]))
    {
      case E_SImode:
        output_asm_insn ("clr%.w %0\n\tsubq%.l #1,%0\n\tjpl %l1", operands);
        break;

      case E_HImode:
        break;

      default:
        gcc_unreachable ();
    }
}

const char *
output_scc_di (rtx op, rtx operand1, rtx operand2, rtx dest)
{
  rtx loperands[7];
  enum rtx_code op_code = GET_CODE (op);

  /* This does not produce a useful cc.  */
  CC_STATUS_INIT;

  /* The m68k cmp.l instruction requires operand1 to be a reg as used
     below.  Swap the operands and change the op if these requirements
     are not fulfilled.  */
  if (GET_CODE (operand2) == REG && GET_CODE (operand1) != REG)
    {
      rtx tmp = operand1;

      operand1 = operand2;
      operand2 = tmp;
      op_code = swap_condition (op_code);
    }
  loperands[0] = operand1;
  if (GET_CODE (operand1) == REG)
    loperands[1] = gen_rtx_REG (SImode, REGNO (operand1) + 1);
  else
    loperands[1] = adjust_address (operand1, SImode, 4);
  if (operand2 != const0_rtx)
    {
      loperands[2] = operand2;
      if (GET_CODE (operand2) == REG)
	loperands[3] = gen_rtx_REG (SImode, REGNO (operand2) + 1);
      else
	loperands[3] = adjust_address (operand2, SImode, 4);
    }
  loperands[4] = gen_label_rtx ();
  if (operand2 != const0_rtx)
    output_asm_insn ("cmp%.l %2,%0\n\tjne %l4\n\tcmp%.l %3,%1", loperands);
  else
    {
      if (TARGET_68020 || TARGET_COLDFIRE || ! ADDRESS_REG_P (loperands[0]))
	output_asm_insn ("tst%.l %0", loperands);
      else
	output_asm_insn ("cmp%.w #0,%0", loperands);

      output_asm_insn ("jne %l4", loperands);

      if (TARGET_68020 || TARGET_COLDFIRE || ! ADDRESS_REG_P (loperands[1]))
	output_asm_insn ("tst%.l %1", loperands);
      else
	output_asm_insn ("cmp%.w #0,%1", loperands);
    }

  loperands[5] = dest;

  switch (op_code)
    {
      case EQ:
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("seq %5", loperands);
        break;

      case NE:
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sne %5", loperands);
        break;

      case GT:
        loperands[6] = gen_label_rtx ();
        output_asm_insn ("shi %5\n\tjra %l6", loperands);
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sgt %5", loperands);
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[6]));
        break;

      case GTU:
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("shi %5", loperands);
        break;

      case LT:
        loperands[6] = gen_label_rtx ();
        output_asm_insn ("scs %5\n\tjra %l6", loperands);
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("slt %5", loperands);
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[6]));
        break;

      case LTU:
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("scs %5", loperands);
        break;

      case GE:
        loperands[6] = gen_label_rtx ();
        output_asm_insn ("scc %5\n\tjra %l6", loperands);
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sge %5", loperands);
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[6]));
        break;

      case GEU:
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("scc %5", loperands);
        break;

      case LE:
        loperands[6] = gen_label_rtx ();
        output_asm_insn ("sls %5\n\tjra %l6", loperands);
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sle %5", loperands);
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[6]));
        break;

      case LEU:
        (*targetm.asm_out.internal_label) (asm_out_file, "L",
					   CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sls %5", loperands);
        break;

      default:
	gcc_unreachable ();
    }
  return "";
}

const char *
output_btst (rtx *operands, rtx countop, rtx dataop, rtx_insn *insn, int signpos)
{
  operands[0] = countop;
  operands[1] = dataop;

  if (GET_CODE (countop) == CONST_INT)
    {
      register int count = INTVAL (countop);
      /* If COUNT is bigger than size of storage unit in use,
	 advance to the containing unit of same size.  */
      if (count > signpos)
	{
	  int offset = (count & ~signpos) / 8;
	  count = count & signpos;
	  operands[1] = dataop = adjust_address (dataop, QImode, offset);
	}
      if (count == signpos)
	cc_status.flags = CC_NOT_POSITIVE | CC_Z_IN_NOT_N;
      else
	cc_status.flags = CC_NOT_NEGATIVE | CC_Z_IN_NOT_N;

      /* These three statements used to use next_insns_test_no...
	 but it appears that this should do the same job.  */
      if (count == 31
	  && next_insn_tests_no_inequality (insn))
	return "tst%.l %1";
      if (count == 15
	  && next_insn_tests_no_inequality (insn))
	return "tst%.w %1";
      if (count == 7
	  && next_insn_tests_no_inequality (insn))
	return "tst%.b %1";
      /* Try to use `movew to ccr' followed by the appropriate branch insn.
         On some m68k variants unfortunately that's slower than btst.
         On 68000 and higher, that should also work for all HImode operands. */
      if (TUNE_CPU32 || TARGET_COLDFIRE || optimize_size)
	{
	  if (count == 3 && DATA_REG_P (operands[1])
	      && next_insn_tests_no_inequality (insn))
	    {
	    cc_status.flags = CC_NOT_NEGATIVE | CC_Z_IN_NOT_N | CC_NO_OVERFLOW;
	    return "move%.w %1,%%ccr";
	    }
	  if (count == 2 && DATA_REG_P (operands[1])
	      && next_insn_tests_no_inequality (insn))
	    {
	    cc_status.flags = CC_NOT_NEGATIVE | CC_INVERTED | CC_NO_OVERFLOW;
	    return "move%.w %1,%%ccr";
	    }
	  /* count == 1 followed by bvc/bvs and
	     count == 0 followed by bcc/bcs are also possible, but need
	     m68k-specific CC_Z_IN_NOT_V and CC_Z_IN_NOT_C flags. */
	}

      cc_status.flags = CC_NOT_NEGATIVE;
    }
  return "btst %0,%1";
}

/* Return true if X is a legitimate base register.  STRICT_P says
   whether we need strict checking.  */

bool
m68k_legitimate_base_reg_p (rtx x, bool strict_p)
{
  /* Allow SUBREG everywhere we allow REG.  This results in better code.  */
  if (!strict_p && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (REG_P (x)
	  && (strict_p
	      ? REGNO_OK_FOR_BASE_P (REGNO (x))
	      : REGNO_OK_FOR_BASE_NONSTRICT_P (REGNO (x))));
}

/* Return true if X is a legitimate index register.  STRICT_P says
   whether we need strict checking.  */

bool
m68k_legitimate_index_reg_p (rtx x, bool strict_p)
{
  if (!strict_p && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (REG_P (x)
	  && (strict_p
	      ? REGNO_OK_FOR_INDEX_P (REGNO (x))
	      : REGNO_OK_FOR_INDEX_NONSTRICT_P (REGNO (x))));
}

/* Return true if X is a legitimate index expression for a (d8,An,Xn) or
   (bd,An,Xn) addressing mode.  Fill in the INDEX and SCALE fields of
   ADDRESS if so.  STRICT_P says whether we need strict checking.  */

static bool
m68k_decompose_index (rtx x, bool strict_p, struct m68k_address *address)
{
  int scale;

  /* Check for a scale factor.  */
  scale = 1;
  if ((TARGET_68020 || TARGET_COLDFIRE)
      && GET_CODE (x) == MULT
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && (INTVAL (XEXP (x, 1)) == 2
	  || INTVAL (XEXP (x, 1)) == 4
	  || (INTVAL (XEXP (x, 1)) == 8
	      && (TARGET_COLDFIRE_FPU || !TARGET_COLDFIRE))))
    {
      scale = INTVAL (XEXP (x, 1));
      x = XEXP (x, 0);
    }

  /* Check for a word extension.  */
  if (!TARGET_COLDFIRE
      && GET_CODE (x) == SIGN_EXTEND
      && GET_MODE (XEXP (x, 0)) == HImode)
    x = XEXP (x, 0);

  if (m68k_legitimate_index_reg_p (x, strict_p))
    {
      address->scale = scale;
      address->index = x;
      return true;
    }

  return false;
}

/* Return true if X is an illegitimate symbolic constant.  */

bool
m68k_illegitimate_symbolic_constant_p (rtx x)
{
  rtx base, offset;

  if (M68K_OFFSETS_MUST_BE_WITHIN_SECTIONS_P)
    {
      split_const (x, &base, &offset);
      if (GET_CODE (base) == SYMBOL_REF
	  && !offset_within_block_p (base, INTVAL (offset)))
	return true;
    }
  return m68k_tls_reference_p (x, false);
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */

static bool
m68k_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  return m68k_illegitimate_symbolic_constant_p (x);
}

/* Return true if X is a legitimate constant address that can reach
   bytes in the range [X, X + REACH).  STRICT_P says whether we need
   strict checking.  */

static bool
m68k_legitimate_constant_address_p (rtx x, unsigned int reach, bool strict_p)
{
  rtx base, offset;

  if (!CONSTANT_ADDRESS_P (x))
    return false;

  if (flag_pic
      && !(strict_p && TARGET_PCREL)
      && symbolic_operand (x, VOIDmode))
    return false;

  if (M68K_OFFSETS_MUST_BE_WITHIN_SECTIONS_P && reach > 1)
    {
      split_const (x, &base, &offset);
      if (GET_CODE (base) == SYMBOL_REF
	  && !offset_within_block_p (base, INTVAL (offset) + reach - 1))
	return false;
    }

  return !m68k_tls_reference_p (x, false);
}

/* Return true if X is a LABEL_REF for a jump table.  Assume that unplaced
   labels will become jump tables.  */

static bool
m68k_jump_table_ref_p (rtx x)
{
  if (GET_CODE (x) != LABEL_REF)
    return false;

  rtx_insn *insn = as_a <rtx_insn *> (XEXP (x, 0));
  if (!NEXT_INSN (insn) && !PREV_INSN (insn))
    return true;

  insn = next_nonnote_insn (insn);
  return insn && JUMP_TABLE_DATA_P (insn);
}

/* Return true if X is a legitimate address for values of mode MODE.
   STRICT_P says whether strict checking is needed.  If the address
   is valid, describe its components in *ADDRESS.  */

static bool
m68k_decompose_address (machine_mode mode, rtx x,
			bool strict_p, struct m68k_address *address)
{
  unsigned int reach;

  memset (address, 0, sizeof (*address));

  if (mode == BLKmode)
    reach = 1;
  else
    reach = GET_MODE_SIZE (mode);

  /* Check for (An) (mode 2).  */
  if (m68k_legitimate_base_reg_p (x, strict_p))
    {
      address->base = x;
      return true;
    }

  /* Check for -(An) and (An)+ (modes 3 and 4).  */
  if ((GET_CODE (x) == PRE_DEC || GET_CODE (x) == POST_INC)
      && m68k_legitimate_base_reg_p (XEXP (x, 0), strict_p))
    {
      address->code = GET_CODE (x);
      address->base = XEXP (x, 0);
      return true;
    }

  /* Check for (d16,An) (mode 5).  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && IN_RANGE (INTVAL (XEXP (x, 1)), -0x8000, 0x8000 - reach)
      && m68k_legitimate_base_reg_p (XEXP (x, 0), strict_p))
    {
      address->base = XEXP (x, 0);
      address->offset = XEXP (x, 1);
      return true;
    }

  /* Check for GOT loads.  These are (bd,An,Xn) addresses if
     TARGET_68020 && flag_pic == 2, otherwise they are (d16,An)
     addresses.  */
  if (GET_CODE (x) == PLUS
      && XEXP (x, 0) == pic_offset_table_rtx)
    {
      /* As we are processing a PLUS, do not unwrap RELOC32 symbols --
	 they are invalid in this context.  */
      if (m68k_unwrap_symbol (XEXP (x, 1), false) != XEXP (x, 1))
	{
	  address->base = XEXP (x, 0);
	  address->offset = XEXP (x, 1);
	  return true;
	}
    }

  /* The ColdFire FPU only accepts addressing modes 2-5.  */
  if (TARGET_COLDFIRE_FPU && GET_MODE_CLASS (mode) == MODE_FLOAT)
    return false;

  /* Check for (xxx).w and (xxx).l.  Also, in the TARGET_PCREL case,
     check for (d16,PC) or (bd,PC,Xn) with a suppressed index register.
     All these modes are variations of mode 7.  */
  if (m68k_legitimate_constant_address_p (x, reach, strict_p))
    {
      address->offset = x;
      return true;
    }

  /* Check for (d8,PC,Xn), a mode 7 form.  This case is needed for
     tablejumps.

     ??? do_tablejump creates these addresses before placing the target
     label, so we have to assume that unplaced labels are jump table
     references.  It seems unlikely that we would ever generate indexed
     accesses to unplaced labels in other cases.  */
  if (GET_CODE (x) == PLUS
      && m68k_jump_table_ref_p (XEXP (x, 1))
      && m68k_decompose_index (XEXP (x, 0), strict_p, address))
    {
      address->offset = XEXP (x, 1);
      return true;
    }

  /* Everything hereafter deals with (d8,An,Xn.SIZE*SCALE) or
     (bd,An,Xn.SIZE*SCALE) addresses.  */

  if (TARGET_68020)
    {
      /* Check for a nonzero base displacement.  */
      if (GET_CODE (x) == PLUS
	  && m68k_legitimate_constant_address_p (XEXP (x, 1), reach, strict_p))
	{
	  address->offset = XEXP (x, 1);
	  x = XEXP (x, 0);
	}

      /* Check for a suppressed index register.  */
      if (m68k_legitimate_base_reg_p (x, strict_p))
	{
	  address->base = x;
	  return true;
	}

      /* Check for a suppressed base register.  Do not allow this case
	 for non-symbolic offsets as it effectively gives gcc freedom
	 to treat data registers as base registers, which can generate
	 worse code.  */
      if (address->offset
	  && symbolic_operand (address->offset, VOIDmode)
	  && m68k_decompose_index (x, strict_p, address))
	return true;
    }
  else
    {
      /* Check for a nonzero base displacement.  */
      if (GET_CODE (x) == PLUS
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && IN_RANGE (INTVAL (XEXP (x, 1)), -0x80, 0x80 - reach))
	{
	  address->offset = XEXP (x, 1);
	  x = XEXP (x, 0);
	}
    }

  /* We now expect the sum of a base and an index.  */
  if (GET_CODE (x) == PLUS)
    {
      if (m68k_legitimate_base_reg_p (XEXP (x, 0), strict_p)
	  && m68k_decompose_index (XEXP (x, 1), strict_p, address))
	{
	  address->base = XEXP (x, 0);
	  return true;
	}

      if (m68k_legitimate_base_reg_p (XEXP (x, 1), strict_p)
	  && m68k_decompose_index (XEXP (x, 0), strict_p, address))
	{
	  address->base = XEXP (x, 1);
	  return true;
	}
    }
  return false;
}

/* Return true if X is a legitimate address for values of mode MODE.
   STRICT_P says whether strict checking is needed.  */

bool
m68k_legitimate_address_p (machine_mode mode, rtx x, bool strict_p)
{
  struct m68k_address address;

  return m68k_decompose_address (mode, x, strict_p, &address);
}

/* Return true if X is a memory, describing its address in ADDRESS if so.
   Apply strict checking if called during or after reload.  */

static bool
m68k_legitimate_mem_p (rtx x, struct m68k_address *address)
{
  return (MEM_P (x)
	  && m68k_decompose_address (GET_MODE (x), XEXP (x, 0),
				     reload_in_progress || reload_completed,
				     address));
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.  */

bool
m68k_legitimate_constant_p (machine_mode mode, rtx x)
{
  return mode != XFmode && !m68k_illegitimate_symbolic_constant_p (x);
}

/* Return true if X matches the 'Q' constraint.  It must be a memory
   with a base address and no constant offset or index.  */

bool
m68k_matches_q_p (rtx x)
{
  struct m68k_address address;

  return (m68k_legitimate_mem_p (x, &address)
	  && address.code == UNKNOWN
	  && address.base
	  && !address.offset
	  && !address.index);
}

/* Return true if X matches the 'U' constraint.  It must be a base address
   with a constant offset and no index.  */

bool
m68k_matches_u_p (rtx x)
{
  struct m68k_address address;

  return (m68k_legitimate_mem_p (x, &address)
	  && address.code == UNKNOWN
	  && address.base
	  && address.offset
	  && !address.index);
}

/* Return GOT pointer.  */

static rtx
m68k_get_gp (void)
{
  if (pic_offset_table_rtx == NULL_RTX)
    pic_offset_table_rtx = gen_rtx_REG (Pmode, PIC_REG);

  crtl->uses_pic_offset_table = 1;

  return pic_offset_table_rtx;
}

/* M68K relocations, used to distinguish GOT and TLS relocations in UNSPEC
   wrappers.  */
enum m68k_reloc { RELOC_GOT, RELOC_TLSGD, RELOC_TLSLDM, RELOC_TLSLDO,
		  RELOC_TLSIE, RELOC_TLSLE };

#define TLS_RELOC_P(RELOC) ((RELOC) != RELOC_GOT)

/* Wrap symbol X into unspec representing relocation RELOC.
   BASE_REG - register that should be added to the result.
   TEMP_REG - if non-null, temporary register.  */

static rtx
m68k_wrap_symbol (rtx x, enum m68k_reloc reloc, rtx base_reg, rtx temp_reg)
{
  bool use_x_p;

  use_x_p = (base_reg == pic_offset_table_rtx) ? TARGET_XGOT : TARGET_XTLS;

  if (TARGET_COLDFIRE && use_x_p)
    /* When compiling with -mx{got, tls} switch the code will look like this:

       move.l <X>@<RELOC>,<TEMP_REG>
       add.l <BASE_REG>,<TEMP_REG>  */
    {
      /* Wrap X in UNSPEC_??? to tip m68k_output_addr_const_extra
	 to put @RELOC after reference.  */
      x = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, x, GEN_INT (reloc)),
			  UNSPEC_RELOC32);
      x = gen_rtx_CONST (Pmode, x);

      if (temp_reg == NULL)
	{
	  gcc_assert (can_create_pseudo_p ());
	  temp_reg = gen_reg_rtx (Pmode);
	}

      emit_move_insn (temp_reg, x);
      emit_insn (gen_addsi3 (temp_reg, temp_reg, base_reg));
      x = temp_reg;
    }
  else
    {
      x = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, x, GEN_INT (reloc)),
			  UNSPEC_RELOC16);
      x = gen_rtx_CONST (Pmode, x);

      x = gen_rtx_PLUS (Pmode, base_reg, x);
    }

  return x;
}

/* Helper for m68k_unwrap_symbol.
   Also, if unwrapping was successful (that is if (ORIG != <return value>)),
   sets *RELOC_PTR to relocation type for the symbol.  */

static rtx
m68k_unwrap_symbol_1 (rtx orig, bool unwrap_reloc32_p,
		      enum m68k_reloc *reloc_ptr)
{
  if (GET_CODE (orig) == CONST)
    {
      rtx x;
      enum m68k_reloc dummy;

      x = XEXP (orig, 0);

      if (reloc_ptr == NULL)
	reloc_ptr = &dummy;

      /* Handle an addend.  */
      if ((GET_CODE (x) == PLUS || GET_CODE (x) == MINUS)
	  && CONST_INT_P (XEXP (x, 1)))
	x = XEXP (x, 0);

      if (GET_CODE (x) == UNSPEC)
	{
	  switch (XINT (x, 1))
	    {
	    case UNSPEC_RELOC16:
	      orig = XVECEXP (x, 0, 0);
	      *reloc_ptr = (enum m68k_reloc) INTVAL (XVECEXP (x, 0, 1));
	      break;

	    case UNSPEC_RELOC32:
	      if (unwrap_reloc32_p)
		{
		  orig = XVECEXP (x, 0, 0);
		  *reloc_ptr = (enum m68k_reloc) INTVAL (XVECEXP (x, 0, 1));
		}
	      break;

	    default:
	      break;
	    }
	}
    }

  return orig;
}

/* Unwrap symbol from UNSPEC_RELOC16 and, if unwrap_reloc32_p,
   UNSPEC_RELOC32 wrappers.  */

rtx
m68k_unwrap_symbol (rtx orig, bool unwrap_reloc32_p)
{
  return m68k_unwrap_symbol_1 (orig, unwrap_reloc32_p, NULL);
}

/* Prescan insn before outputing assembler for it.  */

void
m68k_final_prescan_insn (rtx_insn *insn ATTRIBUTE_UNUSED,
			 rtx *operands, int n_operands)
{
  int i;

  /* Combine and, possibly, other optimizations may do good job
     converting
       (const (unspec [(symbol)]))
     into
       (const (plus (unspec [(symbol)])
                    (const_int N))).
     The problem with this is emitting @TLS or @GOT decorations.
     The decoration is emitted when processing (unspec), so the
     result would be "#symbol@TLSLE+N" instead of "#symbol+N@TLSLE".

     It seems that the easiest solution to this is to convert such
     operands to
       (const (unspec [(plus (symbol)
                             (const_int N))])).
     Note, that the top level of operand remains intact, so we don't have
     to patch up anything outside of the operand.  */

  subrtx_var_iterator::array_type array;
  for (i = 0; i < n_operands; ++i)
    {
      rtx op;

      op = operands[i];

      FOR_EACH_SUBRTX_VAR (iter, array, op, ALL)
	{
	  rtx x = *iter;
	  if (m68k_unwrap_symbol (x, true) != x)
	    {
	      rtx plus;

	      gcc_assert (GET_CODE (x) == CONST);
	      plus = XEXP (x, 0);

	      if (GET_CODE (plus) == PLUS || GET_CODE (plus) == MINUS)
		{
		  rtx unspec;
		  rtx addend;

		  unspec = XEXP (plus, 0);
		  gcc_assert (GET_CODE (unspec) == UNSPEC);
		  addend = XEXP (plus, 1);
		  gcc_assert (CONST_INT_P (addend));

		  /* We now have all the pieces, rearrange them.  */

		  /* Move symbol to plus.  */
		  XEXP (plus, 0) = XVECEXP (unspec, 0, 0);

		  /* Move plus inside unspec.  */
		  XVECEXP (unspec, 0, 0) = plus;

		  /* Move unspec to top level of const.  */
		  XEXP (x, 0) = unspec;
		}
	      iter.skip_subrtxes ();
	    }
	}
    }
}

/* Move X to a register and add REG_EQUAL note pointing to ORIG.
   If REG is non-null, use it; generate new pseudo otherwise.  */

static rtx
m68k_move_to_reg (rtx x, rtx orig, rtx reg)
{
  rtx_insn *insn;

  if (reg == NULL_RTX)
    {
      gcc_assert (can_create_pseudo_p ());
      reg = gen_reg_rtx (Pmode);
    }

  insn = emit_move_insn (reg, x);
  /* Put a REG_EQUAL note on this insn, so that it can be optimized
     by loop.  */
  set_unique_reg_note (insn, REG_EQUAL, orig);

  return reg;
}

/* Does the same as m68k_wrap_symbol, but returns a memory reference to
   GOT slot.  */

static rtx
m68k_wrap_symbol_into_got_ref (rtx x, enum m68k_reloc reloc, rtx temp_reg)
{
  x = m68k_wrap_symbol (x, reloc, m68k_get_gp (), temp_reg);

  x = gen_rtx_MEM (Pmode, x);
  MEM_READONLY_P (x) = 1;

  return x;
}

/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go to REG.  If we need more
   than one register, we lose.  

   An address is legitimized by making an indirect reference
   through the Global Offset Table with the name of the symbol
   used as an offset.  

   The assembler and linker are responsible for placing the 
   address of the symbol in the GOT.  The function prologue
   is responsible for initializing a5 to the starting address
   of the GOT.

   The assembler is also responsible for translating a symbol name
   into a constant displacement from the start of the GOT.  

   A quick example may make things a little clearer:

   When not generating PIC code to store the value 12345 into _foo
   we would generate the following code:

	movel #12345, _foo

   When generating PIC two transformations are made.  First, the compiler
   loads the address of foo into a register.  So the first transformation makes:

	lea	_foo, a0
	movel   #12345, a0@

   The code in movsi will intercept the lea instruction and call this
   routine which will transform the instructions into:

	movel   a5@(_foo:w), a0
	movel   #12345, a0@
   

   That (in a nutshell) is how *all* symbol and label references are 
   handled.  */

rtx
legitimize_pic_address (rtx orig, machine_mode mode ATTRIBUTE_UNUSED,
		        rtx reg)
{
  rtx pic_ref = orig;

  /* First handle a simple SYMBOL_REF or LABEL_REF */
  if (GET_CODE (orig) == SYMBOL_REF || GET_CODE (orig) == LABEL_REF)
    {
      gcc_assert (reg);

      pic_ref = m68k_wrap_symbol_into_got_ref (orig, RELOC_GOT, reg);
      pic_ref = m68k_move_to_reg (pic_ref, orig, reg);
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base;

      /* Make sure this has not already been legitimized.  */
      if (m68k_unwrap_symbol (orig, true) != orig)
	return orig;

      gcc_assert (reg);

      /* legitimize both operands of the PLUS */
      gcc_assert (GET_CODE (XEXP (orig, 0)) == PLUS);
      
      base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
      orig = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
				     base == reg ? 0 : reg);

      if (GET_CODE (orig) == CONST_INT)
	pic_ref = plus_constant (Pmode, base, INTVAL (orig));
      else
	pic_ref = gen_rtx_PLUS (Pmode, base, orig);
    }

  return pic_ref;
}

/* The __tls_get_addr symbol.  */
static GTY(()) rtx m68k_tls_get_addr;

/* Return SYMBOL_REF for __tls_get_addr.  */

static rtx
m68k_get_tls_get_addr (void)
{
  if (m68k_tls_get_addr == NULL_RTX)
    m68k_tls_get_addr = init_one_libfunc ("__tls_get_addr");

  return m68k_tls_get_addr;
}

/* Return libcall result in A0 instead of usual D0.  */
static bool m68k_libcall_value_in_a0_p = false;

/* Emit instruction sequence that calls __tls_get_addr.  X is
   the TLS symbol we are referencing and RELOC is the symbol type to use
   (either TLSGD or TLSLDM).  EQV is the REG_EQUAL note for the sequence
   emitted.  A pseudo register with result of __tls_get_addr call is
   returned.  */

static rtx
m68k_call_tls_get_addr (rtx x, rtx eqv, enum m68k_reloc reloc)
{
  rtx a0;
  rtx_insn *insns;
  rtx dest;

  /* Emit the call sequence.  */
  start_sequence ();

  /* FIXME: Unfortunately, emit_library_call_value does not
     consider (plus (%a5) (const (unspec))) to be a good enough
     operand for push, so it forces it into a register.  The bad
     thing about this is that combiner, due to copy propagation and other
     optimizations, sometimes can not later fix this.  As a consequence,
     additional register may be allocated resulting in a spill.
     For reference, see args processing loops in
     calls.c:emit_library_call_value_1.
     For testcase, see gcc.target/m68k/tls-{gd, ld}.c  */
  x = m68k_wrap_symbol (x, reloc, m68k_get_gp (), NULL_RTX);

  /* __tls_get_addr() is not a libcall, but emitting a libcall_value
     is the simpliest way of generating a call.  The difference between
     __tls_get_addr() and libcall is that the result is returned in D0
     instead of A0.  To workaround this, we use m68k_libcall_value_in_a0_p
     which temporarily switches returning the result to A0.  */ 

  m68k_libcall_value_in_a0_p = true;
  a0 = emit_library_call_value (m68k_get_tls_get_addr (), NULL_RTX, LCT_PURE,
				Pmode, x, Pmode);
  m68k_libcall_value_in_a0_p = false;
  
  insns = get_insns ();
  end_sequence ();

  gcc_assert (can_create_pseudo_p ());
  dest = gen_reg_rtx (Pmode);
  emit_libcall_block (insns, dest, a0, eqv);

  return dest;
}

/* The __tls_get_addr symbol.  */
static GTY(()) rtx m68k_read_tp;

/* Return SYMBOL_REF for __m68k_read_tp.  */

static rtx
m68k_get_m68k_read_tp (void)
{
  if (m68k_read_tp == NULL_RTX)
    m68k_read_tp = init_one_libfunc ("__m68k_read_tp");

  return m68k_read_tp;
}

/* Emit instruction sequence that calls __m68k_read_tp.
   A pseudo register with result of __m68k_read_tp call is returned.  */

static rtx 
m68k_call_m68k_read_tp (void)
{
  rtx a0;
  rtx eqv;
  rtx_insn *insns;
  rtx dest;

  start_sequence ();

  /* __m68k_read_tp() is not a libcall, but emitting a libcall_value
     is the simpliest way of generating a call.  The difference between
     __m68k_read_tp() and libcall is that the result is returned in D0
     instead of A0.  To workaround this, we use m68k_libcall_value_in_a0_p
     which temporarily switches returning the result to A0.  */ 

  /* Emit the call sequence.  */
  m68k_libcall_value_in_a0_p = true;
  a0 = emit_library_call_value (m68k_get_m68k_read_tp (), NULL_RTX, LCT_PURE,
				Pmode);
  m68k_libcall_value_in_a0_p = false;
  insns = get_insns ();
  end_sequence ();

  /* Attach a unique REG_EQUIV, to allow the RTL optimizers to
     share the m68k_read_tp result with other IE/LE model accesses.  */
  eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const1_rtx), UNSPEC_RELOC32);

  gcc_assert (can_create_pseudo_p ());
  dest = gen_reg_rtx (Pmode);
  emit_libcall_block (insns, dest, a0, eqv);

  return dest;
}

/* Return a legitimized address for accessing TLS SYMBOL_REF X.
   For explanations on instructions sequences see TLS/NPTL ABI for m68k and
   ColdFire.  */

rtx
m68k_legitimize_tls_address (rtx orig)
{
  switch (SYMBOL_REF_TLS_MODEL (orig))
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
      orig = m68k_call_tls_get_addr (orig, orig, RELOC_TLSGD);
      break;

    case TLS_MODEL_LOCAL_DYNAMIC:
      {
	rtx eqv;
	rtx a0;
	rtx x;
 
	/* Attach a unique REG_EQUIV, to allow the RTL optimizers to
	   share the LDM result with other LD model accesses.  */
	eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx),
			      UNSPEC_RELOC32);

	a0 = m68k_call_tls_get_addr (orig, eqv, RELOC_TLSLDM);

	x = m68k_wrap_symbol (orig, RELOC_TLSLDO, a0, NULL_RTX);

	if (can_create_pseudo_p ())
	  x = m68k_move_to_reg (x, orig, NULL_RTX);

	orig = x;
	break;
      }

    case TLS_MODEL_INITIAL_EXEC:
      {
	rtx a0;
	rtx x;

	a0 = m68k_call_m68k_read_tp ();

	x = m68k_wrap_symbol_into_got_ref (orig, RELOC_TLSIE, NULL_RTX);
	x = gen_rtx_PLUS (Pmode, x, a0);

	if (can_create_pseudo_p ())
	  x = m68k_move_to_reg (x, orig, NULL_RTX);

	orig = x;
	break;
      }

    case TLS_MODEL_LOCAL_EXEC:
      {
	rtx a0;
	rtx x;

	a0 = m68k_call_m68k_read_tp ();

	x = m68k_wrap_symbol (orig, RELOC_TLSLE, a0, NULL_RTX);

	if (can_create_pseudo_p ())
	  x = m68k_move_to_reg (x, orig, NULL_RTX);

	orig = x;
	break;
      }

    default:
      gcc_unreachable ();
    }

  return orig;
}

/* Return true if X is a TLS symbol.  */

static bool
m68k_tls_symbol_p (rtx x)
{
  if (!TARGET_HAVE_TLS)
    return false;

  if (GET_CODE (x) != SYMBOL_REF)
    return false;

  return SYMBOL_REF_TLS_MODEL (x) != 0;
}

/* If !LEGITIMATE_P, return true if X is a TLS symbol reference,
   though illegitimate one.
   If LEGITIMATE_P, return true if X is a legitimate TLS symbol reference.  */

bool
m68k_tls_reference_p (rtx x, bool legitimate_p)
{
  if (!TARGET_HAVE_TLS)
    return false;

  if (!legitimate_p)
    {
      subrtx_var_iterator::array_type array;
      FOR_EACH_SUBRTX_VAR (iter, array, x, ALL)
	{
	  rtx x = *iter;

	  /* Note: this is not the same as m68k_tls_symbol_p.  */
	  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x) != 0)
	    return true;

	  /* Don't recurse into legitimate TLS references.  */
	  if (m68k_tls_reference_p (x, true))
	    iter.skip_subrtxes ();
	}
      return false;
    }
  else
    {
      enum m68k_reloc reloc = RELOC_GOT;

      return (m68k_unwrap_symbol_1 (x, true, &reloc) != x
	      && TLS_RELOC_P (reloc));
    }
}



#define USE_MOVQ(i)	((unsigned) ((i) + 128) <= 255)

/* Return the type of move that should be used for integer I.  */

M68K_CONST_METHOD
m68k_const_method (HOST_WIDE_INT i)
{
  unsigned u;

  if (USE_MOVQ (i))
    return MOVQ;

  /* The ColdFire doesn't have byte or word operations.  */
  /* FIXME: This may not be useful for the m68060 either.  */
  if (!TARGET_COLDFIRE)
    {
      /* if -256 < N < 256 but N is not in range for a moveq
	 N^ff will be, so use moveq #N^ff, dreg; not.b dreg.  */
      if (USE_MOVQ (i ^ 0xff))
	return NOTB;
      /* Likewise, try with not.w */
      if (USE_MOVQ (i ^ 0xffff))
	return NOTW;
      /* This is the only value where neg.w is useful */
      if (i == -65408)
	return NEGW;
    }

  /* Try also with swap.  */
  u = i;
  if (USE_MOVQ ((u >> 16) | (u << 16)))
    return SWAP;

  if (TARGET_ISAB)
    {
      /* Try using MVZ/MVS with an immediate value to load constants.  */
      if (i >= 0 && i <= 65535)
	return MVZ;
      if (i >= -32768 && i <= 32767)
	return MVS;
    }

  /* Otherwise, use move.l */
  return MOVL;
}

/* Return the cost of moving constant I into a data register.  */

static int
const_int_cost (HOST_WIDE_INT i)
{
  switch (m68k_const_method (i))
    {
    case MOVQ:
      /* Constants between -128 and 127 are cheap due to moveq.  */
      return 0;
    case MVZ:
    case MVS:
    case NOTB:
    case NOTW:
    case NEGW:
    case SWAP:
      /* Constants easily generated by moveq + not.b/not.w/neg.w/swap.  */
      return 1;
    case MOVL:
      return 2;
    default:
      gcc_unreachable ();
    }
}

static bool
m68k_rtx_costs (rtx x, machine_mode mode, int outer_code,
		int opno ATTRIBUTE_UNUSED,
		int *total, bool speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
      /* Constant zero is super cheap due to clr instruction.  */
      if (x == const0_rtx)
	*total = 0;
      else
        *total = const_int_cost (INTVAL (x));
      return true;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = 3;
      return true;

    case CONST_DOUBLE:
      /* Make 0.0 cheaper than other floating constants to
         encourage creating tstsf and tstdf insns.  */
      if (outer_code == COMPARE
          && (x == CONST0_RTX (SFmode) || x == CONST0_RTX (DFmode)))
	*total = 4;
      else
	*total = 5;
      return true;

    /* These are vaguely right for a 68020.  */
    /* The costs for long multiply have been adjusted to work properly
       in synth_mult on the 68020, relative to an average of the time
       for add and the time for shift, taking away a little more because
       sometimes move insns are needed.  */
    /* div?.w is relatively cheaper on 68000 counted in COSTS_N_INSNS
       terms.  */
#define MULL_COST				\
  (TUNE_68060 ? 2				\
   : TUNE_68040 ? 5				\
   : (TUNE_CFV2 && TUNE_EMAC) ? 3		\
   : (TUNE_CFV2 && TUNE_MAC) ? 4		\
   : TUNE_CFV2 ? 8				\
   : TARGET_COLDFIRE ? 3 : 13)

#define MULW_COST				\
  (TUNE_68060 ? 2				\
   : TUNE_68040 ? 3				\
   : TUNE_68000_10 ? 5				\
   : (TUNE_CFV2 && TUNE_EMAC) ? 3		\
   : (TUNE_CFV2 && TUNE_MAC) ? 2		\
   : TUNE_CFV2 ? 8				\
   : TARGET_COLDFIRE ? 2 : 8)

#define DIVW_COST				\
  (TARGET_CF_HWDIV ? 11				\
   : TUNE_68000_10 || TARGET_COLDFIRE ? 12 : 27)

    case PLUS:
      /* An lea costs about three times as much as a simple add.  */
      if (mode == SImode
	  && GET_CODE (XEXP (x, 1)) == REG
	  && GET_CODE (XEXP (x, 0)) == MULT
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == REG
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && (INTVAL (XEXP (XEXP (x, 0), 1)) == 2
	      || INTVAL (XEXP (XEXP (x, 0), 1)) == 4
	      || INTVAL (XEXP (XEXP (x, 0), 1)) == 8))
	{
	    /* lea an@(dx:l:i),am */
	    *total = COSTS_N_INSNS (TARGET_COLDFIRE ? 2 : 3);
	    return true;
	}
      return false;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if (TUNE_68060)
	{
          *total = COSTS_N_INSNS(1);
	  return true;
	}
      if (TUNE_68000_10)
        {
	  if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	    {
	      if (INTVAL (XEXP (x, 1)) < 16)
	        *total = COSTS_N_INSNS (2) + INTVAL (XEXP (x, 1)) / 2;
	      else
	        /* We're using clrw + swap for these cases.  */
	        *total = COSTS_N_INSNS (4) + (INTVAL (XEXP (x, 1)) - 16) / 2;
	    }
	  else
	    *total = COSTS_N_INSNS (10); /* Worst case.  */
	  return true;
        }
      /* A shift by a big integer takes an extra instruction.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && (INTVAL (XEXP (x, 1)) == 16))
	{
	  *total = COSTS_N_INSNS (2);	 /* clrw;swap */
	  return true;
	}
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && !(INTVAL (XEXP (x, 1)) > 0
	       && INTVAL (XEXP (x, 1)) <= 8))
	{
	  *total = COSTS_N_INSNS (TARGET_COLDFIRE ? 1 : 3);	 /* lsr #i,dn */
	  return true;
	}
      return false;

    case MULT:
      if ((GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	   || GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	  && mode == SImode)
        *total = COSTS_N_INSNS (MULW_COST);
      else if (mode == QImode || mode == HImode)
        *total = COSTS_N_INSNS (MULW_COST);
      else
        *total = COSTS_N_INSNS (MULL_COST);
      return true;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      if (mode == QImode || mode == HImode)
        *total = COSTS_N_INSNS (DIVW_COST);	/* div.w */
      else if (TARGET_CF_HWDIV)
        *total = COSTS_N_INSNS (18);
      else
	*total = COSTS_N_INSNS (43);		/* div.l */
      return true;

    case ZERO_EXTRACT:
      if (outer_code == COMPARE)
        *total = 0;
      return false;

    default:
      return false;
    }
}

/* Return an instruction to move CONST_INT OPERANDS[1] into data register
   OPERANDS[0].  */

static const char *
output_move_const_into_data_reg (rtx *operands)
{
  HOST_WIDE_INT i;

  i = INTVAL (operands[1]);
  switch (m68k_const_method (i))
    {
    case MVZ:
      return "mvzw %1,%0";
    case MVS:
      return "mvsw %1,%0";
    case MOVQ:
      return "moveq %1,%0";
    case NOTB:
      CC_STATUS_INIT;
      operands[1] = GEN_INT (i ^ 0xff);
      return "moveq %1,%0\n\tnot%.b %0";
    case NOTW:
      CC_STATUS_INIT;
      operands[1] = GEN_INT (i ^ 0xffff);
      return "moveq %1,%0\n\tnot%.w %0";
    case NEGW:
      CC_STATUS_INIT;
      return "moveq #-128,%0\n\tneg%.w %0";
    case SWAP:
      {
	unsigned u = i;

	operands[1] = GEN_INT ((u << 16) | (u >> 16));
	return "moveq %1,%0\n\tswap %0";
      }
    case MOVL:
      return "move%.l %1,%0";
    default:
      gcc_unreachable ();
    }
}

/* Return true if I can be handled by ISA B's mov3q instruction.  */

bool
valid_mov3q_const (HOST_WIDE_INT i)
{
  return TARGET_ISAB && (i == -1 || IN_RANGE (i, 1, 7));
}

/* Return an instruction to move CONST_INT OPERANDS[1] into OPERANDS[0].
   I is the value of OPERANDS[1].  */

static const char *
output_move_simode_const (rtx *operands)
{
  rtx dest;
  HOST_WIDE_INT src;

  dest = operands[0];
  src = INTVAL (operands[1]);
  if (src == 0
      && (DATA_REG_P (dest) || MEM_P (dest))
      /* clr insns on 68000 read before writing.  */
      && ((TARGET_68010 || TARGET_COLDFIRE)
	  || !(MEM_P (dest) && MEM_VOLATILE_P (dest))))
    return "clr%.l %0";
  else if (GET_MODE (dest) == SImode && valid_mov3q_const (src))
    return "mov3q%.l %1,%0";
  else if (src == 0 && ADDRESS_REG_P (dest))
    return "sub%.l %0,%0";
  else if (DATA_REG_P (dest))
    return output_move_const_into_data_reg (operands);
  else if (ADDRESS_REG_P (dest) && IN_RANGE (src, -0x8000, 0x7fff))
    {
      if (valid_mov3q_const (src))
        return "mov3q%.l %1,%0";
      return "move%.w %1,%0";
    }
  else if (MEM_P (dest)
	   && GET_CODE (XEXP (dest, 0)) == PRE_DEC
	   && REGNO (XEXP (XEXP (dest, 0), 0)) == STACK_POINTER_REGNUM
	   && IN_RANGE (src, -0x8000, 0x7fff))
    {
      if (valid_mov3q_const (src))
        return "mov3q%.l %1,%-";
      return "pea %a1";
    }
  return "move%.l %1,%0";
}

const char *
output_move_simode (rtx *operands)
{
  if (GET_CODE (operands[1]) == CONST_INT)
    return output_move_simode_const (operands);
  else if ((GET_CODE (operands[1]) == SYMBOL_REF
	    || GET_CODE (operands[1]) == CONST)
	   && push_operand (operands[0], SImode))
    return "pea %a1";
  else if ((GET_CODE (operands[1]) == SYMBOL_REF
	    || GET_CODE (operands[1]) == CONST)
	   && ADDRESS_REG_P (operands[0]))
    return "lea %a1,%0";
  return "move%.l %1,%0";
}

const char *
output_move_himode (rtx *operands)
{
 if (GET_CODE (operands[1]) == CONST_INT)
    {
      if (operands[1] == const0_rtx
	  && (DATA_REG_P (operands[0])
	      || GET_CODE (operands[0]) == MEM)
	  /* clr insns on 68000 read before writing.  */
	  && ((TARGET_68010 || TARGET_COLDFIRE)
	      || !(GET_CODE (operands[0]) == MEM
		   && MEM_VOLATILE_P (operands[0]))))
	return "clr%.w %0";
      else if (operands[1] == const0_rtx
	       && ADDRESS_REG_P (operands[0]))
	return "sub%.l %0,%0";
      else if (DATA_REG_P (operands[0])
	       && INTVAL (operands[1]) < 128
	       && INTVAL (operands[1]) >= -128)
	return "moveq %1,%0";
      else if (INTVAL (operands[1]) < 0x8000
	       && INTVAL (operands[1]) >= -0x8000)
	return "move%.w %1,%0";
    }
  else if (CONSTANT_P (operands[1]))
    return "move%.l %1,%0";
  return "move%.w %1,%0";
}

const char *
output_move_qimode (rtx *operands)
{
  /* 68k family always modifies the stack pointer by at least 2, even for
     byte pushes.  The 5200 (ColdFire) does not do this.  */
  
  /* This case is generated by pushqi1 pattern now.  */
  gcc_assert (!(GET_CODE (operands[0]) == MEM
		&& GET_CODE (XEXP (operands[0], 0)) == PRE_DEC
		&& XEXP (XEXP (operands[0], 0), 0) == stack_pointer_rtx
		&& ! ADDRESS_REG_P (operands[1])
		&& ! TARGET_COLDFIRE));

  /* clr and st insns on 68000 read before writing.  */
  if (!ADDRESS_REG_P (operands[0])
      && ((TARGET_68010 || TARGET_COLDFIRE)
	  || !(GET_CODE (operands[0]) == MEM && MEM_VOLATILE_P (operands[0]))))
    {
      if (operands[1] == const0_rtx)
	return "clr%.b %0";
      if ((!TARGET_COLDFIRE || DATA_REG_P (operands[0]))
	  && GET_CODE (operands[1]) == CONST_INT
	  && (INTVAL (operands[1]) & 255) == 255)
	{
	  CC_STATUS_INIT;
	  return "st %0";
	}
    }
  if (GET_CODE (operands[1]) == CONST_INT
      && DATA_REG_P (operands[0])
      && INTVAL (operands[1]) < 128
      && INTVAL (operands[1]) >= -128)
    return "moveq %1,%0";
  if (operands[1] == const0_rtx && ADDRESS_REG_P (operands[0]))
    return "sub%.l %0,%0";
  if (GET_CODE (operands[1]) != CONST_INT && CONSTANT_P (operands[1]))
    return "move%.l %1,%0";
  /* 68k family (including the 5200 ColdFire) does not support byte moves to
     from address registers.  */
  if (ADDRESS_REG_P (operands[0]) || ADDRESS_REG_P (operands[1]))
    return "move%.w %1,%0";
  return "move%.b %1,%0";
}

const char *
output_move_stricthi (rtx *operands)
{
  if (operands[1] == const0_rtx
      /* clr insns on 68000 read before writing.  */
      && ((TARGET_68010 || TARGET_COLDFIRE)
	  || !(GET_CODE (operands[0]) == MEM && MEM_VOLATILE_P (operands[0]))))
    return "clr%.w %0";
  return "move%.w %1,%0";
}

const char *
output_move_strictqi (rtx *operands)
{
  if (operands[1] == const0_rtx
      /* clr insns on 68000 read before writing.  */
      && ((TARGET_68010 || TARGET_COLDFIRE)
          || !(GET_CODE (operands[0]) == MEM && MEM_VOLATILE_P (operands[0]))))
    return "clr%.b %0";
  return "move%.b %1,%0";
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static const char *
singlemove_string (rtx *operands)
{
  if (GET_CODE (operands[1]) == CONST_INT)
    return output_move_simode_const (operands);
  return "move%.l %1,%0";
}


/* Output assembler or rtl code to perform a doubleword move insn
   with operands OPERANDS.
   Pointers to 3 helper functions should be specified:
   HANDLE_REG_ADJUST to adjust a register by a small value,
   HANDLE_COMPADR to compute an address and
   HANDLE_MOVSI to move 4 bytes.  */

static void
handle_move_double (rtx operands[2],
		    void (*handle_reg_adjust) (rtx, int),
		    void (*handle_compadr) (rtx [2]),
		    void (*handle_movsi) (rtx [2]))
{
  enum
    {
      REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP
    } optype0, optype1;
  rtx latehalf[2];
  rtx middlehalf[2];
  rtx xops[2];
  rtx addreg0 = 0, addreg1 = 0;
  int dest_overlapped_low = 0;
  int size = GET_MODE_SIZE (GET_MODE (operands[0]));

  middlehalf[0] = 0;
  middlehalf[1] = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == POST_INC)
    optype0 = POPOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
    optype0 = PUSHOP;
  else if (GET_CODE (operands[0]) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1]))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == POST_INC)
    optype1 = POPOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)
    optype1 = PUSHOP;
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not supposed
     to allow to happen.  Generating code for these cases is
     painful.  */
  gcc_assert (optype0 != RNDOP && optype1 != RNDOP);

  /* If one operand is decrementing and one is incrementing
     decrement the former register explicitly
     and change that operand into ordinary indexing.  */

  if (optype0 == PUSHOP && optype1 == POPOP)
    {
      operands[0] = XEXP (XEXP (operands[0], 0), 0);

      handle_reg_adjust (operands[0], -size);

      if (GET_MODE (operands[1]) == XFmode)
	operands[0] = gen_rtx_MEM (XFmode, operands[0]);
      else if (GET_MODE (operands[0]) == DFmode)
	operands[0] = gen_rtx_MEM (DFmode, operands[0]);
      else
	operands[0] = gen_rtx_MEM (DImode, operands[0]);
      optype0 = OFFSOP;
    }
  if (optype0 == POPOP && optype1 == PUSHOP)
    {
      operands[1] = XEXP (XEXP (operands[1], 0), 0);

      handle_reg_adjust (operands[1], -size);

      if (GET_MODE (operands[1]) == XFmode)
	operands[1] = gen_rtx_MEM (XFmode, operands[1]);
      else if (GET_MODE (operands[1]) == DFmode)
	operands[1] = gen_rtx_MEM (DFmode, operands[1]);
      else
	operands[1] = gen_rtx_MEM (DImode, operands[1]);
      optype1 = OFFSOP;
    }

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (operands[0], 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (operands[1], 0));

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first,
     but if either operand is autodecrementing then we
     do the high-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (size == 12)
    {
      if (optype0 == REGOP)
	{
	  latehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 2);
	  middlehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
	}
      else if (optype0 == OFFSOP)
	{
	  middlehalf[0] = adjust_address (operands[0], SImode, 4);
	  latehalf[0] = adjust_address (operands[0], SImode, size - 4);
	}
      else
	{
	  middlehalf[0] = adjust_address (operands[0], SImode, 0);
	  latehalf[0] = adjust_address (operands[0], SImode, 0);
	}

      if (optype1 == REGOP)
	{
	  latehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 2);
	  middlehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
	}
      else if (optype1 == OFFSOP)
	{
	  middlehalf[1] = adjust_address (operands[1], SImode, 4);
	  latehalf[1] = adjust_address (operands[1], SImode, size - 4);
	}
      else if (optype1 == CNSTOP)
	{
	  if (GET_CODE (operands[1]) == CONST_DOUBLE)
	    {
	      long l[3];

	      REAL_VALUE_TO_TARGET_LONG_DOUBLE
		(*CONST_DOUBLE_REAL_VALUE (operands[1]), l);
	      operands[1] = GEN_INT (l[0]);
	      middlehalf[1] = GEN_INT (l[1]);
	      latehalf[1] = GEN_INT (l[2]);
	    }
	  else
	    {
	      /* No non-CONST_DOUBLE constant should ever appear
		 here.  */
	      gcc_assert (!CONSTANT_P (operands[1]));
	    }
	}
      else
	{
	  middlehalf[1] = adjust_address (operands[1], SImode, 0);
	  latehalf[1] = adjust_address (operands[1], SImode, 0);
	}
    }
  else
    /* size is not 12: */
    {
      if (optype0 == REGOP)
	latehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
      else if (optype0 == OFFSOP)
	latehalf[0] = adjust_address (operands[0], SImode, size - 4);
      else
	latehalf[0] = adjust_address (operands[0], SImode, 0);

      if (optype1 == REGOP)
	latehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
      else if (optype1 == OFFSOP)
	latehalf[1] = adjust_address (operands[1], SImode, size - 4);
      else if (optype1 == CNSTOP)
	split_double (operands[1], &operands[1], &latehalf[1]);
      else
	latehalf[1] = adjust_address (operands[1], SImode, 0);
    }

  /* If insn is effectively movd N(REG),-(REG) then we will do the high
     word first.  We should use the adjusted operand 1 (which is N+4(REG))
     for the low word as well, to compensate for the first decrement of
     REG.  */
  if (optype0 == PUSHOP
      && reg_overlap_mentioned_p (XEXP (XEXP (operands[0], 0), 0), operands[1]))
    operands[1] = middlehalf[1] = latehalf[1];

  /* For (set (reg:DI N) (mem:DI ... (reg:SI N) ...)),
     if the upper part of reg N does not appear in the MEM, arrange to
     emit the move late-half first.  Otherwise, compute the MEM address
     into the upper part of N and use that as a pointer to the memory
     operand.  */
  if (optype0 == REGOP
      && (optype1 == OFFSOP || optype1 == MEMOP))
    {
      rtx testlow = gen_rtx_REG (SImode, REGNO (operands[0]));

      if (reg_overlap_mentioned_p (testlow, XEXP (operands[1], 0))
	  && reg_overlap_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	{
	  /* If both halves of dest are used in the src memory address,
	     compute the address into latehalf of dest.
	     Note that this can't happen if the dest is two data regs.  */
	compadr:
	  xops[0] = latehalf[0];
	  xops[1] = XEXP (operands[1], 0);

	  handle_compadr (xops);
	  if (GET_MODE (operands[1]) == XFmode)
	    {
	      operands[1] = gen_rtx_MEM (XFmode, latehalf[0]);
	      middlehalf[1] = adjust_address (operands[1], DImode, size - 8);
	      latehalf[1] = adjust_address (operands[1], DImode, size - 4);
	    }
	  else
	    {
	      operands[1] = gen_rtx_MEM (DImode, latehalf[0]);
	      latehalf[1] = adjust_address (operands[1], DImode, size - 4);
	    }
	}
      else if (size == 12
	       && reg_overlap_mentioned_p (middlehalf[0],
					   XEXP (operands[1], 0)))
	{
	  /* Check for two regs used by both source and dest.
	     Note that this can't happen if the dest is all data regs.
	     It can happen if the dest is d6, d7, a0.
	     But in that case, latehalf is an addr reg, so
	     the code at compadr does ok.  */

	  if (reg_overlap_mentioned_p (testlow, XEXP (operands[1], 0))
	      || reg_overlap_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	    goto compadr;

	  /* JRV says this can't happen: */
	  gcc_assert (!addreg0 && !addreg1);

	  /* Only the middle reg conflicts; simply put it last.  */
	  handle_movsi (operands);
	  handle_movsi (latehalf);
	  handle_movsi (middlehalf);

	  return;
	}
      else if (reg_overlap_mentioned_p (testlow, XEXP (operands[1], 0)))
	/* If the low half of dest is mentioned in the source memory
	   address, the arrange to emit the move late half first.  */
	dest_overlapped_low = 1;
    }

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */

  /* Likewise,  the first move would clobber the source of the second one,
     do them in the other order.  This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance.  */

  if (optype0 == PUSHOP || optype1 == PUSHOP
      || (optype0 == REGOP && optype1 == REGOP
	  && ((middlehalf[1] && REGNO (operands[0]) == REGNO (middlehalf[1]))
	      || REGNO (operands[0]) == REGNO (latehalf[1])))
      || dest_overlapped_low)
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	handle_reg_adjust (addreg0, size - 4);
      if (addreg1)
	handle_reg_adjust (addreg1, size - 4);

      /* Do that word.  */
      handle_movsi (latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	handle_reg_adjust (addreg0, -4);
      if (addreg1)
	handle_reg_adjust (addreg1, -4);

      if (size == 12)
	{
	  handle_movsi (middlehalf);

	  if (addreg0)
	    handle_reg_adjust (addreg0, -4);
	  if (addreg1)
	    handle_reg_adjust (addreg1, -4);
	}

      /* Do low-numbered word.  */

      handle_movsi (operands);
      return;
    }

  /* Normal case: do the two words, low-numbered first.  */

  m68k_final_prescan_insn (NULL, operands, 2);
  handle_movsi (operands);

  /* Do the middle one of the three words for long double */
  if (size == 12)
    {
      if (addreg0)
	handle_reg_adjust (addreg0, 4);
      if (addreg1)
	handle_reg_adjust (addreg1, 4);

      m68k_final_prescan_insn (NULL, middlehalf, 2);
      handle_movsi (middlehalf);
    }

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    handle_reg_adjust (addreg0, 4);
  if (addreg1)
    handle_reg_adjust (addreg1, 4);

  /* Do that word.  */
  m68k_final_prescan_insn (NULL, latehalf, 2);
  handle_movsi (latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    handle_reg_adjust (addreg0, -(size - 4));
  if (addreg1)
    handle_reg_adjust (addreg1, -(size - 4));

  return;
}

/* Output assembler code to adjust REG by N.  */
static void
output_reg_adjust (rtx reg, int n)
{
  const char *s;

  gcc_assert (GET_MODE (reg) == SImode && n >= -12 && n != 0 && n <= 12);

  switch (n)
    {
    case 12:
      s = "add%.l #12,%0";
      break;

    case 8:
      s = "addq%.l #8,%0";
      break;

    case 4:
      s = "addq%.l #4,%0";
      break;

    case -12:
      s = "sub%.l #12,%0";
      break;

    case -8:
      s = "subq%.l #8,%0";
      break;

    case -4:
      s = "subq%.l #4,%0";
      break;

    default:
      gcc_unreachable ();
      s = NULL;
    }

  output_asm_insn (s, &reg);
}

/* Emit rtl code to adjust REG by N.  */
static void
emit_reg_adjust (rtx reg1, int n)
{
  rtx reg2;

  gcc_assert (GET_MODE (reg1) == SImode && n >= -12 && n != 0 && n <= 12);

  reg1 = copy_rtx (reg1);
  reg2 = copy_rtx (reg1);

  if (n < 0)
    emit_insn (gen_subsi3 (reg1, reg2, GEN_INT (-n)));
  else if (n > 0)
    emit_insn (gen_addsi3 (reg1, reg2, GEN_INT (n)));
  else
    gcc_unreachable ();
}

/* Output assembler to load address OPERANDS[0] to register OPERANDS[1].  */
static void
output_compadr (rtx operands[2])
{
  output_asm_insn ("lea %a1,%0", operands);
}

/* Output the best assembler insn for moving operands[1] into operands[0]
   as a fullword.  */
static void
output_movsi (rtx operands[2])
{
  output_asm_insn (singlemove_string (operands), operands);
}

/* Copy OP and change its mode to MODE.  */
static rtx
copy_operand (rtx op, machine_mode mode)
{
  /* ??? This looks really ugly.  There must be a better way
     to change a mode on the operand.  */
  if (GET_MODE (op) != VOIDmode)
    {
      if (REG_P (op))
	op = gen_rtx_REG (mode, REGNO (op));
      else
	{
	  op = copy_rtx (op);
	  PUT_MODE (op, mode);
	}
    }

  return op;
}

/* Emit rtl code for moving operands[1] into operands[0] as a fullword.  */
static void
emit_movsi (rtx operands[2])
{
  operands[0] = copy_operand (operands[0], SImode);
  operands[1] = copy_operand (operands[1], SImode);

  emit_insn (gen_movsi (operands[0], operands[1]));
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */
const char *
output_move_double (rtx *operands)
{
  handle_move_double (operands,
		      output_reg_adjust, output_compadr, output_movsi);

  return "";
}

/* Output rtl code to perform a doubleword move insn
   with operands OPERANDS.  */
void
m68k_emit_move_double (rtx operands[2])
{
  handle_move_double (operands, emit_reg_adjust, emit_movsi, emit_movsi);
}

/* Ensure mode of ORIG, a REG rtx, is MODE.  Returns either ORIG or a
   new rtx with the correct mode.  */

static rtx
force_mode (machine_mode mode, rtx orig)
{
  if (mode == GET_MODE (orig))
    return orig;

  if (REGNO (orig) >= FIRST_PSEUDO_REGISTER)
    abort ();

  return gen_rtx_REG (mode, REGNO (orig));
}

static int
fp_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return reg_renumber && FP_REG_P (op);
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.

   Note SCRATCH_REG may not be in the proper mode depending on how it
   will be used.  This routine is responsible for creating a new copy
   of SCRATCH_REG in the proper mode.  */

int
emit_move_sequence (rtx *operands, machine_mode mode, rtx scratch_reg)
{
  register rtx operand0 = operands[0];
  register rtx operand1 = operands[1];
  register rtx tem;

  if (scratch_reg
      && reload_in_progress && GET_CODE (operand0) == REG
      && REGNO (operand0) >= FIRST_PSEUDO_REGISTER)
    operand0 = reg_equiv_mem (REGNO (operand0));
  else if (scratch_reg
	   && reload_in_progress && GET_CODE (operand0) == SUBREG
	   && GET_CODE (SUBREG_REG (operand0)) == REG
	   && REGNO (SUBREG_REG (operand0)) >= FIRST_PSEUDO_REGISTER)
    {
     /* We must not alter SUBREG_BYTE (operand0) since that would confuse
	the code which tracks sets/uses for delete_output_reload.  */
      rtx temp = gen_rtx_SUBREG (GET_MODE (operand0),
				 reg_equiv_mem (REGNO (SUBREG_REG (operand0))),
				 SUBREG_BYTE (operand0));
      operand0 = alter_subreg (&temp, true);
    }

  if (scratch_reg
      && reload_in_progress && GET_CODE (operand1) == REG
      && REGNO (operand1) >= FIRST_PSEUDO_REGISTER)
    operand1 = reg_equiv_mem (REGNO (operand1));
  else if (scratch_reg
	   && reload_in_progress && GET_CODE (operand1) == SUBREG
	   && GET_CODE (SUBREG_REG (operand1)) == REG
	   && REGNO (SUBREG_REG (operand1)) >= FIRST_PSEUDO_REGISTER)
    {
     /* We must not alter SUBREG_BYTE (operand0) since that would confuse
	the code which tracks sets/uses for delete_output_reload.  */
      rtx temp = gen_rtx_SUBREG (GET_MODE (operand1),
				 reg_equiv_mem (REGNO (SUBREG_REG (operand1))),
				 SUBREG_BYTE (operand1));
      operand1 = alter_subreg (&temp, true);
    }

  if (scratch_reg && reload_in_progress && GET_CODE (operand0) == MEM
      && ((tem = find_replacement (&XEXP (operand0, 0)))
	  != XEXP (operand0, 0)))
    operand0 = gen_rtx_MEM (GET_MODE (operand0), tem);
  if (scratch_reg && reload_in_progress && GET_CODE (operand1) == MEM
      && ((tem = find_replacement (&XEXP (operand1, 0)))
	  != XEXP (operand1, 0)))
    operand1 = gen_rtx_MEM (GET_MODE (operand1), tem);

  /* Handle secondary reloads for loads/stores of FP registers where
     the address is symbolic by using the scratch register */
  if (fp_reg_operand (operand0, mode)
      && ((GET_CODE (operand1) == MEM
	   && ! memory_address_p (DFmode, XEXP (operand1, 0)))
	  || ((GET_CODE (operand1) == SUBREG
	       && GET_CODE (XEXP (operand1, 0)) == MEM
	       && !memory_address_p (DFmode, XEXP (XEXP (operand1, 0), 0)))))
      && scratch_reg)
    {
      if (GET_CODE (operand1) == SUBREG)
	operand1 = XEXP (operand1, 0);

      /* SCRATCH_REG will hold an address.  We want
	 it in SImode regardless of what mode it was originally given
	 to us.  */
      scratch_reg = force_mode (SImode, scratch_reg);

      /* D might not fit in 14 bits either; for such cases load D into
	 scratch reg.  */
      if (!memory_address_p (Pmode, XEXP (operand1, 0)))
	{
	  emit_move_insn (scratch_reg, XEXP (XEXP (operand1, 0), 1));
	  emit_move_insn (scratch_reg, gen_rtx_fmt_ee (GET_CODE (XEXP (operand1, 0)),
						       Pmode,
						       XEXP (XEXP (operand1, 0), 0),
						       scratch_reg));
	}
      else
	emit_move_insn (scratch_reg, XEXP (operand1, 0));
      emit_insn (gen_rtx_SET (operand0, gen_rtx_MEM (mode, scratch_reg)));
      return 1;
    }
  else if (fp_reg_operand (operand1, mode)
	   && ((GET_CODE (operand0) == MEM
		&& ! memory_address_p (DFmode, XEXP (operand0, 0)))
	       || ((GET_CODE (operand0) == SUBREG)
		   && GET_CODE (XEXP (operand0, 0)) == MEM
		   && !memory_address_p (DFmode, XEXP (XEXP (operand0, 0), 0))))
	   && scratch_reg)
    {
      if (GET_CODE (operand0) == SUBREG)
	operand0 = XEXP (operand0, 0);

      /* SCRATCH_REG will hold an address and maybe the actual data.  We want
	 it in SIMODE regardless of what mode it was originally given
	 to us.  */
      scratch_reg = force_mode (SImode, scratch_reg);

      /* D might not fit in 14 bits either; for such cases load D into
	 scratch reg.  */
      if (!memory_address_p (Pmode, XEXP (operand0, 0)))
	{
	  emit_move_insn (scratch_reg, XEXP (XEXP (operand0, 0), 1));
	  emit_move_insn (scratch_reg, gen_rtx_fmt_ee (GET_CODE (XEXP (operand0,
								        0)),
						       Pmode,
						       XEXP (XEXP (operand0, 0),
								   0),
						       scratch_reg));
	}
      else
	emit_move_insn (scratch_reg, XEXP (operand0, 0));
      emit_insn (gen_rtx_SET (gen_rtx_MEM (mode, scratch_reg), operand1));
      return 1;
    }
  /* Handle secondary reloads for loads of FP registers from constant
     expressions by forcing the constant into memory.

     use scratch_reg to hold the address of the memory location.

     The proper fix is to change PREFERRED_RELOAD_CLASS to return
     NO_REGS when presented with a const_int and an register class
     containing only FP registers.  Doing so unfortunately creates
     more problems than it solves.   Fix this for 2.5.  */
  else if (fp_reg_operand (operand0, mode)
	   && CONSTANT_P (operand1)
	   && scratch_reg)
    {
      rtx xoperands[2];

      /* SCRATCH_REG will hold an address and maybe the actual data.  We want
	 it in SIMODE regardless of what mode it was originally given
	 to us.  */
      scratch_reg = force_mode (SImode, scratch_reg);

      /* Force the constant into memory and put the address of the
	 memory location into scratch_reg.  */
      xoperands[0] = scratch_reg;
      xoperands[1] = XEXP (force_const_mem (mode, operand1), 0);
      emit_insn (gen_rtx_SET (scratch_reg, xoperands[1]));

      /* Now load the destination register.  */
      emit_insn (gen_rtx_SET (operand0, gen_rtx_MEM (mode, scratch_reg)));
      return 1;
    }

  /* Now have insn-emit do whatever it normally does.  */
  return 0;
}

/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands".  */

void
split_di (rtx operands[], int num, rtx lo_half[], rtx hi_half[])
{
  while (num--)
    {
      rtx op = operands[num];

      /* simplify_subreg refuses to split volatile memory addresses,
	 but we still have to handle it.  */
      if (GET_CODE (op) == MEM)
	{
	  lo_half[num] = adjust_address (op, SImode, 4);
	  hi_half[num] = adjust_address (op, SImode, 0);
	}
      else
	{
	  lo_half[num] = simplify_gen_subreg (SImode, op,
					      GET_MODE (op) == VOIDmode
					      ? DImode : GET_MODE (op), 4);
	  hi_half[num] = simplify_gen_subreg (SImode, op,
					      GET_MODE (op) == VOIDmode
					      ? DImode : GET_MODE (op), 0);
	}
    }
}

/* Split X into a base and a constant offset, storing them in *BASE
   and *OFFSET respectively.  */

static void
m68k_split_offset (rtx x, rtx *base, HOST_WIDE_INT *offset)
{
  *offset = 0;
  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      *offset += INTVAL (XEXP (x, 1));
      x = XEXP (x, 0);
    }
  *base = x;
}

/* Return true if PATTERN is a PARALLEL suitable for a movem or fmovem
   instruction.  STORE_P says whether the move is a load or store.

   If the instruction uses post-increment or pre-decrement addressing,
   AUTOMOD_BASE is the base register and AUTOMOD_OFFSET is the total
   adjustment.  This adjustment will be made by the first element of
   PARALLEL, with the loads or stores starting at element 1.  If the
   instruction does not use post-increment or pre-decrement addressing,
   AUTOMOD_BASE is null, AUTOMOD_OFFSET is 0, and the loads or stores
   start at element 0.  */

bool
m68k_movem_pattern_p (rtx pattern, rtx automod_base,
		      HOST_WIDE_INT automod_offset, bool store_p)
{
  rtx base, mem_base, set, mem, reg, last_reg;
  HOST_WIDE_INT offset, mem_offset;
  int i, first, len;
  enum reg_class rclass;

  len = XVECLEN (pattern, 0);
  first = (automod_base != NULL);

  if (automod_base)
    {
      /* Stores must be pre-decrement and loads must be post-increment.  */
      if (store_p != (automod_offset < 0))
	return false;

      /* Work out the base and offset for lowest memory location.  */
      base = automod_base;
      offset = (automod_offset < 0 ? automod_offset : 0);
    }
  else
    {
      /* Allow any valid base and offset in the first access.  */
      base = NULL;
      offset = 0;
    }

  last_reg = NULL;
  rclass = NO_REGS;
  for (i = first; i < len; i++)
    {
      /* We need a plain SET.  */
      set = XVECEXP (pattern, 0, i);
      if (GET_CODE (set) != SET)
	return false;

      /* Check that we have a memory location...  */
      mem = XEXP (set, !store_p);
      if (!MEM_P (mem) || !memory_operand (mem, VOIDmode))
	return false;

      /* ...with the right address.  */
      if (base == NULL)
	{
	  m68k_split_offset (XEXP (mem, 0), &base, &offset);
	  /* The ColdFire instruction only allows (An) and (d16,An) modes.
	     There are no mode restrictions for 680x0 besides the
	     automodification rules enforced above.  */
	  if (TARGET_COLDFIRE
	      && !m68k_legitimate_base_reg_p (base, reload_completed))
	    return false;
	}
      else
	{
	  m68k_split_offset (XEXP (mem, 0), &mem_base, &mem_offset);
	  if (!rtx_equal_p (base, mem_base) || offset != mem_offset)
	    return false;
	}

      /* Check that we have a register of the required mode and class.  */
      reg = XEXP (set, store_p);
      if (!REG_P (reg)
	  || !HARD_REGISTER_P (reg)
	  || GET_MODE (reg) != reg_raw_mode[REGNO (reg)])
	return false;

      if (last_reg)
	{
	  /* The register must belong to RCLASS and have a higher number
	     than the register in the previous SET.  */
	  if (!TEST_HARD_REG_BIT (reg_class_contents[rclass], REGNO (reg))
	      || REGNO (last_reg) >= REGNO (reg))
	    return false;
	}
      else
	{
	  /* Work out which register class we need.  */
	  if (INT_REGNO_P (REGNO (reg)))
	    rclass = GENERAL_REGS;
	  else if (FP_REGNO_P (REGNO (reg)))
	    rclass = FP_REGS;
	  else
	    return false;
	}

      last_reg = reg;
      offset += GET_MODE_SIZE (GET_MODE (reg));
    }

  /* If we have an automodification, check whether the final offset is OK.  */
  if (automod_base && offset != (automod_offset < 0 ? 0 : automod_offset))
    return false;

  /* Reject unprofitable cases.  */
  if (len < first + (rclass == FP_REGS ? MIN_FMOVEM_REGS : MIN_MOVEM_REGS))
    return false;

  return true;
}

/* Return the assembly code template for a movem or fmovem instruction
   whose pattern is given by PATTERN.  Store the template's operands
   in OPERANDS.

   If the instruction uses post-increment or pre-decrement addressing,
   AUTOMOD_OFFSET is the total adjustment, otherwise it is 0.  STORE_P
   is true if this is a store instruction.  */

const char *
m68k_output_movem (rtx *operands, rtx pattern,
		   HOST_WIDE_INT automod_offset, bool store_p)
{
  unsigned int mask;
  int i, first;

  gcc_assert (GET_CODE (pattern) == PARALLEL);
  mask = 0;
  first = (automod_offset != 0);
  for (i = first; i < XVECLEN (pattern, 0); i++)
    {
      /* When using movem with pre-decrement addressing, register X + D0_REG
	 is controlled by bit 15 - X.  For all other addressing modes,
	 register X + D0_REG is controlled by bit X.  Confusingly, the
	 register mask for fmovem is in the opposite order to that for
	 movem.  */
      unsigned int regno;

      gcc_assert (MEM_P (XEXP (XVECEXP (pattern, 0, i), !store_p)));
      gcc_assert (REG_P (XEXP (XVECEXP (pattern, 0, i), store_p)));
      regno = REGNO (XEXP (XVECEXP (pattern, 0, i), store_p));
      if (automod_offset < 0)
	{
	  if (FP_REGNO_P (regno))
	    mask |= 1 << (regno - FP0_REG);
	  else
	    mask |= 1 << (15 - (regno - D0_REG));
	}
      else
	{
	  if (FP_REGNO_P (regno))
	    mask |= 1 << (7 - (regno - FP0_REG));
	  else
	    mask |= 1 << (regno - D0_REG);
	}
    }
  CC_STATUS_INIT;

  if (automod_offset == 0)
    operands[0] = XEXP (XEXP (XVECEXP (pattern, 0, first), !store_p), 0);
  else if (automod_offset < 0)
    operands[0] = gen_rtx_PRE_DEC (Pmode, SET_DEST (XVECEXP (pattern, 0, 0)));
  else
    operands[0] = gen_rtx_POST_INC (Pmode, SET_DEST (XVECEXP (pattern, 0, 0)));
  operands[1] = GEN_INT (mask);
  if (FP_REGNO_P (REGNO (XEXP (XVECEXP (pattern, 0, first), store_p))))
    {
      if (store_p)
	return "fmovem %1,%a0";
      else
	return "fmovem %a0,%1";
    }
  else
    {
      if (store_p)
	return "movem%.l %1,%a0";
      else
	return "movem%.l %a0,%1";
    }
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (rtx addr)
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	gcc_unreachable ();
    }
  gcc_assert (GET_CODE (addr) == REG);
  return addr;
}

/* Output assembler code to perform a 32-bit 3-operand add.  */

const char *
output_addsi3 (rtx *operands)
{
  if (! operands_match_p (operands[0], operands[1]))
    {
      if (!ADDRESS_REG_P (operands[1]))
	{
	  rtx tmp = operands[1];

	  operands[1] = operands[2];
	  operands[2] = tmp;
	}

      /* These insns can result from reloads to access
	 stack slots over 64k from the frame pointer.  */
      if (GET_CODE (operands[2]) == CONST_INT
	  && (INTVAL (operands[2]) < -32768 || INTVAL (operands[2]) > 32767))
        return "move%.l %2,%0\n\tadd%.l %1,%0";
      if (GET_CODE (operands[2]) == REG)
	return MOTOROLA ? "lea (%1,%2.l),%0" : "lea %1@(0,%2:l),%0";
      return MOTOROLA ? "lea (%c2,%1),%0" : "lea %1@(%c2),%0";
    }
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL (operands[2]) > 0
	  && INTVAL (operands[2]) <= 8)
	return "addq%.l %2,%0";
      if (INTVAL (operands[2]) < 0
	  && INTVAL (operands[2]) >= -8)
        {
	  operands[2] = GEN_INT (- INTVAL (operands[2]));
	  return "subq%.l %2,%0";
	}
      /* On the CPU32 it is faster to use two addql instructions to
	 add a small integer (8 < N <= 16) to a register.
	 Likewise for subql.  */
      if (TUNE_CPU32 && REG_P (operands[0]))
	{
	  if (INTVAL (operands[2]) > 8
	      && INTVAL (operands[2]) <= 16)
	    {
	      operands[2] = GEN_INT (INTVAL (operands[2]) - 8);
	      return "addq%.l #8,%0\n\taddq%.l %2,%0";
	    }
	  if (INTVAL (operands[2]) < -8
	      && INTVAL (operands[2]) >= -16)
	    {
	      operands[2] = GEN_INT (- INTVAL (operands[2]) - 8);
	      return "subq%.l #8,%0\n\tsubq%.l %2,%0";
	    }
	}
      if (ADDRESS_REG_P (operands[0])
	  && INTVAL (operands[2]) >= -0x8000
	  && INTVAL (operands[2]) < 0x8000)
	{
	  if (TUNE_68040)
	    return "add%.w %2,%0";
	  else
	    return MOTOROLA ? "lea (%c2,%0),%0" : "lea %0@(%c2),%0";
	}
    }
  return "add%.l %2,%0";
}

/* Store in cc_status the expressions that the condition codes will
   describe after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* On the 68000, all the insns to store in an address register fail to
   set the cc's.  However, in some cases these instructions can make it
   possibly invalid to use the saved cc's.  In those cases we clear out
   some or all of the saved cc's so they won't be used.  */

void
notice_update_cc (rtx exp, rtx insn)
{
  if (GET_CODE (exp) == SET)
    {
      if (GET_CODE (SET_SRC (exp)) == CALL)
	CC_STATUS_INIT; 
      else if (ADDRESS_REG_P (SET_DEST (exp)))
	{
	  if (cc_status.value1 && modified_in_p (cc_status.value1, insn))
	    cc_status.value1 = 0;
	  if (cc_status.value2 && modified_in_p (cc_status.value2, insn))
	    cc_status.value2 = 0; 
	}
      /* fmoves to memory or data registers do not set the condition
	 codes.  Normal moves _do_ set the condition codes, but not in
	 a way that is appropriate for comparison with 0, because -0.0
	 would be treated as a negative nonzero number.  Note that it
	 isn't appropriate to conditionalize this restriction on
	 HONOR_SIGNED_ZEROS because that macro merely indicates whether
	 we care about the difference between -0.0 and +0.0.  */
      else if (!FP_REG_P (SET_DEST (exp))
	       && SET_DEST (exp) != cc0_rtx
	       && (FP_REG_P (SET_SRC (exp))
		   || GET_CODE (SET_SRC (exp)) == FIX
		   || FLOAT_MODE_P (GET_MODE (SET_DEST (exp)))))
	CC_STATUS_INIT; 
      /* A pair of move insns doesn't produce a useful overall cc.  */
      else if (!FP_REG_P (SET_DEST (exp))
	       && !FP_REG_P (SET_SRC (exp))
	       && GET_MODE_SIZE (GET_MODE (SET_SRC (exp))) > 4
	       && (GET_CODE (SET_SRC (exp)) == REG
		   || GET_CODE (SET_SRC (exp)) == MEM
		   || GET_CODE (SET_SRC (exp)) == CONST_DOUBLE))
	CC_STATUS_INIT; 
      else if (SET_DEST (exp) != pc_rtx)
	{
	  cc_status.flags = 0;
	  cc_status.value1 = SET_DEST (exp);
	  cc_status.value2 = SET_SRC (exp);
	}
    }
  else if (GET_CODE (exp) == PARALLEL
	   && GET_CODE (XVECEXP (exp, 0, 0)) == SET)
    {
      rtx dest = SET_DEST (XVECEXP (exp, 0, 0));
      rtx src  = SET_SRC  (XVECEXP (exp, 0, 0));

      if (ADDRESS_REG_P (dest))
	CC_STATUS_INIT;
      else if (dest != pc_rtx)
	{
	  cc_status.flags = 0;
	  cc_status.value1 = dest;
	  cc_status.value2 = src;
	}
    }
  else
    CC_STATUS_INIT;
  if (cc_status.value2 != 0
      && ADDRESS_REG_P (cc_status.value2)
      && GET_MODE (cc_status.value2) == QImode)
    CC_STATUS_INIT;
  if (cc_status.value2 != 0)
    switch (GET_CODE (cc_status.value2))
      {
      case ASHIFT: case ASHIFTRT: case LSHIFTRT:
      case ROTATE: case ROTATERT:
	/* These instructions always clear the overflow bit, and set
	   the carry to the bit shifted out.  */
	cc_status.flags |= CC_OVERFLOW_UNUSABLE | CC_NO_CARRY;
	break;

      case PLUS: case MINUS: case MULT:
      case DIV: case UDIV: case MOD: case UMOD: case NEG:
	if (GET_MODE (cc_status.value2) != VOIDmode)
	  cc_status.flags |= CC_NO_OVERFLOW;
	break;
      case ZERO_EXTEND:
	/* (SET r1 (ZERO_EXTEND r2)) on this machine
	   ends with a move insn moving r2 in r2's mode.
	   Thus, the cc's are set for r2.
	   This can set N bit spuriously.  */
	cc_status.flags |= CC_NOT_NEGATIVE; 

      default:
	break;
      }
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG
      && cc_status.value2
      && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))
    cc_status.value2 = 0;
  /* Check for PRE_DEC in dest modifying a register used in src.  */
  if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM
      && GET_CODE (XEXP (cc_status.value1, 0)) == PRE_DEC
      && cc_status.value2
      && reg_overlap_mentioned_p (XEXP (XEXP (cc_status.value1, 0), 0),
				  cc_status.value2))
    cc_status.value2 = 0;
  if (((cc_status.value1 && FP_REG_P (cc_status.value1))
       || (cc_status.value2 && FP_REG_P (cc_status.value2))))
    cc_status.flags = CC_IN_68881;
  if (cc_status.value2 && GET_CODE (cc_status.value2) == COMPARE
      && GET_MODE_CLASS (GET_MODE (XEXP (cc_status.value2, 0))) == MODE_FLOAT)
    {
      cc_status.flags = CC_IN_68881;
      if (!FP_REG_P (XEXP (cc_status.value2, 0))
	  && FP_REG_P (XEXP (cc_status.value2, 1)))
	cc_status.flags |= CC_REVERSED;
    }
}

const char *
output_move_const_double (rtx *operands)
{
  int code = standard_68881_constant_p (operands[1]);

  if (code != 0)
    {
      static char buf[40];

      sprintf (buf, "fmovecr #0x%x,%%0", code & 0xff);
      return buf;
    }
  return "fmove%.d %1,%0";
}

const char *
output_move_const_single (rtx *operands)
{
  int code = standard_68881_constant_p (operands[1]);

  if (code != 0)
    {
      static char buf[40];

      sprintf (buf, "fmovecr #0x%x,%%0", code & 0xff);
      return buf;
    }
  return "fmove%.s %f1,%0";
}

/* Return nonzero if X, a CONST_DOUBLE, has a value that we can get
   from the "fmovecr" instruction.
   The value, anded with 0xff, gives the code to use in fmovecr
   to get the desired constant.  */

/* This code has been fixed for cross-compilation.  */
  
static int inited_68881_table = 0;

static const char *const strings_68881[7] = {
  "0.0",
  "1.0",
  "10.0",
  "100.0",
  "10000.0",
  "1e8",
  "1e16"
};

static const int codes_68881[7] = {
  0x0f,
  0x32,
  0x33,
  0x34,
  0x35,
  0x36,
  0x37
};

REAL_VALUE_TYPE values_68881[7];

/* Set up values_68881 array by converting the decimal values
   strings_68881 to binary.  */

void
init_68881_table (void)
{
  int i;
  REAL_VALUE_TYPE r;
  machine_mode mode;

  mode = SFmode;
  for (i = 0; i < 7; i++)
    {
      if (i == 6)
        mode = DFmode;
      r = REAL_VALUE_ATOF (strings_68881[i], mode);
      values_68881[i] = r;
    }
  inited_68881_table = 1;
}

int
standard_68881_constant_p (rtx x)
{
  const REAL_VALUE_TYPE *r;
  int i;

  /* fmovecr must be emulated on the 68040 and 68060, so it shouldn't be
     used at all on those chips.  */
  if (TUNE_68040_60)
    return 0;

  if (! inited_68881_table)
    init_68881_table ();

  r = CONST_DOUBLE_REAL_VALUE (x);

  /* Use real_identical instead of real_equal so that -0.0 is rejected.  */
  for (i = 0; i < 6; i++)
    {
      if (real_identical (r, &values_68881[i]))
        return (codes_68881[i]);
    }
  
  if (GET_MODE (x) == SFmode)
    return 0;

  if (real_equal (r, &values_68881[6]))
    return (codes_68881[6]);

  /* larger powers of ten in the constants ram are not used
     because they are not equal to a `double' C constant.  */
  return 0;
}

/* If X is a floating-point constant, return the logarithm of X base 2,
   or 0 if X is not a power of 2.  */

int
floating_exact_log2 (rtx x)
{
  const REAL_VALUE_TYPE *r;
  REAL_VALUE_TYPE r1;
  int exp;

  r = CONST_DOUBLE_REAL_VALUE (x);

  if (real_less (r, &dconst1))
    return 0;

  exp = real_exponent (r);
  real_2expN (&r1, exp, DFmode);
  if (real_equal (&r1, r))
    return exp;

  return 0;
}

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  X is an RTL
   expression.

   CODE is a value that can be used to specify one of several ways
   of printing the operand.  It is used when identical operands
   must be printed differently depending on the context.  CODE
   comes from the `%' specification that was used to request
   printing of the operand.  If the specification was just `%DIGIT'
   then CODE is 0; if the specification was `%LTR DIGIT' then CODE
   is the ASCII code for LTR.

   If X is a register, this macro should print the register's name.
   The names can be found in an array `reg_names' whose type is
   `char *[]'.  `reg_names' is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%'
   followed by a punctuation character), this macro is called with
   a null pointer for X and the punctuation character for CODE.

   The m68k specific codes are:

   '.' for dot needed in Motorola-style opcode names.
   '-' for an operand pushing on the stack:
       sp@-, -(sp) or -(%sp) depending on the style of syntax.
   '+' for an operand pushing on the stack:
       sp@+, (sp)+ or (%sp)+ depending on the style of syntax.
   '@' for a reference to the top word on the stack:
       sp@, (sp) or (%sp) depending on the style of syntax.
   '#' for an immediate operand prefix (# in MIT and Motorola syntax
       but & in SGS syntax).
   '!' for the cc register (used in an `and to cc' insn).
   '$' for the letter `s' in an op code, but only on the 68040.
   '&' for the letter `d' in an op code, but only on the 68040.
   '/' for register prefix needed by longlong.h.
   '?' for m68k_library_id_string

   'b' for byte insn (no effect, on the Sun; this is for the ISI).
   'd' to force memory addressing to be absolute, not relative.
   'f' for float insn (print a CONST_DOUBLE as a float rather than in hex)
   'x' for float insn (print a CONST_DOUBLE as a float rather than in hex),
       or print pair of registers as rx:ry.
   'p' print an address with @PLTPC attached, but only if the operand
       is not locally-bound.  */

void
print_operand (FILE *file, rtx op, int letter)
{
  if (letter == '.')
    {
      if (MOTOROLA)
	fprintf (file, ".");
    }
  else if (letter == '#')
    asm_fprintf (file, "%I");
  else if (letter == '-')
    asm_fprintf (file, MOTOROLA ? "-(%Rsp)" : "%Rsp@-");
  else if (letter == '+')
    asm_fprintf (file, MOTOROLA ? "(%Rsp)+" : "%Rsp@+");
  else if (letter == '@')
    asm_fprintf (file, MOTOROLA ? "(%Rsp)" : "%Rsp@");
  else if (letter == '!')
    asm_fprintf (file, "%Rfpcr");
  else if (letter == '$')
    {
      if (TARGET_68040)
	fprintf (file, "s");
    }
  else if (letter == '&')
    {
      if (TARGET_68040)
	fprintf (file, "d");
    }
  else if (letter == '/')
    asm_fprintf (file, "%R");
  else if (letter == '?')
    asm_fprintf (file, m68k_library_id_string);
  else if (letter == 'p')
    {
      output_addr_const (file, op);
      if (!(GET_CODE (op) == SYMBOL_REF && SYMBOL_REF_LOCAL_P (op)))
	fprintf (file, "@PLTPC");
    }
  else if (GET_CODE (op) == REG)
    {
      if (letter == 'R')
	/* Print out the second register name of a register pair.
	   I.e., R (6) => 7.  */
	fputs (M68K_REGNAME(REGNO (op) + 1), file);
      else
	fputs (M68K_REGNAME(REGNO (op)), file);
    }
  else if (GET_CODE (op) == MEM)
    {
      output_address (GET_MODE (op), XEXP (op, 0));
      if (letter == 'd' && ! TARGET_68020
	  && CONSTANT_ADDRESS_P (XEXP (op, 0))
	  && !(GET_CODE (XEXP (op, 0)) == CONST_INT
	       && INTVAL (XEXP (op, 0)) < 0x8000
	       && INTVAL (XEXP (op, 0)) >= -0x8000))
	fprintf (file, MOTOROLA ? ".l" : ":l");
    }
  else if (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == SFmode)
    {
      long l;
      REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (op), l);
      asm_fprintf (file, "%I0x%lx", l & 0xFFFFFFFF);
    }
  else if (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == XFmode)
    {
      long l[3];
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (*CONST_DOUBLE_REAL_VALUE (op), l);
      asm_fprintf (file, "%I0x%lx%08lx%08lx", l[0] & 0xFFFFFFFF,
		   l[1] & 0xFFFFFFFF, l[2] & 0xFFFFFFFF);
    }
  else if (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == DFmode)
    {
      long l[2];
      REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (op), l);
      asm_fprintf (file, "%I0x%lx%08lx", l[0] & 0xFFFFFFFF, l[1] & 0xFFFFFFFF);
    }
  else
    {
      /* Use `print_operand_address' instead of `output_addr_const'
	 to ensure that we print relevant PIC stuff.  */
      asm_fprintf (file, "%I");
      if (TARGET_PCREL
	  && (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST))
	print_operand_address (file, op);
      else
	output_addr_const (file, op);
    }
}

/* Return string for TLS relocation RELOC.  */

static const char *
m68k_get_reloc_decoration (enum m68k_reloc reloc)
{
  /* To my knowledge, !MOTOROLA assemblers don't support TLS.  */
  gcc_assert (MOTOROLA || reloc == RELOC_GOT);

  switch (reloc)
    {
    case RELOC_GOT:
      if (MOTOROLA)
	{
	  if (flag_pic == 1 && TARGET_68020)
	    return "@GOT.w";
	  else
	    return "@GOT";
	}
      else
	{
	  if (TARGET_68020)
	    {
	      switch (flag_pic)
		{
		case 1:
		  return ":w";
		case 2:
		  return ":l";
		default:
		  return "";
		}
	    }
	}
      gcc_unreachable ();

    case RELOC_TLSGD:
      return "@TLSGD";

    case RELOC_TLSLDM:
      return "@TLSLDM";

    case RELOC_TLSLDO:
      return "@TLSLDO";

    case RELOC_TLSIE:
      return "@TLSIE";

    case RELOC_TLSLE:
      return "@TLSLE";

    default:
      gcc_unreachable ();
    }
}

/* m68k implementation of TARGET_OUTPUT_ADDR_CONST_EXTRA.  */

static bool
m68k_output_addr_const_extra (FILE *file, rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    {
      switch (XINT (x, 1))
	{
	case UNSPEC_RELOC16:
	case UNSPEC_RELOC32:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs (m68k_get_reloc_decoration
		 ((enum m68k_reloc) INTVAL (XVECEXP (x, 0, 1))), file);
	  return true;

	default:
	  break;
	}
    }

  return false;
}

/* M68K implementation of TARGET_ASM_OUTPUT_DWARF_DTPREL.  */

static void
m68k_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  gcc_assert (size == 4);
  fputs ("\t.long\t", file);
  output_addr_const (file, x);
  fputs ("@TLSLDO+0x8000", file);
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize various UNSPEC sequences
   and turn them back into a direct symbol reference.  */

static rtx
m68k_delegitimize_address (rtx orig_x)
{
  rtx x;
  struct m68k_address addr;
  rtx unspec;

  orig_x = delegitimize_mem_from_attrs (orig_x);
  x = orig_x;
  if (MEM_P (x))
    x = XEXP (x, 0);

  if (GET_CODE (x) != PLUS || GET_MODE (x) != Pmode)
    return orig_x;

  if (!m68k_decompose_address (GET_MODE (x), x, false, &addr)
      || addr.offset == NULL_RTX
      || GET_CODE (addr.offset) != CONST)
    return orig_x;

  unspec = XEXP (addr.offset, 0);
  if (GET_CODE (unspec) == PLUS && CONST_INT_P (XEXP (unspec, 1)))
    unspec = XEXP (unspec, 0);
  if (GET_CODE (unspec) != UNSPEC 
      || (XINT (unspec, 1) != UNSPEC_RELOC16
	  && XINT (unspec, 1) != UNSPEC_RELOC32))
    return orig_x;
  x = XVECEXP (unspec, 0, 0);
  gcc_assert (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF);
  if (unspec != XEXP (addr.offset, 0))
    x = gen_rtx_PLUS (Pmode, x, XEXP (XEXP (addr.offset, 0), 1));
  if (addr.index)
    {
      rtx idx = addr.index;
      if (addr.scale != 1)
	idx = gen_rtx_MULT (Pmode, idx, GEN_INT (addr.scale));
      x = gen_rtx_PLUS (Pmode, idx, x);
    }
  if (addr.base)
    x = gen_rtx_PLUS (Pmode, addr.base, x);
  if (MEM_P (orig_x))
    x = replace_equiv_address_nv (orig_x, x);
  return x;
}
  

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   Note that this contains a kludge that knows that the only reason
   we have an address (plus (label_ref...) (reg...)) when not generating
   PIC code is in the insn before a tablejump, and we know that m68k.md
   generates a label LInnn: on such an insn.

   It is possible for PIC to generate a (plus (label_ref...) (reg...))
   and we handle that just like we would a (plus (symbol_ref...) (reg...)).

   This routine is responsible for distinguishing between -fpic and -fPIC 
   style relocations in an address.  When generating -fpic code the
   offset is output in word mode (e.g. movel a5@(_foo:w), a0).  When generating
   -fPIC code the offset is output in long mode (e.g. movel a5@(_foo:l), a0) */

void
print_operand_address (FILE *file, rtx addr)
{
  struct m68k_address address;

  if (!m68k_decompose_address (QImode, addr, true, &address))
    gcc_unreachable ();

  if (address.code == PRE_DEC)
    fprintf (file, MOTOROLA ? "-(%s)" : "%s@-",
	     M68K_REGNAME (REGNO (address.base)));
  else if (address.code == POST_INC)
    fprintf (file, MOTOROLA ? "(%s)+" : "%s@+",
	     M68K_REGNAME (REGNO (address.base)));
  else if (!address.base && !address.index)
    {
      /* A constant address.  */
      gcc_assert (address.offset == addr);
      if (GET_CODE (addr) == CONST_INT)
	{
	  /* (xxx).w or (xxx).l.  */
	  if (IN_RANGE (INTVAL (addr), -0x8000, 0x7fff))
	    fprintf (file, MOTOROLA ? "%d.w" : "%d:w", (int) INTVAL (addr));
	  else
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (addr));
	}
      else if (TARGET_PCREL)
	{
	  /* (d16,PC) or (bd,PC,Xn) (with suppressed index register).  */
	  fputc ('(', file);
	  output_addr_const (file, addr);
	  asm_fprintf (file, flag_pic == 1 ? ":w,%Rpc)" : ":l,%Rpc)");
	}
      else
	{
	  /* (xxx).l.  We need a special case for SYMBOL_REF if the symbol
	     name ends in `.<letter>', as the last 2 characters can be
	     mistaken as a size suffix.  Put the name in parentheses.  */
	  if (GET_CODE (addr) == SYMBOL_REF
	      && strlen (XSTR (addr, 0)) > 2
	      && XSTR (addr, 0)[strlen (XSTR (addr, 0)) - 2] == '.')
	    {
	      putc ('(', file);
	      output_addr_const (file, addr);
	      putc (')', file);
	    }
	  else
	    output_addr_const (file, addr);
	}
    }
  else
    {
      int labelno;

      /* If ADDR is a (d8,pc,Xn) address, this is the number of the
	 label being accessed, otherwise it is -1.  */
      labelno = (address.offset
		 && !address.base
		 && GET_CODE (address.offset) == LABEL_REF
		 ? CODE_LABEL_NUMBER (XEXP (address.offset, 0))
		 : -1);
      if (MOTOROLA)
	{
	  /* Print the "offset(base" component.  */
	  if (labelno >= 0)
	    asm_fprintf (file, "%LL%d(%Rpc,", labelno);
	  else
	    {
	      if (address.offset)
		output_addr_const (file, address.offset);

	      putc ('(', file);
	      if (address.base)
		fputs (M68K_REGNAME (REGNO (address.base)), file);
	    }
	  /* Print the ",index" component, if any.  */
	  if (address.index)
	    {
	      if (address.base)
		putc (',', file);
	      fprintf (file, "%s.%c",
		       M68K_REGNAME (REGNO (address.index)),
		       GET_MODE (address.index) == HImode ? 'w' : 'l');
	      if (address.scale != 1)
		fprintf (file, "*%d", address.scale);
	    }
	  putc (')', file);
	}
      else /* !MOTOROLA */
	{
	  if (!address.offset && !address.index)
	    fprintf (file, "%s@", M68K_REGNAME (REGNO (address.base)));
	  else
	    {
	      /* Print the "base@(offset" component.  */
	      if (labelno >= 0)
		asm_fprintf (file, "%Rpc@(%LL%d", labelno);
	      else
		{
		  if (address.base)
		    fputs (M68K_REGNAME (REGNO (address.base)), file);
		  fprintf (file, "@(");
		  if (address.offset)
		    output_addr_const (file, address.offset);
		}
	      /* Print the ",index" component, if any.  */
	      if (address.index)
		{
		  fprintf (file, ",%s:%c",
			   M68K_REGNAME (REGNO (address.index)),
			   GET_MODE (address.index) == HImode ? 'w' : 'l');
		  if (address.scale != 1)
		    fprintf (file, ":%d", address.scale);
		}
	      putc (')', file);
	    }
	}
    }
}

/* Check for cases where a clr insns can be omitted from code using
   strict_low_part sets.  For example, the second clrl here is not needed:
   clrl d0; movw a0@+,d0; use d0; clrl d0; movw a0@+; use d0; ...

   MODE is the mode of this STRICT_LOW_PART set.  FIRST_INSN is the clear
   insn we are checking for redundancy.  TARGET is the register set by the
   clear insn.  */

bool
strict_low_part_peephole_ok (machine_mode mode, rtx_insn *first_insn,
                             rtx target)
{
  rtx_insn *p = first_insn;

  while ((p = PREV_INSN (p)))
    {
      if (NOTE_INSN_BASIC_BLOCK_P (p))
	return false;

      if (NOTE_P (p))
	continue;

      /* If it isn't an insn, then give up.  */
      if (!INSN_P (p))
	return false;

      if (reg_set_p (target, p))
	{
	  rtx set = single_set (p);
	  rtx dest;

	  /* If it isn't an easy to recognize insn, then give up.  */
	  if (! set)
	    return false;

	  dest = SET_DEST (set);

	  /* If this sets the entire target register to zero, then our
	     first_insn is redundant.  */
	  if (rtx_equal_p (dest, target)
	      && SET_SRC (set) == const0_rtx)
	    return true;
	  else if (GET_CODE (dest) == STRICT_LOW_PART
		   && GET_CODE (XEXP (dest, 0)) == REG
		   && REGNO (XEXP (dest, 0)) == REGNO (target)
		   && (GET_MODE_SIZE (GET_MODE (XEXP (dest, 0)))
		       <= GET_MODE_SIZE (mode)))
	    /* This is a strict low part set which modifies less than
	       we are using, so it is safe.  */
	    ;
	  else
	    return false;
	}
    }

  return false;
}

/* Operand predicates for implementing asymmetric pc-relative addressing
   on m68k.  The m68k supports pc-relative addressing (mode 7, register 2)
   when used as a source operand, but not as a destination operand.

   We model this by restricting the meaning of the basic predicates
   (general_operand, memory_operand, etc) to forbid the use of this
   addressing mode, and then define the following predicates that permit
   this addressing mode.  These predicates can then be used for the
   source operands of the appropriate instructions.

   n.b.  While it is theoretically possible to change all machine patterns
   to use this addressing more where permitted by the architecture,
   it has only been implemented for "common" cases: SImode, HImode, and
   QImode operands, and only for the principle operations that would
   require this addressing mode: data movement and simple integer operations.

   In parallel with these new predicates, two new constraint letters
   were defined: 'S' and 'T'.  'S' is the -mpcrel analog of 'm'.
   'T' replaces 's' in the non-pcrel case.  It is a no-op in the pcrel case.
   In the pcrel case 's' is only valid in combination with 'a' registers.
   See addsi3, subsi3, cmpsi, and movsi patterns for a better understanding
   of how these constraints are used.

   The use of these predicates is strictly optional, though patterns that
   don't will cause an extra reload register to be allocated where one
   was not necessary:

	lea (abc:w,%pc),%a0	; need to reload address
	moveq &1,%d1		; since write to pc-relative space
	movel %d1,%a0@		; is not allowed
	...
	lea (abc:w,%pc),%a1	; no need to reload address here
	movel %a1@,%d0		; since "movel (abc:w,%pc),%d0" is ok

   For more info, consult tiemann@cygnus.com.


   All of the ugliness with predicates and constraints is due to the
   simple fact that the m68k does not allow a pc-relative addressing
   mode as a destination.  gcc does not distinguish between source and
   destination addresses.  Hence, if we claim that pc-relative address
   modes are valid, e.g. TARGET_LEGITIMATE_ADDRESS_P accepts them, then we
   end up with invalid code.  To get around this problem, we left
   pc-relative modes as invalid addresses, and then added special
   predicates and constraints to accept them.

   A cleaner way to handle this is to modify gcc to distinguish
   between source and destination addresses.  We can then say that
   pc-relative is a valid source address but not a valid destination
   address, and hopefully avoid a lot of the predicate and constraint
   hackery.  Unfortunately, this would be a pretty big change.  It would
   be a useful change for a number of ports, but there aren't any current
   plans to undertake this.

   ***************************************************************************/


const char *
output_andsi3 (rtx *operands)
{
  int logval;
  if (GET_CODE (operands[2]) == CONST_INT
      && (INTVAL (operands[2]) | 0xffff) == -1
      && (DATA_REG_P (operands[0])
	  || offsettable_memref_p (operands[0]))
      && !TARGET_COLDFIRE)
    {
      if (GET_CODE (operands[0]) != REG)
        operands[0] = adjust_address (operands[0], HImode, 2);
      operands[2] = GEN_INT (INTVAL (operands[2]) & 0xffff);
      /* Do not delete a following tstl %0 insn; that would be incorrect.  */
      CC_STATUS_INIT;
      if (operands[2] == const0_rtx)
        return "clr%.w %0";
      return "and%.w %2,%0";
    }
  if (GET_CODE (operands[2]) == CONST_INT
      && (logval = exact_log2 (~ INTVAL (operands[2]) & 0xffffffff)) >= 0
      && (DATA_REG_P (operands[0])
          || offsettable_memref_p (operands[0])))
    {
      if (DATA_REG_P (operands[0]))
	operands[1] = GEN_INT (logval);
      else
        {
	  operands[0] = adjust_address (operands[0], SImode, 3 - (logval / 8));
	  operands[1] = GEN_INT (logval % 8);
        }
      /* This does not set condition codes in a standard way.  */
      CC_STATUS_INIT;
      return "bclr %1,%0";
    }
  return "and%.l %2,%0";
}

const char *
output_iorsi3 (rtx *operands)
{
  register int logval;
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >> 16 == 0
      && (DATA_REG_P (operands[0])
	  || offsettable_memref_p (operands[0]))
      && !TARGET_COLDFIRE)
    {
      if (GET_CODE (operands[0]) != REG)
        operands[0] = adjust_address (operands[0], HImode, 2);
      /* Do not delete a following tstl %0 insn; that would be incorrect.  */
      CC_STATUS_INIT;
      if (INTVAL (operands[2]) == 0xffff)
	return "mov%.w %2,%0";
      return "or%.w %2,%0";
    }
  if (GET_CODE (operands[2]) == CONST_INT
      && (logval = exact_log2 (INTVAL (operands[2]) & 0xffffffff)) >= 0
      && (DATA_REG_P (operands[0])
	  || offsettable_memref_p (operands[0])))
    {
      if (DATA_REG_P (operands[0]))
	operands[1] = GEN_INT (logval);
      else
        {
	  operands[0] = adjust_address (operands[0], SImode, 3 - (logval / 8));
	  operands[1] = GEN_INT (logval % 8);
	}
      CC_STATUS_INIT;
      return "bset %1,%0";
    }
  return "or%.l %2,%0";
}

const char *
output_xorsi3 (rtx *operands)
{
  register int logval;
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >> 16 == 0
      && (offsettable_memref_p (operands[0]) || DATA_REG_P (operands[0]))
      && !TARGET_COLDFIRE)
    {
      if (! DATA_REG_P (operands[0]))
	operands[0] = adjust_address (operands[0], HImode, 2);
      /* Do not delete a following tstl %0 insn; that would be incorrect.  */
      CC_STATUS_INIT;
      if (INTVAL (operands[2]) == 0xffff)
	return "not%.w %0";
      return "eor%.w %2,%0";
    }
  if (GET_CODE (operands[2]) == CONST_INT
      && (logval = exact_log2 (INTVAL (operands[2]) & 0xffffffff)) >= 0
      && (DATA_REG_P (operands[0])
	  || offsettable_memref_p (operands[0])))
    {
      if (DATA_REG_P (operands[0]))
	operands[1] = GEN_INT (logval);
      else
        {
	  operands[0] = adjust_address (operands[0], SImode, 3 - (logval / 8));
	  operands[1] = GEN_INT (logval % 8);
	}
      CC_STATUS_INIT;
      return "bchg %1,%0";
    }
  return "eor%.l %2,%0";
}

/* Return the instruction that should be used for a call to address X,
   which is known to be in operand 0.  */

const char *
output_call (rtx x)
{
  if (symbolic_operand (x, VOIDmode))
    return m68k_symbolic_call;
  else
    return "jsr %a0";
}

/* Likewise sibling calls.  */

const char *
output_sibcall (rtx x)
{
  if (symbolic_operand (x, VOIDmode))
    return m68k_symbolic_jump;
  else
    return "jmp %a0";
}

static void
m68k_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
		      HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
		      tree function)
{
  rtx this_slot, offset, addr, mem, tmp;
  rtx_insn *insn;

  /* Avoid clobbering the struct value reg by using the
     static chain reg as a temporary.  */
  tmp = gen_rtx_REG (Pmode, STATIC_CHAIN_REGNUM);

  /* Pretend to be a post-reload pass while generating rtl.  */
  reload_completed = 1;

  /* The "this" pointer is stored at 4(%sp).  */
  this_slot = gen_rtx_MEM (Pmode, plus_constant (Pmode,
						 stack_pointer_rtx, 4));

  /* Add DELTA to THIS.  */
  if (delta != 0)
    {
      /* Make the offset a legitimate operand for memory addition.  */
      offset = GEN_INT (delta);
      if ((delta < -8 || delta > 8)
	  && (TARGET_COLDFIRE || USE_MOVQ (delta)))
	{
	  emit_move_insn (gen_rtx_REG (Pmode, D0_REG), offset);
	  offset = gen_rtx_REG (Pmode, D0_REG);
	}
      emit_insn (gen_add3_insn (copy_rtx (this_slot),
				copy_rtx (this_slot), offset));
    }

  /* If needed, add *(*THIS + VCALL_OFFSET) to THIS.  */
  if (vcall_offset != 0)
    {
      /* Set the static chain register to *THIS.  */
      emit_move_insn (tmp, this_slot);
      emit_move_insn (tmp, gen_rtx_MEM (Pmode, tmp));

      /* Set ADDR to a legitimate address for *THIS + VCALL_OFFSET.  */
      addr = plus_constant (Pmode, tmp, vcall_offset);
      if (!m68k_legitimate_address_p (Pmode, addr, true))
	{
	  emit_insn (gen_rtx_SET (tmp, addr));
	  addr = tmp;
	}

      /* Load the offset into %d0 and add it to THIS.  */
      emit_move_insn (gen_rtx_REG (Pmode, D0_REG),
		      gen_rtx_MEM (Pmode, addr));
      emit_insn (gen_add3_insn (copy_rtx (this_slot),
				copy_rtx (this_slot),
				gen_rtx_REG (Pmode, D0_REG)));
    }

  /* Jump to the target function.  Use a sibcall if direct jumps are
     allowed, otherwise load the address into a register first.  */
  mem = DECL_RTL (function);
  if (!sibcall_operand (XEXP (mem, 0), VOIDmode))
    {
      gcc_assert (flag_pic);

      if (!TARGET_SEP_DATA)
	{
	  /* Use the static chain register as a temporary (call-clobbered)
	     GOT pointer for this function.  We can use the static chain
	     register because it isn't live on entry to the thunk.  */
	  SET_REGNO (pic_offset_table_rtx, STATIC_CHAIN_REGNUM);
	  emit_insn (gen_load_got (pic_offset_table_rtx));
	}
      legitimize_pic_address (XEXP (mem, 0), Pmode, tmp);
      mem = replace_equiv_address (mem, tmp);
    }
  insn = emit_call_insn (gen_sibcall (mem, const0_rtx));
  SIBLING_CALL_P (insn) = 1;

  /* Run just enough of rest_of_compilation.  */
  insn = get_insns ();
  split_all_insns_noflow ();
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();

  /* Clean up the vars set above.  */
  reload_completed = 0;

  /* Restore the original PIC register.  */
  if (flag_pic)
    SET_REGNO (pic_offset_table_rtx, PIC_REG);
}

/* Worker function for TARGET_STRUCT_VALUE_RTX.  */

static rtx
m68k_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
		       int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, M68K_STRUCT_VALUE_REGNUM);
}

/* Return nonzero if register old_reg can be renamed to register new_reg.  */
int
m68k_hard_regno_rename_ok (unsigned int old_reg ATTRIBUTE_UNUSED,
			   unsigned int new_reg)
{

  /* Interrupt functions can only use registers that have already been
     saved by the prologue, even if they would normally be
     call-clobbered.  */

  if ((m68k_get_function_kind (current_function_decl)
       == m68k_fk_interrupt_handler)
      && !df_regs_ever_live_p (new_reg))
    return 0;

  return 1;
}

/* Implement TARGET_HARD_REGNO_NREGS.

   On the m68k, ordinary registers hold 32 bits worth;
   for the 68881 registers, a single register is always enough for
   anything that can be stored in them at all.  */

static unsigned int
m68k_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  if (regno >= 16)
    return GET_MODE_NUNITS (mode);
  return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  On the 68000, we let the cpu
   registers can hold any mode, but restrict the 68881 registers to
   floating-point modes.  */

static bool
m68k_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (DATA_REGNO_P (regno))
    {
      /* Data Registers, can hold aggregate if fits in.  */
      if (regno + GET_MODE_SIZE (mode) / 4 <= 8)
	return true;
    }
  else if (ADDRESS_REGNO_P (regno))
    {
      if (regno + GET_MODE_SIZE (mode) / 4 <= 16)
	return true;
    }
  else if (FP_REGNO_P (regno))
    {
      /* FPU registers, hold float or complex float of long double or
	 smaller.  */
      if ((GET_MODE_CLASS (mode) == MODE_FLOAT
	   || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	  && GET_MODE_UNIT_SIZE (mode) <= TARGET_FP_REG_SIZE)
	return true;
    }
  return false;
}

/* Implement TARGET_MODES_TIEABLE_P.  */

static bool
m68k_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  return (!TARGET_HARD_FLOAT
	  || ((GET_MODE_CLASS (mode1) == MODE_FLOAT
	       || GET_MODE_CLASS (mode1) == MODE_COMPLEX_FLOAT)
	      == (GET_MODE_CLASS (mode2) == MODE_FLOAT
		  || GET_MODE_CLASS (mode2) == MODE_COMPLEX_FLOAT)));
}

/* Implement SECONDARY_RELOAD_CLASS.  */

enum reg_class
m68k_secondary_reload_class (enum reg_class rclass,
			     machine_mode mode, rtx x)
{
  int regno;

  regno = true_regnum (x);

  /* If one operand of a movqi is an address register, the other
     operand must be a general register or constant.  Other types
     of operand must be reloaded through a data register.  */
  if (GET_MODE_SIZE (mode) == 1
      && reg_classes_intersect_p (rclass, ADDR_REGS)
      && !(INT_REGNO_P (regno) || CONSTANT_P (x)))
    return DATA_REGS;

  /* PC-relative addresses must be loaded into an address register first.  */
  if (TARGET_PCREL
      && !reg_class_subset_p (rclass, ADDR_REGS)
      && symbolic_operand (x, VOIDmode))
    return ADDR_REGS;

  return NO_REGS;
}

/* Implement PREFERRED_RELOAD_CLASS.  */

enum reg_class
m68k_preferred_reload_class (rtx x, enum reg_class rclass)
{
  enum reg_class secondary_class;

  /* If RCLASS might need a secondary reload, try restricting it to
     a class that doesn't.  */
  secondary_class = m68k_secondary_reload_class (rclass, GET_MODE (x), x);
  if (secondary_class != NO_REGS
      && reg_class_subset_p (secondary_class, rclass))
    return secondary_class;

  /* Prefer to use moveq for in-range constants.  */
  if (GET_CODE (x) == CONST_INT
      && reg_class_subset_p (DATA_REGS, rclass)
      && IN_RANGE (INTVAL (x), -0x80, 0x7f))
    return DATA_REGS;

  /* ??? Do we really need this now?  */
  if (GET_CODE (x) == CONST_DOUBLE
      && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    {
      if (TARGET_HARD_FLOAT && reg_class_subset_p (FP_REGS, rclass))
	return FP_REGS;

      return NO_REGS;
    }

  return rclass;
}

/* Return floating point values in a 68881 register.  This makes 68881 code
   a little bit faster.  It also makes -msoft-float code incompatible with
   hard-float code, so people have to be careful not to mix the two.
   For ColdFire it was decided the ABI incompatibility is undesirable.
   If there is need for a hard-float ABI it is probably worth doing it
   properly and also passing function arguments in FP registers.  */
rtx
m68k_libcall_value (machine_mode mode)
{
  switch (mode) {
  case E_SFmode:
  case E_DFmode:
  case E_XFmode:
    if (TARGET_68881)
      return gen_rtx_REG (mode, FP0_REG);
    break;
  default:
    break;
  }

  return gen_rtx_REG (mode, m68k_libcall_value_in_a0_p ? A0_REG : D0_REG);
}

/* Location in which function value is returned.
   NOTE: Due to differences in ABIs, don't call this function directly,
   use FUNCTION_VALUE instead.  */
rtx
m68k_function_value (const_tree valtype, const_tree func ATTRIBUTE_UNUSED)
{
  machine_mode mode;

  mode = TYPE_MODE (valtype);
  switch (mode) {
  case E_SFmode:
  case E_DFmode:
  case E_XFmode:
    if (TARGET_68881)
      return gen_rtx_REG (mode, FP0_REG);
    break;
  default:
    break;
  }

  /* If the function returns a pointer, push that into %a0.  */
  if (func && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (func))))
    /* For compatibility with the large body of existing code which
       does not always properly declare external functions returning
       pointer types, the m68k/SVR4 convention is to copy the value
       returned for pointer functions from a0 to d0 in the function
       epilogue, so that callers that have neglected to properly
       declare the callee can still find the correct return value in
       d0.  */
    return gen_rtx_PARALLEL
      (mode,
       gen_rtvec (2,
		  gen_rtx_EXPR_LIST (VOIDmode,
				     gen_rtx_REG (mode, A0_REG),
				     const0_rtx),
		  gen_rtx_EXPR_LIST (VOIDmode,
				     gen_rtx_REG (mode, D0_REG),
				     const0_rtx)));
  else if (POINTER_TYPE_P (valtype))
    return gen_rtx_REG (mode, A0_REG);
  else
    return gen_rtx_REG (mode, D0_REG);
}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */
#if M68K_HONOR_TARGET_STRICT_ALIGNMENT
static bool
m68k_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  machine_mode mode = TYPE_MODE (type);

  if (mode == BLKmode)
    return true;

  /* If TYPE's known alignment is less than the alignment of MODE that
     would contain the structure, then return in memory.  We need to
     do so to maintain the compatibility between code compiled with
     -mstrict-align and that compiled with -mno-strict-align.  */
  if (AGGREGATE_TYPE_P (type)
      && TYPE_ALIGN (type) < GET_MODE_ALIGNMENT (mode))
    return true;

  return false;
}
#endif

/* CPU to schedule the program for.  */
enum attr_cpu m68k_sched_cpu;

/* MAC to schedule the program for.  */
enum attr_mac m68k_sched_mac;

/* Operand type.  */
enum attr_op_type
  {
    /* No operand.  */
    OP_TYPE_NONE,

    /* Integer register.  */
    OP_TYPE_RN,

    /* FP register.  */
    OP_TYPE_FPN,

    /* Implicit mem reference (e.g. stack).  */
    OP_TYPE_MEM1,

    /* Memory without offset or indexing.  EA modes 2, 3 and 4.  */
    OP_TYPE_MEM234,

    /* Memory with offset but without indexing.  EA mode 5.  */
    OP_TYPE_MEM5,

    /* Memory with indexing.  EA mode 6.  */
    OP_TYPE_MEM6,

    /* Memory referenced by absolute address.  EA mode 7.  */
    OP_TYPE_MEM7,

    /* Immediate operand that doesn't require extension word.  */
    OP_TYPE_IMM_Q,

    /* Immediate 16 bit operand.  */
    OP_TYPE_IMM_W,

    /* Immediate 32 bit operand.  */
    OP_TYPE_IMM_L
  };

/* Return type of memory ADDR_RTX refers to.  */
static enum attr_op_type
sched_address_type (machine_mode mode, rtx addr_rtx)
{
  struct m68k_address address;

  if (symbolic_operand (addr_rtx, VOIDmode))
    return OP_TYPE_MEM7;

  if (!m68k_decompose_address (mode, addr_rtx,
			       reload_completed, &address))
    {
      gcc_assert (!reload_completed);
      /* Reload will likely fix the address to be in the register.  */
      return OP_TYPE_MEM234;
    }

  if (address.scale != 0)
    return OP_TYPE_MEM6;

  if (address.base != NULL_RTX)
    {
      if (address.offset == NULL_RTX)
	return OP_TYPE_MEM234;

      return OP_TYPE_MEM5;
    }

  gcc_assert (address.offset != NULL_RTX);

  return OP_TYPE_MEM7;
}

/* Return X or Y (depending on OPX_P) operand of INSN.  */
static rtx
sched_get_operand (rtx_insn *insn, bool opx_p)
{
  int i;

  if (recog_memoized (insn) < 0)
    gcc_unreachable ();

  extract_constrain_insn_cached (insn);

  if (opx_p)
    i = get_attr_opx (insn);
  else
    i = get_attr_opy (insn);

  if (i >= recog_data.n_operands)
    return NULL;

  return recog_data.operand[i];
}

/* Return type of INSN's operand X (if OPX_P) or operand Y (if !OPX_P).
   If ADDRESS_P is true, return type of memory location operand refers to.  */
static enum attr_op_type
sched_attr_op_type (rtx_insn *insn, bool opx_p, bool address_p)
{
  rtx op;

  op = sched_get_operand (insn, opx_p);

  if (op == NULL)
    {
      gcc_assert (!reload_completed);
      return OP_TYPE_RN;
    }

  if (address_p)
    return sched_address_type (QImode, op);

  if (memory_operand (op, VOIDmode))
    return sched_address_type (GET_MODE (op), XEXP (op, 0));

  if (register_operand (op, VOIDmode))
    {
      if ((!reload_completed && FLOAT_MODE_P (GET_MODE (op)))
	  || (reload_completed && FP_REG_P (op)))
	return OP_TYPE_FPN;

      return OP_TYPE_RN;
    }

  if (GET_CODE (op) == CONST_INT)
    {
      int ival;

      ival = INTVAL (op);

      /* Check for quick constants.  */
      switch (get_attr_type (insn))
	{
	case TYPE_ALUQ_L:
	  if (IN_RANGE (ival, 1, 8) || IN_RANGE (ival, -8, -1))
	    return OP_TYPE_IMM_Q;

	  gcc_assert (!reload_completed);
	  break;

	case TYPE_MOVEQ_L:
	  if (USE_MOVQ (ival))
	    return OP_TYPE_IMM_Q;

	  gcc_assert (!reload_completed);
	  break;

	case TYPE_MOV3Q_L:
	  if (valid_mov3q_const (ival))
	    return OP_TYPE_IMM_Q;

	  gcc_assert (!reload_completed);
	  break;

	default:
	  break;
	}

      if (IN_RANGE (ival, -0x8000, 0x7fff))
	return OP_TYPE_IMM_W;

      return OP_TYPE_IMM_L;
    }

  if (GET_CODE (op) == CONST_DOUBLE)
    {
      switch (GET_MODE (op))
	{
	case E_SFmode:
	  return OP_TYPE_IMM_W;

	case E_VOIDmode:
	case E_DFmode:
	  return OP_TYPE_IMM_L;

	default:
	  gcc_unreachable ();
	}
    }

  if (GET_CODE (op) == CONST
      || symbolic_operand (op, VOIDmode)
      || LABEL_P (op))
    {
      switch (GET_MODE (op))
	{
	case E_QImode:
	  return OP_TYPE_IMM_Q;

	case E_HImode:
	  return OP_TYPE_IMM_W;

	case E_SImode:
	  return OP_TYPE_IMM_L;

	default:
	  if (symbolic_operand (m68k_unwrap_symbol (op, false), VOIDmode))
	    /* Just a guess.  */
	    return OP_TYPE_IMM_W;

	  return OP_TYPE_IMM_L;
	}
    }

  gcc_assert (!reload_completed);

  if (FLOAT_MODE_P (GET_MODE (op)))
    return OP_TYPE_FPN;

  return OP_TYPE_RN;
}

/* Implement opx_type attribute.
   Return type of INSN's operand X.
   If ADDRESS_P is true, return type of memory location operand refers to.  */
enum attr_opx_type
m68k_sched_attr_opx_type (rtx_insn *insn, int address_p)
{
  switch (sched_attr_op_type (insn, true, address_p != 0))
    {
    case OP_TYPE_RN:
      return OPX_TYPE_RN;

    case OP_TYPE_FPN:
      return OPX_TYPE_FPN;

    case OP_TYPE_MEM1:
      return OPX_TYPE_MEM1;

    case OP_TYPE_MEM234:
      return OPX_TYPE_MEM234;

    case OP_TYPE_MEM5:
      return OPX_TYPE_MEM5;

    case OP_TYPE_MEM6:
      return OPX_TYPE_MEM6;

    case OP_TYPE_MEM7:
      return OPX_TYPE_MEM7;

    case OP_TYPE_IMM_Q:
      return OPX_TYPE_IMM_Q;

    case OP_TYPE_IMM_W:
      return OPX_TYPE_IMM_W;

    case OP_TYPE_IMM_L:
      return OPX_TYPE_IMM_L;

    default:
      gcc_unreachable ();
    }
}

/* Implement opy_type attribute.
   Return type of INSN's operand Y.
   If ADDRESS_P is true, return type of memory location operand refers to.  */
enum attr_opy_type
m68k_sched_attr_opy_type (rtx_insn *insn, int address_p)
{
  switch (sched_attr_op_type (insn, false, address_p != 0))
    {
    case OP_TYPE_RN:
      return OPY_TYPE_RN;

    case OP_TYPE_FPN:
      return OPY_TYPE_FPN;

    case OP_TYPE_MEM1:
      return OPY_TYPE_MEM1;

    case OP_TYPE_MEM234:
      return OPY_TYPE_MEM234;

    case OP_TYPE_MEM5:
      return OPY_TYPE_MEM5;

    case OP_TYPE_MEM6:
      return OPY_TYPE_MEM6;

    case OP_TYPE_MEM7:
      return OPY_TYPE_MEM7;

    case OP_TYPE_IMM_Q:
      return OPY_TYPE_IMM_Q;

    case OP_TYPE_IMM_W:
      return OPY_TYPE_IMM_W;

    case OP_TYPE_IMM_L:
      return OPY_TYPE_IMM_L;

    default:
      gcc_unreachable ();
    }
}

/* Return size of INSN as int.  */
static int
sched_get_attr_size_int (rtx_insn *insn)
{
  int size;

  switch (get_attr_type (insn))
    {
    case TYPE_IGNORE:
      /* There should be no references to m68k_sched_attr_size for 'ignore'
	 instructions.  */
      gcc_unreachable ();
      return 0;

    case TYPE_MUL_L:
      size = 2;
      break;

    default:
      size = 1;
      break;
    }

  switch (get_attr_opx_type (insn))
    {
    case OPX_TYPE_NONE:
    case OPX_TYPE_RN:
    case OPX_TYPE_FPN:
    case OPX_TYPE_MEM1:
    case OPX_TYPE_MEM234:
    case OPY_TYPE_IMM_Q:
      break;

    case OPX_TYPE_MEM5:
    case OPX_TYPE_MEM6:
      /* Here we assume that most absolute references are short.  */
    case OPX_TYPE_MEM7:
    case OPY_TYPE_IMM_W:
      ++size;
      break;

    case OPY_TYPE_IMM_L:
      size += 2;
      break;

    default:
      gcc_unreachable ();
    }

  switch (get_attr_opy_type (insn))
    {
    case OPY_TYPE_NONE:
    case OPY_TYPE_RN:
    case OPY_TYPE_FPN:
    case OPY_TYPE_MEM1:
    case OPY_TYPE_MEM234:
    case OPY_TYPE_IMM_Q:
      break;

    case OPY_TYPE_MEM5:
    case OPY_TYPE_MEM6:
      /* Here we assume that most absolute references are short.  */
    case OPY_TYPE_MEM7:
    case OPY_TYPE_IMM_W:
      ++size;
      break;

    case OPY_TYPE_IMM_L:
      size += 2;
      break;

    default:
      gcc_unreachable ();
    }

  if (size > 3)
    {
      gcc_assert (!reload_completed);

      size = 3;
    }

  return size;
}

/* Return size of INSN as attribute enum value.  */
enum attr_size
m68k_sched_attr_size (rtx_insn *insn)
{
  switch (sched_get_attr_size_int (insn))
    {
    case 1:
      return SIZE_1;

    case 2:
      return SIZE_2;

    case 3:
      return SIZE_3;

    default:
      gcc_unreachable ();
    }
}

/* Return operand X or Y (depending on OPX_P) of INSN,
   if it is a MEM, or NULL overwise.  */
static enum attr_op_type
sched_get_opxy_mem_type (rtx_insn *insn, bool opx_p)
{
  if (opx_p)
    {
      switch (get_attr_opx_type (insn))
	{
	case OPX_TYPE_NONE:
	case OPX_TYPE_RN:
	case OPX_TYPE_FPN:
	case OPX_TYPE_IMM_Q:
	case OPX_TYPE_IMM_W:
	case OPX_TYPE_IMM_L:
	  return OP_TYPE_RN;

	case OPX_TYPE_MEM1:
	case OPX_TYPE_MEM234:
	case OPX_TYPE_MEM5:
	case OPX_TYPE_MEM7:
	  return OP_TYPE_MEM1;

	case OPX_TYPE_MEM6:
	  return OP_TYPE_MEM6;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      switch (get_attr_opy_type (insn))
	{
	case OPY_TYPE_NONE:
	case OPY_TYPE_RN:
	case OPY_TYPE_FPN:
	case OPY_TYPE_IMM_Q:
	case OPY_TYPE_IMM_W:
	case OPY_TYPE_IMM_L:
	  return OP_TYPE_RN;

	case OPY_TYPE_MEM1:
	case OPY_TYPE_MEM234:
	case OPY_TYPE_MEM5:
	case OPY_TYPE_MEM7:
	  return OP_TYPE_MEM1;

	case OPY_TYPE_MEM6:
	  return OP_TYPE_MEM6;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Implement op_mem attribute.  */
enum attr_op_mem
m68k_sched_attr_op_mem (rtx_insn *insn)
{
  enum attr_op_type opx;
  enum attr_op_type opy;

  opx = sched_get_opxy_mem_type (insn, true);
  opy = sched_get_opxy_mem_type (insn, false);

  if (opy == OP_TYPE_RN && opx == OP_TYPE_RN)
    return OP_MEM_00;

  if (opy == OP_TYPE_RN && opx == OP_TYPE_MEM1)
    {
      switch (get_attr_opx_access (insn))
	{
	case OPX_ACCESS_R:
	  return OP_MEM_10;

	case OPX_ACCESS_W:
	  return OP_MEM_01;

	case OPX_ACCESS_RW:
	  return OP_MEM_11;

	default:
	  gcc_unreachable ();
	}
    }

  if (opy == OP_TYPE_RN && opx == OP_TYPE_MEM6)
    {
      switch (get_attr_opx_access (insn))
	{
	case OPX_ACCESS_R:
	  return OP_MEM_I0;

	case OPX_ACCESS_W:
	  return OP_MEM_0I;

	case OPX_ACCESS_RW:
	  return OP_MEM_I1;

	default:
	  gcc_unreachable ();
	}
    }

  if (opy == OP_TYPE_MEM1 && opx == OP_TYPE_RN)
    return OP_MEM_10;

  if (opy == OP_TYPE_MEM1 && opx == OP_TYPE_MEM1)
    {
      switch (get_attr_opx_access (insn))
	{
	case OPX_ACCESS_W:
	  return OP_MEM_11;

	default:
	  gcc_assert (!reload_completed);
	  return OP_MEM_11;
	}
    }

  if (opy == OP_TYPE_MEM1 && opx == OP_TYPE_MEM6)
    {
      switch (get_attr_opx_access (insn))
	{
	case OPX_ACCESS_W:
	  return OP_MEM_1I;

	default:
	  gcc_assert (!reload_completed);
	  return OP_MEM_1I;
	}
    }

  if (opy == OP_TYPE_MEM6 && opx == OP_TYPE_RN)
    return OP_MEM_I0;

  if (opy == OP_TYPE_MEM6 && opx == OP_TYPE_MEM1)
    {
      switch (get_attr_opx_access (insn))
	{
	case OPX_ACCESS_W:
	  return OP_MEM_I1;

	default:
	  gcc_assert (!reload_completed);
	  return OP_MEM_I1;
	}
    }

  gcc_assert (opy == OP_TYPE_MEM6 && opx == OP_TYPE_MEM6);
  gcc_assert (!reload_completed);
  return OP_MEM_I1;
}

/* Data for ColdFire V4 index bypass.
   Producer modifies register that is used as index in consumer with
   specified scale.  */
static struct
{
  /* Producer instruction.  */
  rtx pro;

  /* Consumer instruction.  */
  rtx con;

  /* Scale of indexed memory access within consumer.
     Or zero if bypass should not be effective at the moment.  */
  int scale;
} sched_cfv4_bypass_data;

/* An empty state that is used in m68k_sched_adjust_cost.  */
static state_t sched_adjust_cost_state;

/* Implement adjust_cost scheduler hook.
   Return adjusted COST of dependency LINK between DEF_INSN and INSN.  */
static int
m68k_sched_adjust_cost (rtx_insn *insn, int, rtx_insn *def_insn, int cost,
			unsigned int)
{
  int delay;

  if (recog_memoized (def_insn) < 0
      || recog_memoized (insn) < 0)
    return cost;

  if (sched_cfv4_bypass_data.scale == 1)
    /* Handle ColdFire V4 bypass for indexed address with 1x scale.  */
    {
      /* haifa-sched.c: insn_cost () calls bypass_p () just before
	 targetm.sched.adjust_cost ().  Hence, we can be relatively sure
	 that the data in sched_cfv4_bypass_data is up to date.  */
      gcc_assert (sched_cfv4_bypass_data.pro == def_insn
		  && sched_cfv4_bypass_data.con == insn);

      if (cost < 3)
	cost = 3;

      sched_cfv4_bypass_data.pro = NULL;
      sched_cfv4_bypass_data.con = NULL;
      sched_cfv4_bypass_data.scale = 0;
    }
  else
    gcc_assert (sched_cfv4_bypass_data.pro == NULL
		&& sched_cfv4_bypass_data.con == NULL
		&& sched_cfv4_bypass_data.scale == 0);

  /* Don't try to issue INSN earlier than DFA permits.
     This is especially useful for instructions that write to memory,
     as their true dependence (default) latency is better to be set to 0
     to workaround alias analysis limitations.
     This is, in fact, a machine independent tweak, so, probably,
     it should be moved to haifa-sched.c: insn_cost ().  */
  delay = min_insn_conflict_delay (sched_adjust_cost_state, def_insn, insn);
  if (delay > cost)
    cost = delay;

  return cost;
}

/* Return maximal number of insns that can be scheduled on a single cycle.  */
static int
m68k_sched_issue_rate (void)
{
  switch (m68k_sched_cpu)
    {
    case CPU_CFV1:
    case CPU_CFV2:
    case CPU_CFV3:
      return 1;

    case CPU_CFV4:
      return 2;

    default:
      gcc_unreachable ();
      return 0;
    }
}

/* Maximal length of instruction for current CPU.
   E.g. it is 3 for any ColdFire core.  */
static int max_insn_size;

/* Data to model instruction buffer of CPU.  */
struct _sched_ib
{
  /* True if instruction buffer model is modeled for current CPU.  */
  bool enabled_p;

  /* Size of the instruction buffer in words.  */
  int size;

  /* Number of filled words in the instruction buffer.  */
  int filled;

  /* Additional information about instruction buffer for CPUs that have
     a buffer of instruction records, rather then a plain buffer
     of instruction words.  */
  struct _sched_ib_records
  {
    /* Size of buffer in records.  */
    int n_insns;

    /* Array to hold data on adjustments made to the size of the buffer.  */
    int *adjust;

    /* Index of the above array.  */
    int adjust_index;
  } records;

  /* An insn that reserves (marks empty) one word in the instruction buffer.  */
  rtx insn;
};

static struct _sched_ib sched_ib;

/* ID of memory unit.  */
static int sched_mem_unit_code;

/* Implementation of the targetm.sched.variable_issue () hook.
   It is called after INSN was issued.  It returns the number of insns
   that can possibly get scheduled on the current cycle.
   It is used here to determine the effect of INSN on the instruction
   buffer.  */
static int
m68k_sched_variable_issue (FILE *sched_dump ATTRIBUTE_UNUSED,
			   int sched_verbose ATTRIBUTE_UNUSED,
			   rtx_insn *insn, int can_issue_more)
{
  int insn_size;

  if (recog_memoized (insn) >= 0 && get_attr_type (insn) != TYPE_IGNORE)
    {
      switch (m68k_sched_cpu)
	{
	case CPU_CFV1:
	case CPU_CFV2:
	  insn_size = sched_get_attr_size_int (insn);
	  break;

	case CPU_CFV3:
	  insn_size = sched_get_attr_size_int (insn);
	  
	  /* ColdFire V3 and V4 cores have instruction buffers that can
	     accumulate up to 8 instructions regardless of instructions'
	     sizes.  So we should take care not to "prefetch" 24 one-word
	     or 12 two-words instructions.
	     To model this behavior we temporarily decrease size of the
	     buffer by (max_insn_size - insn_size) for next 7 instructions.  */
	  {
	    int adjust;

	    adjust = max_insn_size - insn_size;
	    sched_ib.size -= adjust;

	    if (sched_ib.filled > sched_ib.size)
	      sched_ib.filled = sched_ib.size;

	    sched_ib.records.adjust[sched_ib.records.adjust_index] = adjust;
	  }

	  ++sched_ib.records.adjust_index;
	  if (sched_ib.records.adjust_index == sched_ib.records.n_insns)
	    sched_ib.records.adjust_index = 0;

	  /* Undo adjustment we did 7 instructions ago.  */
	  sched_ib.size
	    += sched_ib.records.adjust[sched_ib.records.adjust_index];

	  break;

	case CPU_CFV4:
	  gcc_assert (!sched_ib.enabled_p);
	  insn_size = 0;
	  break;

	default:
	  gcc_unreachable ();
	}

      if (insn_size > sched_ib.filled)
	/* Scheduling for register pressure does not always take DFA into
	   account.  Workaround instruction buffer not being filled enough.  */
	{
	  gcc_assert (sched_pressure == SCHED_PRESSURE_WEIGHTED);
	  insn_size = sched_ib.filled;
	}

      --can_issue_more;
    }
  else if (GET_CODE (PATTERN (insn)) == ASM_INPUT
	   || asm_noperands (PATTERN (insn)) >= 0)
    insn_size = sched_ib.filled;
  else
    insn_size = 0;

  sched_ib.filled -= insn_size;

  return can_issue_more;
}

/* Return how many instructions should scheduler lookahead to choose the
   best one.  */
static int
m68k_sched_first_cycle_multipass_dfa_lookahead (void)
{
  return m68k_sched_issue_rate () - 1;
}

/* Implementation of targetm.sched.init_global () hook.
   It is invoked once per scheduling pass and is used here
   to initialize scheduler constants.  */
static void
m68k_sched_md_init_global (FILE *sched_dump ATTRIBUTE_UNUSED,
			   int sched_verbose ATTRIBUTE_UNUSED,
			   int n_insns ATTRIBUTE_UNUSED)
{
  /* Check that all instructions have DFA reservations and
     that all instructions can be issued from a clean state.  */
  if (flag_checking)
    {
      rtx_insn *insn;
      state_t state;

      state = alloca (state_size ());

      for (insn = get_insns (); insn != NULL; insn = NEXT_INSN (insn))
	{
	  if (INSN_P (insn) && recog_memoized (insn) >= 0)
	    {
	      gcc_assert (insn_has_dfa_reservation_p (insn));

	      state_reset (state);
	      if (state_transition (state, insn) >= 0)
		gcc_unreachable ();
	    }
	}
    }

  /* Setup target cpu.  */

  /* ColdFire V4 has a set of features to keep its instruction buffer full
     (e.g., a separate memory bus for instructions) and, hence, we do not model
     buffer for this CPU.  */
  sched_ib.enabled_p = (m68k_sched_cpu != CPU_CFV4);

  switch (m68k_sched_cpu)
    {
    case CPU_CFV4:
      sched_ib.filled = 0;

      /* FALLTHRU */

    case CPU_CFV1:
    case CPU_CFV2:
      max_insn_size = 3;
      sched_ib.records.n_insns = 0;
      sched_ib.records.adjust = NULL;
      break;

    case CPU_CFV3:
      max_insn_size = 3;
      sched_ib.records.n_insns = 8;
      sched_ib.records.adjust = XNEWVEC (int, sched_ib.records.n_insns);
      break;

    default:
      gcc_unreachable ();
    }

  sched_mem_unit_code = get_cpu_unit_code ("cf_mem1");

  sched_adjust_cost_state = xmalloc (state_size ());
  state_reset (sched_adjust_cost_state);

  start_sequence ();
  emit_insn (gen_ib ());
  sched_ib.insn = get_insns ();
  end_sequence ();
}

/* Scheduling pass is now finished.  Free/reset static variables.  */
static void
m68k_sched_md_finish_global (FILE *dump ATTRIBUTE_UNUSED,
			     int verbose ATTRIBUTE_UNUSED)
{
  sched_ib.insn = NULL;

  free (sched_adjust_cost_state);
  sched_adjust_cost_state = NULL;

  sched_mem_unit_code = 0;

  free (sched_ib.records.adjust);
  sched_ib.records.adjust = NULL;
  sched_ib.records.n_insns = 0;
  max_insn_size = 0;
}

/* Implementation of targetm.sched.init () hook.
   It is invoked each time scheduler starts on the new block (basic block or
   extended basic block).  */
static void
m68k_sched_md_init (FILE *sched_dump ATTRIBUTE_UNUSED,
		    int sched_verbose ATTRIBUTE_UNUSED,
		    int n_insns ATTRIBUTE_UNUSED)
{
  switch (m68k_sched_cpu)
    {
    case CPU_CFV1:
    case CPU_CFV2:
      sched_ib.size = 6;
      break;

    case CPU_CFV3:
      sched_ib.size = sched_ib.records.n_insns * max_insn_size;

      memset (sched_ib.records.adjust, 0,
	      sched_ib.records.n_insns * sizeof (*sched_ib.records.adjust));
      sched_ib.records.adjust_index = 0;
      break;

    case CPU_CFV4:
      gcc_assert (!sched_ib.enabled_p);
      sched_ib.size = 0;
      break;

    default:
      gcc_unreachable ();
    }

  if (sched_ib.enabled_p)
    /* haifa-sched.c: schedule_block () calls advance_cycle () just before
       the first cycle.  Workaround that.  */
    sched_ib.filled = -2;
}

/* Implementation of targetm.sched.dfa_pre_advance_cycle () hook.
   It is invoked just before current cycle finishes and is used here
   to track if instruction buffer got its two words this cycle.  */
static void
m68k_sched_dfa_pre_advance_cycle (void)
{
  if (!sched_ib.enabled_p)
    return;

  if (!cpu_unit_reservation_p (curr_state, sched_mem_unit_code))
    {
      sched_ib.filled += 2;

      if (sched_ib.filled > sched_ib.size)
	sched_ib.filled = sched_ib.size;
    }
}

/* Implementation of targetm.sched.dfa_post_advance_cycle () hook.
   It is invoked just after new cycle begins and is used here
   to setup number of filled words in the instruction buffer so that
   instructions which won't have all their words prefetched would be
   stalled for a cycle.  */
static void
m68k_sched_dfa_post_advance_cycle (void)
{
  int i;

  if (!sched_ib.enabled_p)
    return;

  /* Setup number of prefetched instruction words in the instruction
     buffer.  */
  i = max_insn_size - sched_ib.filled;

  while (--i >= 0)
    {
      if (state_transition (curr_state, sched_ib.insn) >= 0)
	/* Pick up scheduler state.  */
	++sched_ib.filled;
    }
}

/* Return X or Y (depending on OPX_P) operand of INSN,
   if it is an integer register, or NULL overwise.  */
static rtx
sched_get_reg_operand (rtx_insn *insn, bool opx_p)
{
  rtx op = NULL;

  if (opx_p)
    {
      if (get_attr_opx_type (insn) == OPX_TYPE_RN)
	{
	  op = sched_get_operand (insn, true);
	  gcc_assert (op != NULL);

	  if (!reload_completed && !REG_P (op))
	    return NULL;
	}
    }
  else
    {
      if (get_attr_opy_type (insn) == OPY_TYPE_RN)
	{
	  op = sched_get_operand (insn, false);
	  gcc_assert (op != NULL);

	  if (!reload_completed && !REG_P (op))
	    return NULL;
	}
    }

  return op;
}

/* Return true, if X or Y (depending on OPX_P) operand of INSN
   is a MEM.  */
static bool
sched_mem_operand_p (rtx_insn *insn, bool opx_p)
{
  switch (sched_get_opxy_mem_type (insn, opx_p))
    {
    case OP_TYPE_MEM1:
    case OP_TYPE_MEM6:
      return true;

    default:
      return false;
    }
}

/* Return X or Y (depending on OPX_P) operand of INSN,
   if it is a MEM, or NULL overwise.  */
static rtx
sched_get_mem_operand (rtx_insn *insn, bool must_read_p, bool must_write_p)
{
  bool opx_p;
  bool opy_p;

  opx_p = false;
  opy_p = false;

  if (must_read_p)
    {
      opx_p = true;
      opy_p = true;
    }

  if (must_write_p)
    {
      opx_p = true;
      opy_p = false;
    }

  if (opy_p && sched_mem_operand_p (insn, false))
    return sched_get_operand (insn, false);

  if (opx_p && sched_mem_operand_p (insn, true))
    return sched_get_operand (insn, true);

  gcc_unreachable ();
  return NULL;
}

/* Return non-zero if PRO modifies register used as part of
   address in CON.  */
int
m68k_sched_address_bypass_p (rtx_insn *pro, rtx_insn *con)
{
  rtx pro_x;
  rtx con_mem_read;

  pro_x = sched_get_reg_operand (pro, true);
  if (pro_x == NULL)
    return 0;

  con_mem_read = sched_get_mem_operand (con, true, false);
  gcc_assert (con_mem_read != NULL);

  if (reg_mentioned_p (pro_x, con_mem_read))
    return 1;

  return 0;
}

/* Helper function for m68k_sched_indexed_address_bypass_p.
   if PRO modifies register used as index in CON,
   return scale of indexed memory access in CON.  Return zero overwise.  */
static int
sched_get_indexed_address_scale (rtx_insn *pro, rtx_insn *con)
{
  rtx reg;
  rtx mem;
  struct m68k_address address;

  reg = sched_get_reg_operand (pro, true);
  if (reg == NULL)
    return 0;

  mem = sched_get_mem_operand (con, true, false);
  gcc_assert (mem != NULL && MEM_P (mem));

  if (!m68k_decompose_address (GET_MODE (mem), XEXP (mem, 0), reload_completed,
			       &address))
    gcc_unreachable ();

  if (REGNO (reg) == REGNO (address.index))
    {
      gcc_assert (address.scale != 0);
      return address.scale;
    }

  return 0;
}

/* Return non-zero if PRO modifies register used
   as index with scale 2 or 4 in CON.  */
int
m68k_sched_indexed_address_bypass_p (rtx_insn *pro, rtx_insn *con)
{
  gcc_assert (sched_cfv4_bypass_data.pro == NULL
	      && sched_cfv4_bypass_data.con == NULL
	      && sched_cfv4_bypass_data.scale == 0);

  switch (sched_get_indexed_address_scale (pro, con))
    {
    case 1:
      /* We can't have a variable latency bypass, so
	 remember to adjust the insn cost in adjust_cost hook.  */
      sched_cfv4_bypass_data.pro = pro;
      sched_cfv4_bypass_data.con = con;
      sched_cfv4_bypass_data.scale = 1;
      return 0;

    case 2:
    case 4:
      return 1;

    default:
      return 0;
    }
}

/* We generate a two-instructions program at M_TRAMP :
	movea.l &CHAIN_VALUE,%a0
	jmp FNADDR
   where %a0 can be modified by changing STATIC_CHAIN_REGNUM.  */

static void
m68k_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx mem;

  gcc_assert (ADDRESS_REGNO_P (STATIC_CHAIN_REGNUM));

  mem = adjust_address (m_tramp, HImode, 0);
  emit_move_insn (mem, GEN_INT(0x207C + ((STATIC_CHAIN_REGNUM-8) << 9)));
  mem = adjust_address (m_tramp, SImode, 2);
  emit_move_insn (mem, chain_value);

  mem = adjust_address (m_tramp, HImode, 6);
  emit_move_insn (mem, GEN_INT(0x4EF9));
  mem = adjust_address (m_tramp, SImode, 8);
  emit_move_insn (mem, fnaddr);

  FINALIZE_TRAMPOLINE (XEXP (m_tramp, 0));
}

/* On the 68000, the RTS insn cannot pop anything.
   On the 68010, the RTD insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RTD can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RTD is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.  */

static poly_int64
m68k_return_pops_args (tree fundecl, tree funtype, poly_int64 size)
{
  return ((TARGET_RTD
	   && (!fundecl
	       || TREE_CODE (fundecl) != IDENTIFIER_NODE)
	   && (!stdarg_p (funtype)))
	  ? (HOST_WIDE_INT) size : 0);
}

/* Make sure everything's fine if we *don't* have a given processor.
   This assumes that putting a register in fixed_regs will keep the
   compiler's mitts completely off it.  We don't bother to zero it out
   of register classes.  */

static void
m68k_conditional_register_usage (void)
{
  int i;
  HARD_REG_SET x;
  if (!TARGET_HARD_FLOAT)
    {
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FP_REGS]);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
        if (TEST_HARD_REG_BIT (x, i))
	  fixed_regs[i] = call_used_regs[i] = 1;
    }
  if (flag_pic)
    fixed_regs[PIC_REG] = call_used_regs[PIC_REG] = 1;
}

static void
m68k_init_sync_libfuncs (void)
{
  init_sync_libfuncs (UNITS_PER_WORD);
}

/* Implements EPILOGUE_USES.  All registers are live on exit from an
   interrupt routine.  */
bool
m68k_epilogue_uses (int regno ATTRIBUTE_UNUSED)
{
  return (reload_completed
	  && (m68k_get_function_kind (current_function_decl)
	      == m68k_fk_interrupt_handler));
}


/* Implement TARGET_C_EXCESS_PRECISION.

   Set the value of FLT_EVAL_METHOD in float.h.  When using 68040 fp
   instructions, we get proper intermediate rounding, otherwise we
   get extended precision results.  */

static enum flt_eval_method
m68k_excess_precision (enum excess_precision_type type)
{
  switch (type)
    {
      case EXCESS_PRECISION_TYPE_FAST:
	/* The fastest type to promote to will always be the native type,
	   whether that occurs with implicit excess precision or
	   otherwise.  */
	return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT;
      case EXCESS_PRECISION_TYPE_STANDARD:
      case EXCESS_PRECISION_TYPE_IMPLICIT:
	/* Otherwise, the excess precision we want when we are
	   in a standards compliant mode, and the implicit precision we
	   provide can be identical.  */
	if (TARGET_68040 || ! TARGET_68881)
	  return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT;

	return FLT_EVAL_METHOD_PROMOTE_TO_LONG_DOUBLE;
      default:
	gcc_unreachable ();
    }
  return FLT_EVAL_METHOD_UNPREDICTABLE;
}

/* Implement PUSH_ROUNDING.  On the 680x0, sp@- in a byte insn really pushes
   a word.  On the ColdFire, sp@- in a byte insn pushes just a byte.  */

poly_int64
m68k_push_rounding (poly_int64 bytes)
{
  if (TARGET_COLDFIRE)
    return bytes;
  return (bytes + 1) & ~1;
}

/* Implement TARGET_PROMOTE_FUNCTION_MODE.  */

static machine_mode
m68k_promote_function_mode (const_tree type, machine_mode mode,
                            int *punsignedp ATTRIBUTE_UNUSED,
                            const_tree fntype ATTRIBUTE_UNUSED,
                            int for_return)
{
  /* Promote libcall arguments narrower than int to match the normal C
     ABI (for which promotions are handled via
     TARGET_PROMOTE_PROTOTYPES).  */
  if (type == NULL_TREE && !for_return && (mode == QImode || mode == HImode))
    return SImode;
  return mode;
}

#include "gt-m68k.h"
