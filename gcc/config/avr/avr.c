/* Subroutines for insn-output.c for ATMEL AVR micro controllers
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2008,
   2009, 2010, 2011 Free Software Foundation, Inc.
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
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "flags.h"
#include "reload.h"
#include "tree.h"
#include "output.h"
#include "expr.h"
#include "diagnostic-core.h"
#include "obstack.h"
#include "function.h"
#include "recog.h"
#include "optabs.h"
#include "ggc.h"
#include "langhooks.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "params.h"
#include "df.h"

/* Maximal allowed offset for an address in the LD command */
#define MAX_LD_OFFSET(MODE) (64 - (signed)GET_MODE_SIZE (MODE))

/* Return true if STR starts with PREFIX and false, otherwise.  */
#define STR_PREFIX_P(STR,PREFIX) (0 == strncmp (STR, PREFIX, strlen (PREFIX)))

#define AVR_SECTION_PROGMEM (SECTION_MACH_DEP << 0)

static void avr_option_override (void);
static int avr_naked_function_p (tree);
static int interrupt_function_p (tree);
static int signal_function_p (tree);
static int avr_OS_task_function_p (tree);
static int avr_OS_main_function_p (tree);
static int avr_regs_to_save (HARD_REG_SET *);
static int get_sequence_length (rtx insns);
static int sequent_regs_live (void);
static const char *ptrreg_to_str (int);
static const char *cond_string (enum rtx_code);
static int avr_num_arg_regs (enum machine_mode, const_tree);

static RTX_CODE compare_condition (rtx insn);
static rtx avr_legitimize_address (rtx, rtx, enum machine_mode);
static int compare_sign_p (rtx insn);
static tree avr_handle_progmem_attribute (tree *, tree, tree, int, bool *);
static tree avr_handle_fndecl_attribute (tree *, tree, tree, int, bool *);
static tree avr_handle_fntype_attribute (tree *, tree, tree, int, bool *);
static bool avr_assemble_integer (rtx, unsigned int, int);
static void avr_file_start (void);
static void avr_file_end (void);
static bool avr_legitimate_address_p (enum machine_mode, rtx, bool);
static void avr_asm_function_end_prologue (FILE *);
static void avr_asm_function_begin_epilogue (FILE *);
static bool avr_cannot_modify_jumps_p (void);
static rtx avr_function_value (const_tree, const_tree, bool);
static rtx avr_libcall_value (enum machine_mode, const_rtx);
static bool avr_function_value_regno_p (const unsigned int);
static void avr_insert_attributes (tree, tree *);
static void avr_asm_init_sections (void);
static unsigned int avr_section_type_flags (tree, const char *, int);

static void avr_reorg (void);
static void avr_asm_out_ctor (rtx, int);
static void avr_asm_out_dtor (rtx, int);
static int avr_register_move_cost (enum machine_mode, reg_class_t, reg_class_t);
static int avr_memory_move_cost (enum machine_mode, reg_class_t, bool);
static int avr_operand_rtx_cost (rtx, enum machine_mode, enum rtx_code,
				 int, bool);
static bool avr_rtx_costs (rtx, int, int, int, int *, bool);
static int avr_address_cost (rtx, bool);
static bool avr_return_in_memory (const_tree, const_tree);
static struct machine_function * avr_init_machine_status (void);
static void avr_init_builtins (void);
static rtx avr_expand_builtin (tree, rtx, rtx, enum machine_mode, int);
static rtx avr_builtin_setjmp_frame_value (void);
static bool avr_hard_regno_scratch_ok (unsigned int);
static unsigned int avr_case_values_threshold (void);
static bool avr_frame_pointer_required_p (void);
static bool avr_can_eliminate (const int, const int);
static bool avr_class_likely_spilled_p (reg_class_t c);
static rtx avr_function_arg (cumulative_args_t , enum machine_mode,
			     const_tree, bool);
static void avr_function_arg_advance (cumulative_args_t, enum machine_mode,
				      const_tree, bool);
static bool avr_function_ok_for_sibcall (tree, tree);
static void avr_asm_named_section (const char *name, unsigned int flags, tree decl);
static void avr_encode_section_info (tree, rtx, int);
static section* avr_asm_function_rodata_section (tree);
static section* avr_asm_select_section (tree, int, unsigned HOST_WIDE_INT);

/* Allocate registers from r25 to r8 for parameters for function calls.  */
#define FIRST_CUM_REG 26

/* Temporary register RTX (gen_rtx_REG (QImode, TMP_REGNO)) */
static GTY(()) rtx tmp_reg_rtx;

/* Zeroed register RTX (gen_rtx_REG (QImode, ZERO_REGNO)) */
static GTY(()) rtx zero_reg_rtx;

/* AVR register names {"r0", "r1", ..., "r31"} */
static const char *const avr_regnames[] = REGISTER_NAMES;

/* Preprocessor macros to define depending on MCU type.  */
const char *avr_extra_arch_macro;

/* Current architecture.  */
const struct base_arch_s *avr_current_arch;

/* Current device.  */
const struct mcu_type_s *avr_current_device;

/* Section to put switch tables in.  */
static GTY(()) section *progmem_swtable_section;

/* Unnamed section associated to __attribute__((progmem)) aka. PROGMEM.  */
static GTY(()) section *progmem_section;

/* To track if code will use .bss and/or .data.  */
bool avr_need_clear_bss_p = false;
bool avr_need_copy_data_p = false;

/* AVR attributes.  */
static const struct attribute_spec avr_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  { "progmem",   0, 0, false, false, false,  avr_handle_progmem_attribute,
    false },
  { "signal",    0, 0, true,  false, false,  avr_handle_fndecl_attribute,
    false },
  { "interrupt", 0, 0, true,  false, false,  avr_handle_fndecl_attribute,
    false },
  { "naked",     0, 0, false, true,  true,   avr_handle_fntype_attribute,
    false },
  { "OS_task",   0, 0, false, true,  true,   avr_handle_fntype_attribute,
    false },
  { "OS_main",   0, 0, false, true,  true,   avr_handle_fntype_attribute,
    false },
  { NULL,        0, 0, false, false, false, NULL, false }
};

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.long\t"
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.word\t"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.long\t"
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER avr_assemble_integer
#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START avr_file_start
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END avr_file_end

#undef TARGET_ASM_FUNCTION_END_PROLOGUE
#define TARGET_ASM_FUNCTION_END_PROLOGUE avr_asm_function_end_prologue
#undef TARGET_ASM_FUNCTION_BEGIN_EPILOGUE
#define TARGET_ASM_FUNCTION_BEGIN_EPILOGUE avr_asm_function_begin_epilogue

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE avr_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE avr_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P avr_function_value_regno_p

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE avr_attribute_table
#undef TARGET_ASM_FUNCTION_RODATA_SECTION
#define TARGET_ASM_FUNCTION_RODATA_SECTION default_no_function_rodata_section
#undef TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES avr_insert_attributes
#undef TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS avr_section_type_flags

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION avr_asm_named_section
#undef TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS avr_asm_init_sections
#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO avr_encode_section_info
#undef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION avr_asm_select_section

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST avr_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST avr_memory_move_cost
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS avr_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST avr_address_cost
#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG avr_reorg
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG avr_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE avr_function_arg_advance

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS avr_legitimize_address

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY avr_return_in_memory

#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

#undef TARGET_BUILTIN_SETJMP_FRAME_VALUE
#define TARGET_BUILTIN_SETJMP_FRAME_VALUE avr_builtin_setjmp_frame_value

#undef TARGET_HARD_REGNO_SCRATCH_OK
#define TARGET_HARD_REGNO_SCRATCH_OK avr_hard_regno_scratch_ok
#undef TARGET_CASE_VALUES_THRESHOLD
#define TARGET_CASE_VALUES_THRESHOLD avr_case_values_threshold

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P avr_legitimate_address_p

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED avr_frame_pointer_required_p
#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE avr_can_eliminate

#undef TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P avr_class_likely_spilled_p

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE avr_option_override

#undef TARGET_CANNOT_MODIFY_JUMPS_P
#define TARGET_CANNOT_MODIFY_JUMPS_P avr_cannot_modify_jumps_p

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL avr_function_ok_for_sibcall

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS avr_init_builtins

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN avr_expand_builtin

#undef TARGET_ASM_FUNCTION_RODATA_SECTION
#define TARGET_ASM_FUNCTION_RODATA_SECTION avr_asm_function_rodata_section

struct gcc_target targetm = TARGET_INITIALIZER;


/* Custom function to replace string prefix.

   Return a ggc-allocated string with strlen (OLD_PREFIX) characters removed
   from the start of OLD_STR and then prepended with NEW_PREFIX.  */

static inline const char*
avr_replace_prefix (const char *old_str,
                    const char *old_prefix, const char *new_prefix)
{
  char *new_str;
  size_t len = strlen (old_str) + strlen (new_prefix) - strlen (old_prefix);

  gcc_assert (strlen (old_prefix) <= strlen (old_str));

  /* Unfortunately, ggc_alloc_string returns a const char* and thus cannot be
     used here.  */
     
  new_str = (char*) ggc_alloc_atomic (1 + len);

  strcat (stpcpy (new_str, new_prefix), old_str + strlen (old_prefix));
  
  return (const char*) new_str;
}

static void
avr_option_override (void)
{
  flag_delete_null_pointer_checks = 0;

  avr_current_device = &avr_mcu_types[avr_mcu_index];
  avr_current_arch = &avr_arch_types[avr_current_device->arch];
  avr_extra_arch_macro = avr_current_device->macro;

  tmp_reg_rtx  = gen_rtx_REG (QImode, TMP_REGNO);
  zero_reg_rtx = gen_rtx_REG (QImode, ZERO_REGNO);

  init_machine_status = avr_init_machine_status;
}

/* Function to set up the backend function structure.  */

static struct machine_function *
avr_init_machine_status (void)
{
  return ggc_alloc_cleared_machine_function ();
}

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
interrupt_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "interrupt");
}

/* Return nonzero if FUNC is a signal function as specified
   by the "signal" attribute.  */

static int
signal_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "signal");
}

/* Return nonzero if FUNC is a OS_task function.  */

static int
avr_OS_task_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "OS_task");
}

/* Return nonzero if FUNC is a OS_main function.  */

static int
avr_OS_main_function_p (tree func)
{
  return avr_lookup_function_attribute1 (func, "OS_main");
}

/* Return the number of hard registers to push/pop in the prologue/epilogue
   of the current function, and optionally store these registers in SET.  */

static int
avr_regs_to_save (HARD_REG_SET *set)
{
  int reg, count;
  int int_or_sig_p = (interrupt_function_p (current_function_decl)
		      || signal_function_p (current_function_decl));

  if (set)
    CLEAR_HARD_REG_SET (*set);
  count = 0;

  /* No need to save any registers if the function never returns or 
     is have "OS_task" or "OS_main" attribute.  */
  if (TREE_THIS_VOLATILE (current_function_decl)
      || cfun->machine->is_OS_task
      || cfun->machine->is_OS_main)
    return 0;

  for (reg = 0; reg < 32; reg++)
    {
      /* Do not push/pop __tmp_reg__, __zero_reg__, as well as
	 any global register variables.  */
      if (fixed_regs[reg])
	continue;

      if ((int_or_sig_p && !current_function_is_leaf && call_used_regs[reg])
	  || (df_regs_ever_live_p (reg)
	      && (int_or_sig_p || !call_used_regs[reg])
	      && !(frame_pointer_needed
		   && (reg == REG_Y || reg == (REG_Y+1)))))
	{
	  if (set)
	    SET_HARD_REG_BIT (*set, reg);
	  count++;
	}
    }
  return count;
}

/* Return true if register FROM can be eliminated via register TO.  */

bool
avr_can_eliminate (const int from, const int to)
{
  return ((from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
	  || ((from == FRAME_POINTER_REGNUM 
	       || from == FRAME_POINTER_REGNUM + 1)
	      && !frame_pointer_needed));
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

      offset += avr_regs_to_save (NULL);
      return get_frame_size () + (avr_pc_size) + 1 + offset;
    }
}

/* Actual start of frame is virtual_stack_vars_rtx this is offset from 
   frame pointer by +STARTING_FRAME_OFFSET.
   Using saved frame = virtual_stack_vars_rtx - STARTING_FRAME_OFFSET
   avoids creating add/sub of offset in nonlocal goto and setjmp.  */

rtx avr_builtin_setjmp_frame_value (void)
{
  return gen_rtx_MINUS (Pmode, virtual_stack_vars_rtx, 
			 gen_int_mode (STARTING_FRAME_OFFSET, Pmode));
}

/* Return contents of MEM at frame pointer + stack size + 1 (+2 if 3 byte PC).
   This is return address of function.  */
rtx 
avr_return_addr_rtx (int count, rtx tem)
{
  rtx r;
    
  /* Can only return this functions return address. Others not supported.  */
  if (count)
     return NULL;

  if (AVR_3_BYTE_PC)
    {
      r = gen_rtx_SYMBOL_REF (Pmode, ".L__stack_usage+2");
      warning (0, "'builtin_return_address' contains only 2 bytes of address");
    }
  else
    r = gen_rtx_SYMBOL_REF (Pmode, ".L__stack_usage+1");

  r = gen_rtx_PLUS (Pmode, tem, r);
  r = gen_frame_mem (Pmode, memory_address (Pmode, r));
  r = gen_rtx_ROTATE (HImode, r, GEN_INT (8));
  return  r;
}

/* Return 1 if the function epilogue is just a single "ret".  */

int
avr_simple_epilogue (void)
{
  return (! frame_pointer_needed
	  && get_frame_size () == 0
	  && avr_regs_to_save (NULL) == 0
	  && ! interrupt_function_p (current_function_decl)
	  && ! signal_function_p (current_function_decl)
	  && ! avr_naked_function_p (current_function_decl)
	  && ! TREE_THIS_VOLATILE (current_function_decl));
}

/* This function checks sequence of live registers.  */

static int
sequent_regs_live (void)
{
  int reg;
  int live_seq=0;
  int cur_seq=0;

  for (reg = 0; reg < 18; ++reg)
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

      if (df_regs_ever_live_p (REG_Y+1))
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

int
get_sequence_length (rtx insns)
{
  rtx insn;
  int length;
  
  for (insn = insns, length = 0; insn; insn = NEXT_INSN (insn))
    length += get_attr_length (insn);
		
  return length;
}

/*  Implement INCOMING_RETURN_ADDR_RTX.  */

rtx
avr_incoming_return_addr_rtx (void)
{
  /* The return address is at the top of the stack.  Note that the push
     was via post-decrement, which means the actual address is off by one.  */
  return gen_frame_mem (HImode, plus_constant (stack_pointer_rtx, 1));
}

/*  Helper for expand_prologue.  Emit a push of a byte register.  */

static void
emit_push_byte (unsigned regno, bool frame_related_p)
{
  rtx mem, reg, insn;

  mem = gen_rtx_POST_DEC (HImode, stack_pointer_rtx);
  mem = gen_frame_mem (QImode, mem);
  reg = gen_rtx_REG (QImode, regno);

  insn = emit_insn (gen_rtx_SET (VOIDmode, mem, reg));
  if (frame_related_p)
    RTX_FRAME_RELATED_P (insn) = 1;

  cfun->machine->stack_usage++;
}


/*  Output function prologue.  */

void
expand_prologue (void)
{
  int live_seq;
  HARD_REG_SET set;
  int minimize;
  HOST_WIDE_INT size = get_frame_size();
  rtx insn;
  
  /* Init cfun->machine.  */
  cfun->machine->is_naked = avr_naked_function_p (current_function_decl);
  cfun->machine->is_interrupt = interrupt_function_p (current_function_decl);
  cfun->machine->is_signal = signal_function_p (current_function_decl);
  cfun->machine->is_OS_task = avr_OS_task_function_p (current_function_decl);
  cfun->machine->is_OS_main = avr_OS_main_function_p (current_function_decl);
  cfun->machine->stack_usage = 0;
  
  /* Prologue: naked.  */
  if (cfun->machine->is_naked)
    {
      return;
    }

  avr_regs_to_save (&set);
  live_seq = sequent_regs_live ();
  minimize = (TARGET_CALL_PROLOGUES
	      && !cfun->machine->is_interrupt
	      && !cfun->machine->is_signal
	      && !cfun->machine->is_OS_task
	      && !cfun->machine->is_OS_main
	      && live_seq);

  if (cfun->machine->is_interrupt || cfun->machine->is_signal)
    {
      /* Enable interrupts.  */
      if (cfun->machine->is_interrupt)
	emit_insn (gen_enable_interrupt ());
	
      /* Push zero reg.  */
      emit_push_byte (ZERO_REGNO, true);

      /* Push tmp reg.  */
      emit_push_byte (TMP_REGNO, true);

      /* Push SREG.  */
      /* ??? There's no dwarf2 column reserved for SREG.  */
      emit_move_insn (tmp_reg_rtx, gen_rtx_MEM (QImode, GEN_INT (SREG_ADDR)));
      emit_push_byte (TMP_REGNO, false);

      /* Push RAMPZ.  */
      /* ??? There's no dwarf2 column reserved for RAMPZ.  */
      if (AVR_HAVE_RAMPZ 
          && TEST_HARD_REG_BIT (set, REG_Z)
          && TEST_HARD_REG_BIT (set, REG_Z + 1))
        {
          emit_move_insn (tmp_reg_rtx,
			  gen_rtx_MEM (QImode, GEN_INT (RAMPZ_ADDR)));
	  emit_push_byte (TMP_REGNO, false);
        }
	
      /* Clear zero reg.  */
      emit_move_insn (zero_reg_rtx, const0_rtx);

      /* Prevent any attempt to delete the setting of ZERO_REG!  */
      emit_use (zero_reg_rtx);
    }
  if (minimize && (frame_pointer_needed 
		   || (AVR_2_BYTE_PC && live_seq > 6)
		   || live_seq > 7)) 
    {
      int first_reg, reg, offset;

      emit_move_insn (gen_rtx_REG (HImode, REG_X), 
                      gen_int_mode (size, HImode));

      insn = emit_insn (gen_call_prologue_saves
			(gen_int_mode (live_seq, HImode),
		         gen_int_mode (size + live_seq, HImode)));
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Describe the effect of the unspec_volatile call to prologue_saves.
	 Note that this formulation assumes that add_reg_note pushes the
	 notes to the front.  Thus we build them in the reverse order of
	 how we want dwarf2out to process them.  */

      /* The function does always set frame_pointer_rtx, but whether that
	 is going to be permanent in the function is frame_pointer_needed.  */
      add_reg_note (insn, REG_CFA_ADJUST_CFA,
		    gen_rtx_SET (VOIDmode,
				 (frame_pointer_needed
				  ? frame_pointer_rtx : stack_pointer_rtx),
				 plus_constant (stack_pointer_rtx,
						-(size + live_seq))));

      /* Note that live_seq always contains r28+r29, but the other
	 registers to be saved are all below 18.  */
      first_reg = 18 - (live_seq - 2);

      for (reg = 29, offset = -live_seq + 1;
	   reg >= first_reg;
	   reg = (reg == 28 ? 17 : reg - 1), ++offset)
	{
	  rtx m, r;

	  m = gen_rtx_MEM (QImode, plus_constant (stack_pointer_rtx, offset));
	  r = gen_rtx_REG (QImode, reg);
	  add_reg_note (insn, REG_CFA_OFFSET, gen_rtx_SET (VOIDmode, m, r));
	}

      cfun->machine->stack_usage += size + live_seq;
    }
  else
    {
      int reg;
      for (reg = 0; reg < 32; ++reg)
        if (TEST_HARD_REG_BIT (set, reg))
	  emit_push_byte (reg, true);

      if (frame_pointer_needed)
        {
	  if (!(cfun->machine->is_OS_task || cfun->machine->is_OS_main))
	    {
              /* Push frame pointer.  Always be consistent about the
		 ordering of pushes -- epilogue_restores expects the
		 register pair to be pushed low byte first.  */
	      emit_push_byte (REG_Y, true);
	      emit_push_byte (REG_Y + 1, true);
	    }

          if (!size)
            {
              insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
              RTX_FRAME_RELATED_P (insn) = 1;
            }
          else
            {
              /*  Creating a frame can be done by direct manipulation of the
                  stack or via the frame pointer. These two methods are:
                    fp=sp
                    fp-=size
                    sp=fp
                OR
                    sp-=size
                    fp=sp
              the optimum method depends on function type, stack and frame size.
              To avoid a complex logic, both methods are tested and shortest
              is selected.  */
              rtx myfp;
	      rtx fp_plus_insns; 

              if (AVR_HAVE_8BIT_SP)
                {
                  /* The high byte (r29) doesn't change.  Prefer 'subi'
		     (1 cycle) over 'sbiw' (2 cycles, same size).  */
                  myfp = gen_rtx_REG (QImode, FRAME_POINTER_REGNUM);
                }
              else 
                {
                  /*  Normal sized addition.  */
                  myfp = frame_pointer_rtx;
                }

	      /* Method 1-Adjust frame pointer.  */
	      start_sequence ();

	      /* Normally the dwarf2out frame-related-expr interpreter does
		 not expect to have the CFA change once the frame pointer is
		 set up.  Thus we avoid marking the move insn below and
		 instead indicate that the entire operation is complete after
		 the frame pointer subtraction is done.  */

              emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);

              insn = emit_move_insn (myfp, plus_constant (myfp, -size));
              RTX_FRAME_RELATED_P (insn) = 1;
	      add_reg_note (insn, REG_CFA_ADJUST_CFA,
			    gen_rtx_SET (VOIDmode, frame_pointer_rtx,
					 plus_constant (stack_pointer_rtx,
							-size)));

	      /* Copy to stack pointer.  Note that since we've already
		 changed the CFA to the frame pointer this operation
		 need not be annotated at all.  */
	      if (AVR_HAVE_8BIT_SP)
		{
		  emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
		}
	      else if (TARGET_NO_INTERRUPTS 
		       || cfun->machine->is_signal
		       || cfun->machine->is_OS_main)
		{
		  emit_insn (gen_movhi_sp_r_irq_off (stack_pointer_rtx, 
						     frame_pointer_rtx));
		}
	      else if (cfun->machine->is_interrupt)
		{
		  emit_insn (gen_movhi_sp_r_irq_on (stack_pointer_rtx, 
						    frame_pointer_rtx));
		}
	      else
		{
		  emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
		}

	      fp_plus_insns = get_insns ();
	      end_sequence ();

	      /* Method 2-Adjust Stack pointer.  */
              if (size <= 6)
                {
		  rtx sp_plus_insns;

		  start_sequence ();

	          insn = plus_constant (stack_pointer_rtx, -size);
		  insn = emit_move_insn (stack_pointer_rtx, insn);
		  RTX_FRAME_RELATED_P (insn) = 1;
		  
		  insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
		  RTX_FRAME_RELATED_P (insn) = 1;

		  sp_plus_insns = get_insns ();
		  end_sequence ();

		  /* Use shortest method.  */
		  if (get_sequence_length (sp_plus_insns) 
		      < get_sequence_length (fp_plus_insns))
		    emit_insn (sp_plus_insns);
		  else
		    emit_insn (fp_plus_insns);
                }
	      else
		emit_insn (fp_plus_insns);

	      cfun->machine->stack_usage += size;
            }
        }
    }

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->stack_usage;
}

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
  fprintf (file, "/* frame size = " HOST_WIDE_INT_PRINT_DEC " */\n",
                 get_frame_size());
  fprintf (file, "/* stack size = %d */\n",
                 cfun->machine->stack_usage);
  /* Create symbol stack offset here so all functions have it. Add 1 to stack
     usage for offset so that SP + .L__stack_offset = return address.  */
  fprintf (file, ".L__stack_usage = %d\n", cfun->machine->stack_usage);
}


/* Implement EPILOGUE_USES.  */

int
avr_epilogue_uses (int regno ATTRIBUTE_UNUSED)
{
  if (reload_completed 
      && cfun->machine
      && (cfun->machine->is_interrupt || cfun->machine->is_signal))
    return 1;
  return 0;
}

/*  Helper for expand_epilogue.  Emit a pop of a byte register.  */

static void
emit_pop_byte (unsigned regno)
{
  rtx mem, reg;

  mem = gen_rtx_PRE_INC (HImode, stack_pointer_rtx);
  mem = gen_frame_mem (QImode, mem);
  reg = gen_rtx_REG (QImode, regno);

  emit_insn (gen_rtx_SET (VOIDmode, reg, mem));
}

/*  Output RTL epilogue.  */

void
expand_epilogue (bool sibcall_p)
{
  int reg;
  int live_seq;
  HARD_REG_SET set;      
  int minimize;
  HOST_WIDE_INT size = get_frame_size();
  
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
	      && !cfun->machine->is_interrupt
	      && !cfun->machine->is_signal
	      && !cfun->machine->is_OS_task
	      && !cfun->machine->is_OS_main
	      && live_seq);
  
  if (minimize && (frame_pointer_needed || live_seq > 4))
    {
      if (frame_pointer_needed)
	{
          /*  Get rid of frame.  */
	  emit_move_insn(frame_pointer_rtx,
                         gen_rtx_PLUS (HImode, frame_pointer_rtx,
                                       gen_int_mode (size, HImode)));
	}
      else
	{
          emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
	}
	
      emit_insn (gen_epilogue_restores (gen_int_mode (live_seq, HImode)));
    }
  else
    {
      if (frame_pointer_needed)
	{
	  if (size)
	    {
              /* Try two methods to adjust stack and select shortest.  */
	      rtx myfp;
	      rtx fp_plus_insns;

	      if (AVR_HAVE_8BIT_SP)
                {
                  /* The high byte (r29) doesn't change - prefer 'subi' 
                     (1 cycle) over 'sbiw' (2 cycles, same size).  */
                  myfp = gen_rtx_REG (QImode, FRAME_POINTER_REGNUM);
                }
              else 
                {
                  /* Normal sized addition.  */
                  myfp = frame_pointer_rtx;
                }
	      
              /* Method 1-Adjust frame pointer.  */
	      start_sequence ();

	      emit_move_insn (myfp, plus_constant (myfp, size));

	      /* Copy to stack pointer.  */
	      if (AVR_HAVE_8BIT_SP)
		{
		  emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
		}
	      else if (TARGET_NO_INTERRUPTS 
		       || cfun->machine->is_signal)
		{
		  emit_insn (gen_movhi_sp_r_irq_off (stack_pointer_rtx, 
						     frame_pointer_rtx));
		}
	      else if (cfun->machine->is_interrupt)
		{
		  emit_insn (gen_movhi_sp_r_irq_on (stack_pointer_rtx, 
						    frame_pointer_rtx));
		}
	      else
		{
		  emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
		}

	      fp_plus_insns = get_insns ();
	      end_sequence ();	      

              /* Method 2-Adjust Stack pointer.  */
              if (size <= 5)
                {
		  rtx sp_plus_insns;

		  start_sequence ();

		  emit_move_insn (stack_pointer_rtx,
				  plus_constant (stack_pointer_rtx, size));

		  sp_plus_insns = get_insns ();
		  end_sequence ();

		  /* Use shortest method.  */
		  if (get_sequence_length (sp_plus_insns) 
		      < get_sequence_length (fp_plus_insns))
		    emit_insn (sp_plus_insns);
		  else
		    emit_insn (fp_plus_insns);
                }
	      else
		emit_insn (fp_plus_insns);
            }
	  if (!(cfun->machine->is_OS_task || cfun->machine->is_OS_main))
	    {
              /* Restore previous frame_pointer.  See expand_prologue for
		 rationale for not using pophi.  */
	      emit_pop_byte (REG_Y + 1);
	      emit_pop_byte (REG_Y);
	    }
	}

      /* Restore used registers.  */
      for (reg = 31; reg >= 0; --reg)
        if (TEST_HARD_REG_BIT (set, reg))
          emit_pop_byte (reg);

      if (cfun->machine->is_interrupt || cfun->machine->is_signal)
        {
          /* Restore RAMPZ using tmp reg as scratch.  */
	  if (AVR_HAVE_RAMPZ 
              && TEST_HARD_REG_BIT (set, REG_Z)
	      && TEST_HARD_REG_BIT (set, REG_Z + 1))
            {
	      emit_pop_byte (TMP_REGNO);
	      emit_move_insn (gen_rtx_MEM (QImode, GEN_INT (RAMPZ_ADDR)), 
			      tmp_reg_rtx);
	    }

          /* Restore SREG using tmp reg as scratch.  */
          emit_pop_byte (TMP_REGNO);
      
          emit_move_insn (gen_rtx_MEM (QImode, GEN_INT (SREG_ADDR)), 
			  tmp_reg_rtx);

          /* Restore tmp REG.  */
          emit_pop_byte (TMP_REGNO);

          /* Restore zero REG.  */
          emit_pop_byte (ZERO_REGNO);
        }

      if (!sibcall_p)
        emit_jump_insn (gen_return ());
    }
}

/* Output summary messages at beginning of function epilogue.  */

static void
avr_asm_function_begin_epilogue (FILE *file)
{
  fprintf (file, "/* epilogue start */\n");
}


/* Implement TARGET_CANNOT_MODITY_JUMPS_P */

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


/* Return nonzero if X (an RTX) is a legitimate memory address on the target
   machine for a memory operand of mode MODE.  */

bool
avr_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  enum reg_class r = NO_REGS;
  
  if (TARGET_ALL_DEBUG)
    {
      fprintf (stderr, "mode: (%s) %s %s %s %s:",
	       GET_MODE_NAME(mode),
	       strict ? "(strict)": "",
	       reload_completed ? "(reload_completed)": "",
	       reload_in_progress ? "(reload_in_progress)": "",
	       reg_renumber ? "(reg_renumber)" : "");
      if (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) <= MAX_LD_OFFSET (mode)
	  && reg_renumber
	  )
	fprintf (stderr, "(r%d ---> r%d)", REGNO (XEXP (x, 0)),
		 true_regnum (XEXP (x, 0)));
      debug_rtx (x);
    }
  
  if (REG_P (x) && (strict ? REG_OK_FOR_BASE_STRICT_P (x)
                    : REG_OK_FOR_BASE_NOSTRICT_P (x)))
    r = POINTER_REGS;
  else if (CONSTANT_ADDRESS_P (x))
    r = ALL_REGS;
  else if (GET_CODE (x) == PLUS
           && REG_P (XEXP (x, 0))
	   && GET_CODE (XEXP (x, 1)) == CONST_INT
	   && INTVAL (XEXP (x, 1)) >= 0)
    {
      int fit = INTVAL (XEXP (x, 1)) <= MAX_LD_OFFSET (mode);
      if (fit)
	{
	  if (! strict
	      || REGNO (XEXP (x,0)) == REG_X
	      || REGNO (XEXP (x,0)) == REG_Y
	      || REGNO (XEXP (x,0)) == REG_Z)
	    r = BASE_POINTER_REGS;
	  if (XEXP (x,0) == frame_pointer_rtx
	      || XEXP (x,0) == arg_pointer_rtx)
	    r = BASE_POINTER_REGS;
	}
      else if (frame_pointer_needed && XEXP (x,0) == frame_pointer_rtx)
	r = POINTER_Y_REGS;
    }
  else if ((GET_CODE (x) == PRE_DEC || GET_CODE (x) == POST_INC)
           && REG_P (XEXP (x, 0))
           && (strict ? REG_OK_FOR_BASE_STRICT_P (XEXP (x, 0))
               : REG_OK_FOR_BASE_NOSTRICT_P (XEXP (x, 0))))
    {
      r = POINTER_REGS;
    }
  if (TARGET_ALL_DEBUG)
    {
      fprintf (stderr, "   ret = %c\n", r + '0');
    }
  return r == NO_REGS ? 0 : (int)r;
}

/* Attempts to replace X with a valid
   memory address for an operand of mode MODE  */

rtx
avr_legitimize_address (rtx x, rtx oldx, enum machine_mode mode)
{
  x = oldx;
  if (TARGET_ALL_DEBUG)
    {
      fprintf (stderr, "legitimize_address mode: %s", GET_MODE_NAME(mode));
      debug_rtx (oldx);
    }
  
  if (GET_CODE (oldx) == PLUS
      && REG_P (XEXP (oldx,0)))
    {
      if (REG_P (XEXP (oldx,1)))
	x = force_reg (GET_MODE (oldx), oldx);
      else if (GET_CODE (XEXP (oldx, 1)) == CONST_INT)
	{
	  int offs = INTVAL (XEXP (oldx,1));
	  if (frame_pointer_rtx != XEXP (oldx,0))
	    if (offs > MAX_LD_OFFSET (mode))
	      {
		if (TARGET_ALL_DEBUG)
		  fprintf (stderr, "force_reg (big offset)\n");
		x = force_reg (GET_MODE (oldx), oldx);
	      }
	}
    }
  return x;
}


/* Helper function to print assembler resp. track instruction
   sequence lengths.
   
   If PLEN == NULL:
       Output assembler code from template TPL with operands supplied
       by OPERANDS.  This is just forwarding to output_asm_insn.
   
   If PLEN != NULL:
       Add N_WORDS to *PLEN.
       Don't output anything.
*/

static void
avr_asm_len (const char* tpl, rtx* operands, int* plen, int n_words)
{
  if (NULL == plen)
    {
      output_asm_insn (tpl, operands);
    }
  else
    {
      *plen += n_words;
    }
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
      output_operand_lossage ("address operand requires constraint for X, Y, or Z register");
    }
  return NULL;
}

/* Return the condition name as a string.
   Used in conditional jump constructing  */

static const char *
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
}

/* Output ADDR to FILE as address.  */

void
print_operand_address (FILE *file, rtx addr)
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, ptrreg_to_str (REGNO (addr)));
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
	  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x,1)) == CONST_INT)
	    {
	      /* Assembler gs() will implant word address. Make offset 
		 a byte offset inside gs() for assembler. This is 
		 needed because the more logical (constant+gs(sym)) is not 
		 accepted by gas. For 128K and lower devices this is ok. For
		 large devices it will create a Trampoline to offset from symbol 
		 which may not be what the user really wanted.  */
	      fprintf (file, "gs(");
	      output_addr_const (file, XEXP (x,0));
	      fprintf (file,"+" HOST_WIDE_INT_PRINT_DEC ")", 2 * INTVAL (XEXP (x,1)));
	      if (AVR_3_BYTE_PC)
	        if (warning (0, "pointer offset from symbol maybe incorrect"))
		  {
		    output_addr_const (stderr, addr);
		    fprintf(stderr,"\n");
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


/* Output X as assembler operand to file FILE.  */
     
void
print_operand (FILE *file, rtx x, int code)
{
  int abcd = 0;

  if (code >= 'A' && code <= 'D')
    abcd = code - 'A';

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
  else if (REG_P (x))
    {
      if (x == zero_reg_rtx)
	fprintf (file, "__zero_reg__");
      else
	fprintf (file, reg_names[true_regnum (x) + abcd]);
    }
  else if (GET_CODE (x) == CONST_INT)
    fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) + abcd);
  else if (GET_CODE (x) == MEM)
    {
      rtx addr = XEXP (x,0);
      if (code == 'm')
	{
	   if (!CONSTANT_P (addr))
	    fatal_insn ("bad address, not a constant):", addr);
	  /* Assembler template with m-code is data - not progmem section */
	  if (text_segment_operand (addr, VOIDmode))
	    if (warning ( 0, "accessing data memory with program memory address"))
	      {
		output_addr_const (stderr, addr);
		fprintf(stderr,"\n");
	      }
	  output_addr_const (file, addr);
	}
      else if (code == 'o')
	{
	  if (GET_CODE (addr) != PLUS)
	    fatal_insn ("bad address, not (reg+disp):", addr);

	  print_operand (file, XEXP (addr, 1), 0);
	}
      else if (code == 'p' || code == 'r')
        {
          if (GET_CODE (addr) != POST_INC && GET_CODE (addr) != PRE_DEC)
            fatal_insn ("bad address, not post_inc or pre_dec:", addr);
          
          if (code == 'p')
            print_operand_address (file, XEXP (addr, 0));  /* X, Y, Z */
          else
            print_operand (file, XEXP (addr, 0), 0);  /* r26, r28, r30 */
        }
      else if (GET_CODE (addr) == PLUS)
	{
	  print_operand_address (file, XEXP (addr,0));
	  if (REGNO (XEXP (addr, 0)) == REG_X)
	    fatal_insn ("internal compiler error.  Bad address:"
			,addr);
	  fputc ('+', file);
	  print_operand (file, XEXP (addr,1), code);
	}
      else
	print_operand_address (file, addr);
    }
  else if (code == 'x')
    {
      /* Constant progmem address - like used in jmp or call */
      if (0 == text_segment_operand (x, VOIDmode))
	    if (warning ( 0, "accessing program  memory with data memory address"))
	  {
	    output_addr_const (stderr, x);
	    fprintf(stderr,"\n");
	  }
      /* Use normal symbol for direct address no linker trampoline needed */
      output_addr_const (file, x);
    }
  else if (GET_CODE (x) == CONST_DOUBLE)
    {
      long val;
      REAL_VALUE_TYPE rv;
      if (GET_MODE (x) != SFmode)
	fatal_insn ("internal compiler error.  Unknown mode:", x);
      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_SINGLE (rv, val);
      fprintf (file, "0x%lx", val);
    }
  else if (code == 'j')
    fputs (cond_string (GET_CODE (x)), file);
  else if (code == 'k')
    fputs (cond_string (reverse_condition (GET_CODE (x))), file);
  else
    print_operand_address (file, x);
}

/* Update the condition code in the INSN.  */

void
notice_update_cc (rtx body ATTRIBUTE_UNUSED, rtx insn)
{
  rtx set;
  
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Insn does not affect CC at all.  */
      break;

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

      /* Correct CC for the ashrqi3 with the shift count as CONST_INT < 6 */
      set = single_set (insn);
      if (set)
	{
	  rtx src = SET_SRC (set);
	  
	  if (GET_CODE (src) == ASHIFTRT
	      && GET_MODE (src) == QImode)
	    {
	      rtx x = XEXP (src, 1);

	      if (CONST_INT_P (x)
		  && IN_RANGE (INTVAL (x), 1, 5))
		{
		  cc_status.value1 = SET_DEST (set);
		  cc_status.flags |= CC_OVERFLOW_UNUSABLE;
		}
	    }
	}
      break;
    }
}

/* Choose mode for jump insn:
   1 - relative jump in range -63 <= x <= 62 ;
   2 - relative jump in range -2046 <= x <= 2045 ;
   3 - absolute jump (only for ATmega[16]03).  */

int
avr_jump_mode (rtx x, rtx insn)
{
  int dest_addr = INSN_ADDRESSES (INSN_UID (GET_CODE (x) == LABEL_REF
					    ? XEXP (x, 0) : x));
  int cur_addr = INSN_ADDRESSES (INSN_UID (insn));
  int jump_distance = cur_addr - dest_addr;
  
  if (-63 <= jump_distance && jump_distance <= 62)
    return 1;
  else if (-2046 <= jump_distance && jump_distance <= 2045)
    return 2;
  else if (AVR_HAVE_JMP_CALL)
    return 3;
  
  return 2;
}

/* return an AVR condition jump commands.
   X is a comparison RTX.
   LEN is a number returned by avr_jump_mode function.
   if REVERSE nonzero then condition code in X must be reversed.  */

const char *
ret_cond_branch (rtx x, int len, int reverse)
{
  RTX_CODE cond = reverse ? reverse_condition (GET_CODE (x)) : GET_CODE (x);
  
  switch (cond)
    {
    case GT:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return (len == 1 ? (AS1 (breq,.+2) CR_TAB
			    AS1 (brpl,%0)) :
		len == 2 ? (AS1 (breq,.+4) CR_TAB
			    AS1 (brmi,.+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,.+6) CR_TAB
		 AS1 (brmi,.+4) CR_TAB
		 AS1 (jmp,%0)));
	  
      else
	return (len == 1 ? (AS1 (breq,.+2) CR_TAB
			    AS1 (brge,%0)) :
		len == 2 ? (AS1 (breq,.+4) CR_TAB
			    AS1 (brlt,.+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,.+6) CR_TAB
		 AS1 (brlt,.+4) CR_TAB
		 AS1 (jmp,%0)));
    case GTU:
      return (len == 1 ? (AS1 (breq,.+2) CR_TAB
                          AS1 (brsh,%0)) :
              len == 2 ? (AS1 (breq,.+4) CR_TAB
                          AS1 (brlo,.+2) CR_TAB
                          AS1 (rjmp,%0)) :
              (AS1 (breq,.+6) CR_TAB
               AS1 (brlo,.+4) CR_TAB
               AS1 (jmp,%0)));
    case LE:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return (len == 1 ? (AS1 (breq,%0) CR_TAB
			    AS1 (brmi,%0)) :
		len == 2 ? (AS1 (breq,.+2) CR_TAB
			    AS1 (brpl,.+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,.+2) CR_TAB
		 AS1 (brpl,.+4) CR_TAB
		 AS1 (jmp,%0)));
      else
	return (len == 1 ? (AS1 (breq,%0) CR_TAB
			    AS1 (brlt,%0)) :
		len == 2 ? (AS1 (breq,.+2) CR_TAB
			    AS1 (brge,.+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,.+2) CR_TAB
		 AS1 (brge,.+4) CR_TAB
		 AS1 (jmp,%0)));
    case LEU:
      return (len == 1 ? (AS1 (breq,%0) CR_TAB
                          AS1 (brlo,%0)) :
              len == 2 ? (AS1 (breq,.+2) CR_TAB
                          AS1 (brsh,.+2) CR_TAB
			  AS1 (rjmp,%0)) :
              (AS1 (breq,.+2) CR_TAB
               AS1 (brsh,.+4) CR_TAB
	       AS1 (jmp,%0)));
    default:
      if (reverse)
	{
	  switch (len)
	    {
	    case 1:
	      return AS1 (br%k1,%0);
	    case 2:
	      return (AS1 (br%j1,.+2) CR_TAB
		      AS1 (rjmp,%0));
	    default:
	      return (AS1 (br%j1,.+4) CR_TAB
		      AS1 (jmp,%0));
	    }
	}
	else
	  {
	    switch (len)
	      {
	      case 1:
		return AS1 (br%j1,%0);
	      case 2:
		return (AS1 (br%k1,.+2) CR_TAB
			AS1 (rjmp,%0));
	      default:
		return (AS1 (br%k1,.+4) CR_TAB
			AS1 (jmp,%0));
	      }
	  }
    }
  return "";
}

/* Output insn cost for next insn.  */

void
final_prescan_insn (rtx insn, rtx *operand ATTRIBUTE_UNUSED,
                    int num_operands ATTRIBUTE_UNUSED)
{
  if (TARGET_ALL_DEBUG)
    {
      rtx set = single_set (insn);

      if (set)
        fprintf (asm_out_file, "/* DEBUG: cost = %d.  */\n",
                 set_src_cost (SET_SRC (set), optimize_insn_for_speed_p ()));
      else
        fprintf (asm_out_file, "/* DEBUG: pattern-cost = %d.  */\n",
                 rtx_cost (PATTERN (insn), INSN, 0,
			   optimize_insn_for_speed_p()));
    }
}

/* Return 0 if undefined, 1 if always true or always false.  */

int
avr_simplify_comparison_p (enum machine_mode mode, RTX_CODE op, rtx x)
{
  unsigned int max = (mode == QImode ? 0xff :
                      mode == HImode ? 0xffff :
                      mode == SImode ? 0xffffffff : 0);
  if (max && op && GET_CODE (x) == CONST_INT)
    {
      if (unsigned_condition (op) != op)
	max >>= 1;

      if (max != (INTVAL (x) & max)
	  && INTVAL (x) != 0xff)
	return 1;
    }
  return 0;
}


/* Returns nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */

int
function_arg_regno_p(int r)
{
  return (r >= 8 && r <= 25);
}

/* Initializing the variable cum for the state at the beginning
   of the argument list.  */

void
init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype, rtx libname,
		      tree fndecl ATTRIBUTE_UNUSED)
{
  cum->nregs = 18;
  cum->regno = FIRST_CUM_REG;
  if (!libname && stdarg_p (fntype))
    cum->nregs = 0;

  /* Assume the calle may be tail called */
  
  cfun->machine->sibcall_fails = 0;
}

/* Returns the number of registers to allocate for a function argument.  */

static int
avr_num_arg_regs (enum machine_mode mode, const_tree type)
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

/* Controls whether a function argument is passed
   in a register, and which register.  */

static rtx
avr_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		  const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = avr_num_arg_regs (mode, type);

  if (cum->nregs && bytes <= cum->nregs)
    return gen_rtx_REG (mode, cum->regno - bytes);

  return NULL_RTX;
}

/* Update the summarizer variable CUM to advance past an argument
   in the argument list.  */
   
static void
avr_function_arg_advance (cumulative_args_t cum_v, enum machine_mode mode,
			  const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = avr_num_arg_regs (mode, type);

  cum->nregs -= bytes;
  cum->regno -= bytes;

  /* A parameter is being passed in a call-saved register. As the original
     contents of these regs has to be restored before leaving the function,
     a function must not pass arguments in call-saved regs in order to get
     tail-called. */
  
  if (cum->regno >= 8
      && cum->nregs >= 0
      && !call_used_regs[cum->regno])
    {
      /* FIXME: We ship info on failing tail-call in struct machine_function.
         This uses internals of calls.c:expand_call() and the way args_so_far
         is used. targetm.function_ok_for_sibcall() needs to be extended to
         pass &args_so_far, too. At present, CUMULATIVE_ARGS is target
         dependent so that such an extension is not wanted. */
      
      cfun->machine->sibcall_fails = 1;
    }

  /* Test if all registers needed by the ABI are actually available.  If the
     user has fixed a GPR needed to pass an argument, an (implicit) function
     call would clobber that fixed register.  See PR45099 for an example.  */
  
  if (cum->regno >= 8
      && cum->nregs >= 0)
    {
      int regno;

      for (regno = cum->regno; regno < cum->regno + bytes; regno++)
        if (fixed_regs[regno])
          error ("Register %s is needed to pass a parameter but is fixed",
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
   CALL_EXPR representing the call. */

static bool
avr_function_ok_for_sibcall (tree decl_callee, tree exp_callee)
{
  tree fntype_callee;

  /* Tail-calling must fail if callee-saved regs are used to pass
     function args.  We must not tail-call when `epilogue_restores'
     is used.  Unfortunately, we cannot tell at this point if that
     actually will happen or not, and we cannot step back from
     tail-calling. Thus, we inhibit tail-calling with -mcall-prologues. */
  
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
  
  if (interrupt_function_p (current_function_decl)
      || signal_function_p (current_function_decl)
      || avr_naked_function_p (decl_callee)
      || avr_naked_function_p (current_function_decl)
      /* FIXME: For OS_task and OS_main, we are over-conservative.
         This is due to missing documentation of these attributes
         and what they actually should do and should not do. */
      || (avr_OS_task_function_p (decl_callee)
          != avr_OS_task_function_p (current_function_decl))
      || (avr_OS_main_function_p (decl_callee)
          != avr_OS_main_function_p (current_function_decl)))
    {
      return false;
    }
 
  return true;
}

/***********************************************************************
  Functions for outputting various mov's for a various modes
************************************************************************/
const char *
output_movqi (rtx insn, rtx operands[], int *l)
{
  int dummy;
  rtx dest = operands[0];
  rtx src = operands[1];
  int *real_l = l;
  
  if (!l)
    l = &dummy;

  *l = 1;
  
  if (register_operand (dest, QImode))
    {
      if (register_operand (src, QImode)) /* mov r,r */
	{
	  if (test_hard_reg_class (STACK_REG, dest))
	    return AS2 (out,%0,%1);
	  else if (test_hard_reg_class (STACK_REG, src))
	    return AS2 (in,%0,%1);
	  
	  return AS2 (mov,%0,%1);
	}
      else if (CONSTANT_P (src))
	{
	  if (test_hard_reg_class (LD_REGS, dest)) /* ldi d,i */
	    return AS2 (ldi,%0,lo8(%1));
	  
	  if (GET_CODE (src) == CONST_INT)
	    {
	      if (src == const0_rtx) /* mov r,L */
		return AS1 (clr,%0);
	      else if (src == const1_rtx)
		{
		  *l = 2;
		  return (AS1 (clr,%0) CR_TAB
			  AS1 (inc,%0));
		}
	      else if (src == constm1_rtx)
		{
		  /* Immediate constants -1 to any register */
		  *l = 2;
		  return (AS1 (clr,%0) CR_TAB
			  AS1 (dec,%0));
		}
	      else
		{
		  int bit_nr = exact_log2 (INTVAL (src));

		  if (bit_nr >= 0)
		    {
		      *l = 3;
		      if (!real_l)
			output_asm_insn ((AS1 (clr,%0) CR_TAB
					  "set"), operands);
		      if (!real_l)
			avr_output_bld (operands, bit_nr);

		      return "";
		    }
		}
	    }
	  
	  /* Last resort, larger than loading from memory.  */
	  *l = 4;
	  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
		  AS2 (ldi,r31,lo8(%1))     CR_TAB
		  AS2 (mov,%0,r31)          CR_TAB
		  AS2 (mov,r31,__tmp_reg__));
	}
      else if (GET_CODE (src) == MEM)
	return out_movqi_r_mr (insn, operands, real_l); /* mov r,m */
    }
  else if (GET_CODE (dest) == MEM)
    {
      const char *templ;

      if (src == const0_rtx)
	operands[1] = zero_reg_rtx;

      templ = out_movqi_mr_r (insn, operands, real_l);

      if (!real_l)
	output_asm_insn (templ, operands);

      operands[1] = src;
    }
  return "";
}


const char *
output_movhi (rtx insn, rtx operands[], int *l)
{
  int dummy;
  rtx dest = operands[0];
  rtx src = operands[1];
  int *real_l = l;
  
  if (!l)
    l = &dummy;
  
  if (register_operand (dest, HImode))
    {
      if (register_operand (src, HImode)) /* mov r,r */
	{
	  if (test_hard_reg_class (STACK_REG, dest))
	    {
	      if (AVR_HAVE_8BIT_SP)
		return *l = 1, AS2 (out,__SP_L__,%A1);
              /* Use simple load of stack pointer if no interrupts are 
		 used.  */
	      else if (TARGET_NO_INTERRUPTS)
		return *l = 2, (AS2 (out,__SP_H__,%B1) CR_TAB
				AS2 (out,__SP_L__,%A1));
	      *l = 5;
	      return (AS2 (in,__tmp_reg__,__SREG__)  CR_TAB
		      "cli"                          CR_TAB
		      AS2 (out,__SP_H__,%B1)         CR_TAB
		      AS2 (out,__SREG__,__tmp_reg__) CR_TAB
		      AS2 (out,__SP_L__,%A1));
	    }
	  else if (test_hard_reg_class (STACK_REG, src))
	    {
	      *l = 2;	
	      return (AS2 (in,%A0,__SP_L__) CR_TAB
		      AS2 (in,%B0,__SP_H__));
	    }

	  if (AVR_HAVE_MOVW)
	    {
	      *l = 1;
	      return (AS2 (movw,%0,%1));
	    }
	  else
	    {
	      *l = 2;
	      return (AS2 (mov,%A0,%A1) CR_TAB
		      AS2 (mov,%B0,%B1));
	    }
	}
      else if (CONSTANT_P (src))
	{
	  if (test_hard_reg_class (LD_REGS, dest)) /* ldi d,i */
	    {
	      *l = 2;
	      return (AS2 (ldi,%A0,lo8(%1)) CR_TAB
		      AS2 (ldi,%B0,hi8(%1)));
	    }
	  
	  if (GET_CODE (src) == CONST_INT)
	    {
	      if (src == const0_rtx) /* mov r,L */
		{
		  *l = 2;
		  return (AS1 (clr,%A0) CR_TAB
			  AS1 (clr,%B0));
		}
	      else if (src == const1_rtx)
		{
		  *l = 3;
		  return (AS1 (clr,%A0) CR_TAB
			  AS1 (clr,%B0) CR_TAB
			  AS1 (inc,%A0));
		}
	      else if (src == constm1_rtx)
		{
		  /* Immediate constants -1 to any register */
		  *l = 3;
		  return (AS1 (clr,%0)  CR_TAB
			  AS1 (dec,%A0) CR_TAB
			  AS2 (mov,%B0,%A0));
		}
	      else
		{
		  int bit_nr = exact_log2 (INTVAL (src));

		  if (bit_nr >= 0)
		    {
		      *l = 4;
		      if (!real_l)
			output_asm_insn ((AS1 (clr,%A0) CR_TAB
					  AS1 (clr,%B0) CR_TAB
					  "set"), operands);
		      if (!real_l)
			avr_output_bld (operands, bit_nr);

		      return "";
		    }
		}

	      if ((INTVAL (src) & 0xff) == 0)
		{
		  *l = 5;
		  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
			  AS1 (clr,%A0)             CR_TAB
			  AS2 (ldi,r31,hi8(%1))     CR_TAB
			  AS2 (mov,%B0,r31)         CR_TAB
			  AS2 (mov,r31,__tmp_reg__));
		}
	      else if ((INTVAL (src) & 0xff00) == 0)
		{
		  *l = 5;
		  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
			  AS2 (ldi,r31,lo8(%1))     CR_TAB
			  AS2 (mov,%A0,r31)         CR_TAB
			  AS1 (clr,%B0)             CR_TAB
			  AS2 (mov,r31,__tmp_reg__));
		}
	    }
	  
	  /* Last resort, equal to loading from memory.  */
	  *l = 6;
	  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
		  AS2 (ldi,r31,lo8(%1))     CR_TAB
		  AS2 (mov,%A0,r31)         CR_TAB
		  AS2 (ldi,r31,hi8(%1))     CR_TAB
		  AS2 (mov,%B0,r31)         CR_TAB
		  AS2 (mov,r31,__tmp_reg__));
	}
      else if (GET_CODE (src) == MEM)
	return out_movhi_r_mr (insn, operands, real_l); /* mov r,m */
    }
  else if (GET_CODE (dest) == MEM)
    {
      const char *templ;

      if (src == const0_rtx)
	operands[1] = zero_reg_rtx;

      templ = out_movhi_mr_r (insn, operands, real_l);

      if (!real_l)
	output_asm_insn (templ, operands);

      operands[1] = src;
      return "";
    }
  fatal_insn ("invalid insn:", insn);
  return "";
}

const char *
out_movqi_r_mr (rtx insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (src, 0);
  int dummy;
  
  if (!l)
    l = &dummy;
  
  if (CONSTANT_ADDRESS_P (x))
    {
      if (CONST_INT_P (x) && INTVAL (x) == SREG_ADDR)
	{
	  *l = 1;
	  return AS2 (in,%0,__SREG__);
	}
      if (optimize > 0 && io_address_operand (x, QImode))
	{
	  *l = 1;
	  return AS2 (in,%0,%m1-0x20);
	}
      *l = 2;
      return AS2 (lds,%0,%m1);
    }
  /* memory access by reg+disp */
  else if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x,0))
      && GET_CODE (XEXP (x,1)) == CONST_INT)
    {
      if ((INTVAL (XEXP (x,1)) - GET_MODE_SIZE (GET_MODE (src))) >= 63)
	{
	  int disp = INTVAL (XEXP (x,1));
	  if (REGNO (XEXP (x,0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return *l = 3, (AS2 (adiw,r28,%o1-63) CR_TAB
			    AS2 (ldd,%0,Y+63)     CR_TAB
			    AS2 (sbiw,r28,%o1-63));

	  return *l = 5, (AS2 (subi,r28,lo8(-%o1)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o1)) CR_TAB
			  AS2 (ld,%0,Y)            CR_TAB
			  AS2 (subi,r28,lo8(%o1))  CR_TAB
			  AS2 (sbci,r29,hi8(%o1)));
	}
      else if (REGNO (XEXP (x,0)) == REG_X)
	{
	  /* This is a paranoid case LEGITIMIZE_RELOAD_ADDRESS must exclude
	     it but I have this situation with extremal optimizing options.  */
	  if (reg_overlap_mentioned_p (dest, XEXP (x,0))
	      || reg_unused_after (insn, XEXP (x,0)))
	    return *l = 2, (AS2 (adiw,r26,%o1) CR_TAB
			    AS2 (ld,%0,X));

	  return *l = 3, (AS2 (adiw,r26,%o1) CR_TAB
			  AS2 (ld,%0,X)      CR_TAB
			  AS2 (sbiw,r26,%o1));
	}
      *l = 1;
      return AS2 (ldd,%0,%1);
    }
  *l = 1;
  return AS2 (ld,%0,%1);
}

const char *
out_movhi_r_mr (rtx insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);
  /* "volatile" forces reading low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers.  */
  int mem_volatile_p = MEM_VOLATILE_P (src);
  int tmp;

  if (!l)
    l = &tmp;

  if (reg_base > 0)
    {
      if (reg_dest == reg_base)         /* R = (R) */
	{
	  *l = 3;
	  return (AS2 (ld,__tmp_reg__,%1+) CR_TAB
		  AS2 (ld,%B0,%1) CR_TAB
		  AS2 (mov,%A0,__tmp_reg__));
	}
      else if (reg_base == REG_X)        /* (R26) */
        {
          if (reg_unused_after (insn, base))
	    {
	      *l = 2;
	      return (AS2 (ld,%A0,X+) CR_TAB
		      AS2 (ld,%B0,X));
	    }
	  *l  = 3;
	  return (AS2 (ld,%A0,X+) CR_TAB
		  AS2 (ld,%B0,X) CR_TAB
		  AS2 (sbiw,r26,1));
        }
      else                      /* (R)  */
	{
	  *l = 2;
	  return (AS2 (ld,%A0,%1)    CR_TAB
		  AS2 (ldd,%B0,%1+1));
	}
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));
      int reg_base = true_regnum (XEXP (base, 0));
      
      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
	{
	  if (REGNO (XEXP (base, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);
	  
	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return *l = 4, (AS2 (adiw,r28,%o1-62) CR_TAB
			    AS2 (ldd,%A0,Y+62)    CR_TAB
			    AS2 (ldd,%B0,Y+63)    CR_TAB
			    AS2 (sbiw,r28,%o1-62));

	  return *l = 6, (AS2 (subi,r28,lo8(-%o1)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o1)) CR_TAB
			  AS2 (ld,%A0,Y)           CR_TAB
			  AS2 (ldd,%B0,Y+1)        CR_TAB
			  AS2 (subi,r28,lo8(%o1))  CR_TAB
			  AS2 (sbci,r29,hi8(%o1)));
	}
      if (reg_base == REG_X)
	{
	  /* This is a paranoid case. LEGITIMIZE_RELOAD_ADDRESS must exclude
	     it but I have this situation with extremal
	     optimization options.  */
	  
	  *l = 4;
	  if (reg_base == reg_dest)
	    return (AS2 (adiw,r26,%o1)      CR_TAB
		    AS2 (ld,__tmp_reg__,X+) CR_TAB
		    AS2 (ld,%B0,X)          CR_TAB
		    AS2 (mov,%A0,__tmp_reg__));

	  return (AS2 (adiw,r26,%o1) CR_TAB
		  AS2 (ld,%A0,X+)    CR_TAB
		  AS2 (ld,%B0,X)     CR_TAB
		  AS2 (sbiw,r26,%o1+1));
	}

      if (reg_base == reg_dest)
	{
	  *l = 3;
	  return (AS2 (ldd,__tmp_reg__,%A1) CR_TAB
		  AS2 (ldd,%B0,%B1)         CR_TAB
		  AS2 (mov,%A0,__tmp_reg__));
	}
      
      *l = 2;
      return (AS2 (ldd,%A0,%A1) CR_TAB
	      AS2 (ldd,%B0,%B1));
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    {
      if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
	fatal_insn ("incorrect insn:", insn);

      if (mem_volatile_p)
        {
          if (REGNO (XEXP (base, 0)) == REG_X)
            {
              *l = 4;
              return (AS2 (sbiw,r26,2)  CR_TAB
                      AS2 (ld,%A0,X+)   CR_TAB
                      AS2 (ld,%B0,X)    CR_TAB
                      AS2 (sbiw,r26,1));
            }
          else
            {
              *l = 3;
              return (AS2 (sbiw,%r1,2)   CR_TAB
                      AS2 (ld,%A0,%p1)  CR_TAB
                      AS2 (ldd,%B0,%p1+1));
            }
        }

      *l = 2;
      return (AS2 (ld,%B0,%1) CR_TAB
	      AS2 (ld,%A0,%1));
    }
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    {
      if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
	fatal_insn ("incorrect insn:", insn);

      *l = 2;
      return (AS2 (ld,%A0,%1)  CR_TAB
	      AS2 (ld,%B0,%1));
    }
  else if (CONSTANT_ADDRESS_P (base))
    {
      if (optimize > 0 && io_address_operand (base, HImode))
	{
	  *l = 2;
	  return (AS2 (in,%A0,%m1-0x20) CR_TAB
		  AS2 (in,%B0,%m1+1-0x20));
	}
      *l = 4;
      return (AS2 (lds,%A0,%m1) CR_TAB
	      AS2 (lds,%B0,%m1+1));
    }
  
  fatal_insn ("unknown move insn:",insn);
  return "";
}

const char *
out_movsi_r_mr (rtx insn, rtx op[], int *l)
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
      if (reg_base == REG_X)        /* (R26) */
        {
          if (reg_dest == REG_X)
	    /* "ld r26,-X" is undefined */
	    return *l=7, (AS2 (adiw,r26,3)        CR_TAB
			  AS2 (ld,r29,X)          CR_TAB
			  AS2 (ld,r28,-X)         CR_TAB
			  AS2 (ld,__tmp_reg__,-X) CR_TAB
			  AS2 (sbiw,r26,1)        CR_TAB
			  AS2 (ld,r26,X)          CR_TAB
			  AS2 (mov,r27,__tmp_reg__));
          else if (reg_dest == REG_X - 2)
            return *l=5, (AS2 (ld,%A0,X+)  CR_TAB
                          AS2 (ld,%B0,X+) CR_TAB
                          AS2 (ld,__tmp_reg__,X+)  CR_TAB
                          AS2 (ld,%D0,X)  CR_TAB
                          AS2 (mov,%C0,__tmp_reg__));
          else if (reg_unused_after (insn, base))
            return  *l=4, (AS2 (ld,%A0,X+)  CR_TAB
                           AS2 (ld,%B0,X+) CR_TAB
                           AS2 (ld,%C0,X+) CR_TAB
                           AS2 (ld,%D0,X));
          else
            return  *l=5, (AS2 (ld,%A0,X+)  CR_TAB
                           AS2 (ld,%B0,X+) CR_TAB
                           AS2 (ld,%C0,X+) CR_TAB
                           AS2 (ld,%D0,X)  CR_TAB
                           AS2 (sbiw,r26,3));
        }
      else
        {
          if (reg_dest == reg_base)
            return *l=5, (AS2 (ldd,%D0,%1+3) CR_TAB
                          AS2 (ldd,%C0,%1+2) CR_TAB
                          AS2 (ldd,__tmp_reg__,%1+1)  CR_TAB
                          AS2 (ld,%A0,%1)  CR_TAB
                          AS2 (mov,%B0,__tmp_reg__));
          else if (reg_base == reg_dest + 2)
            return *l=5, (AS2 (ld ,%A0,%1)    CR_TAB
                          AS2 (ldd,%B0,%1+1) CR_TAB
                          AS2 (ldd,__tmp_reg__,%1+2)  CR_TAB
                          AS2 (ldd,%D0,%1+3) CR_TAB
                          AS2 (mov,%C0,__tmp_reg__));
          else
            return *l=4, (AS2 (ld ,%A0,%1)   CR_TAB
                          AS2 (ldd,%B0,%1+1) CR_TAB
                          AS2 (ldd,%C0,%1+2) CR_TAB
                          AS2 (ldd,%D0,%1+3));
        }
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));
      
      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
	{
	  if (REGNO (XEXP (base, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return *l = 6, (AS2 (adiw,r28,%o1-60) CR_TAB
			    AS2 (ldd,%A0,Y+60)    CR_TAB
			    AS2 (ldd,%B0,Y+61)    CR_TAB
			    AS2 (ldd,%C0,Y+62)    CR_TAB
			    AS2 (ldd,%D0,Y+63)    CR_TAB
			    AS2 (sbiw,r28,%o1-60));

	  return *l = 8, (AS2 (subi,r28,lo8(-%o1)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o1)) CR_TAB
			  AS2 (ld,%A0,Y)           CR_TAB
			  AS2 (ldd,%B0,Y+1)        CR_TAB
			  AS2 (ldd,%C0,Y+2)        CR_TAB
			  AS2 (ldd,%D0,Y+3)        CR_TAB
			  AS2 (subi,r28,lo8(%o1))  CR_TAB
			  AS2 (sbci,r29,hi8(%o1)));
	}

      reg_base = true_regnum (XEXP (base, 0));
      if (reg_base == REG_X)
	{
	  /* R = (X + d) */
	  if (reg_dest == REG_X)
	    {
	      *l = 7;
	      /* "ld r26,-X" is undefined */
	      return (AS2 (adiw,r26,%o1+3)    CR_TAB
		      AS2 (ld,r29,X)          CR_TAB
		      AS2 (ld,r28,-X)         CR_TAB
		      AS2 (ld,__tmp_reg__,-X) CR_TAB
		      AS2 (sbiw,r26,1)        CR_TAB
		      AS2 (ld,r26,X)          CR_TAB
		      AS2 (mov,r27,__tmp_reg__));
	    }
	  *l = 6;
	  if (reg_dest == REG_X - 2)
	    return (AS2 (adiw,r26,%o1)      CR_TAB
		    AS2 (ld,r24,X+)         CR_TAB
		    AS2 (ld,r25,X+)         CR_TAB
		    AS2 (ld,__tmp_reg__,X+) CR_TAB
		    AS2 (ld,r27,X)          CR_TAB
		    AS2 (mov,r26,__tmp_reg__));

	  return (AS2 (adiw,r26,%o1) CR_TAB
		  AS2 (ld,%A0,X+)    CR_TAB
		  AS2 (ld,%B0,X+)    CR_TAB
		  AS2 (ld,%C0,X+)    CR_TAB
		  AS2 (ld,%D0,X)     CR_TAB
		  AS2 (sbiw,r26,%o1+3));
	}
      if (reg_dest == reg_base)
        return *l=5, (AS2 (ldd,%D0,%D1) CR_TAB
                      AS2 (ldd,%C0,%C1) CR_TAB
                      AS2 (ldd,__tmp_reg__,%B1)  CR_TAB
                      AS2 (ldd,%A0,%A1) CR_TAB
                      AS2 (mov,%B0,__tmp_reg__));
      else if (reg_dest == reg_base - 2)
        return *l=5, (AS2 (ldd,%A0,%A1) CR_TAB
                      AS2 (ldd,%B0,%B1) CR_TAB
                      AS2 (ldd,__tmp_reg__,%C1)  CR_TAB
                      AS2 (ldd,%D0,%D1) CR_TAB
                      AS2 (mov,%C0,__tmp_reg__));
      return *l=4, (AS2 (ldd,%A0,%A1) CR_TAB
                    AS2 (ldd,%B0,%B1) CR_TAB
                    AS2 (ldd,%C0,%C1) CR_TAB
                    AS2 (ldd,%D0,%D1));
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=4, (AS2 (ld,%D0,%1) CR_TAB
		  AS2 (ld,%C0,%1) CR_TAB
		  AS2 (ld,%B0,%1) CR_TAB
		  AS2 (ld,%A0,%1));
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return *l=4, (AS2 (ld,%A0,%1) CR_TAB
		  AS2 (ld,%B0,%1) CR_TAB
		  AS2 (ld,%C0,%1) CR_TAB
		  AS2 (ld,%D0,%1));
  else if (CONSTANT_ADDRESS_P (base))
      return *l=8, (AS2 (lds,%A0,%m1) CR_TAB
		    AS2 (lds,%B0,%m1+1) CR_TAB
		    AS2 (lds,%C0,%m1+2) CR_TAB
		    AS2 (lds,%D0,%m1+3));
    
  fatal_insn ("unknown move insn:",insn);
  return "";
}

const char *
out_movsi_mr_r (rtx insn, rtx op[], int *l)
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
    return *l=8,(AS2 (sts,%m0,%A1) CR_TAB
		 AS2 (sts,%m0+1,%B1) CR_TAB
		 AS2 (sts,%m0+2,%C1) CR_TAB
		 AS2 (sts,%m0+3,%D1));
  if (reg_base > 0)                 /* (r) */
    {
      if (reg_base == REG_X)                /* (R26) */
        {
          if (reg_src == REG_X)
            {
	      /* "st X+,r26" is undefined */
              if (reg_unused_after (insn, base))
		return *l=6, (AS2 (mov,__tmp_reg__,r27) CR_TAB
			      AS2 (st,X,r26)            CR_TAB
			      AS2 (adiw,r26,1)          CR_TAB
			      AS2 (st,X+,__tmp_reg__)   CR_TAB
			      AS2 (st,X+,r28)           CR_TAB
			      AS2 (st,X,r29));
              else
                return *l=7, (AS2 (mov,__tmp_reg__,r27) CR_TAB
			      AS2 (st,X,r26)            CR_TAB
			      AS2 (adiw,r26,1)          CR_TAB
			      AS2 (st,X+,__tmp_reg__)   CR_TAB
			      AS2 (st,X+,r28)           CR_TAB
			      AS2 (st,X,r29)            CR_TAB
			      AS2 (sbiw,r26,3));
            }
          else if (reg_base == reg_src + 2)
            {
              if (reg_unused_after (insn, base))
                return *l=7, (AS2 (mov,__zero_reg__,%C1) CR_TAB
                              AS2 (mov,__tmp_reg__,%D1) CR_TAB
                              AS2 (st,%0+,%A1) CR_TAB
                              AS2 (st,%0+,%B1) CR_TAB
                              AS2 (st,%0+,__zero_reg__)  CR_TAB
                              AS2 (st,%0,__tmp_reg__)   CR_TAB
                              AS1 (clr,__zero_reg__));
              else
                return *l=8, (AS2 (mov,__zero_reg__,%C1) CR_TAB
                              AS2 (mov,__tmp_reg__,%D1) CR_TAB
                              AS2 (st,%0+,%A1) CR_TAB
                              AS2 (st,%0+,%B1) CR_TAB
                              AS2 (st,%0+,__zero_reg__)  CR_TAB
                              AS2 (st,%0,__tmp_reg__)   CR_TAB
                              AS1 (clr,__zero_reg__)     CR_TAB
                              AS2 (sbiw,r26,3));
            }
          return *l=5, (AS2 (st,%0+,%A1)  CR_TAB
                        AS2 (st,%0+,%B1) CR_TAB
                        AS2 (st,%0+,%C1) CR_TAB
                        AS2 (st,%0,%D1)  CR_TAB
                        AS2 (sbiw,r26,3));
        }
      else
        return *l=4, (AS2 (st,%0,%A1)    CR_TAB
		      AS2 (std,%0+1,%B1) CR_TAB
		      AS2 (std,%0+2,%C1) CR_TAB
		      AS2 (std,%0+3,%D1));
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));
      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
	{
	  if (reg_base != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return *l = 6, (AS2 (adiw,r28,%o0-60) CR_TAB
			    AS2 (std,Y+60,%A1)    CR_TAB
			    AS2 (std,Y+61,%B1)    CR_TAB
			    AS2 (std,Y+62,%C1)    CR_TAB
			    AS2 (std,Y+63,%D1)    CR_TAB
			    AS2 (sbiw,r28,%o0-60));

	  return *l = 8, (AS2 (subi,r28,lo8(-%o0)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o0)) CR_TAB
			  AS2 (st,Y,%A1)           CR_TAB
			  AS2 (std,Y+1,%B1)        CR_TAB
			  AS2 (std,Y+2,%C1)        CR_TAB
			  AS2 (std,Y+3,%D1)        CR_TAB
			  AS2 (subi,r28,lo8(%o0))  CR_TAB
			  AS2 (sbci,r29,hi8(%o0)));
	}
      if (reg_base == REG_X)
	{
	  /* (X + d) = R */
	  if (reg_src == REG_X)
	    {
	      *l = 9;
	      return (AS2 (mov,__tmp_reg__,r26)  CR_TAB
		      AS2 (mov,__zero_reg__,r27) CR_TAB
		      AS2 (adiw,r26,%o0)         CR_TAB
		      AS2 (st,X+,__tmp_reg__)    CR_TAB
		      AS2 (st,X+,__zero_reg__)   CR_TAB
		      AS2 (st,X+,r28)            CR_TAB
		      AS2 (st,X,r29)             CR_TAB
		      AS1 (clr,__zero_reg__)     CR_TAB
		      AS2 (sbiw,r26,%o0+3));
	    }
	  else if (reg_src == REG_X - 2)
	    {
	      *l = 9;
	      return (AS2 (mov,__tmp_reg__,r26)  CR_TAB
		      AS2 (mov,__zero_reg__,r27) CR_TAB
		      AS2 (adiw,r26,%o0)         CR_TAB
		      AS2 (st,X+,r24)            CR_TAB
		      AS2 (st,X+,r25)            CR_TAB
		      AS2 (st,X+,__tmp_reg__)    CR_TAB
		      AS2 (st,X,__zero_reg__)    CR_TAB
		      AS1 (clr,__zero_reg__)     CR_TAB
		      AS2 (sbiw,r26,%o0+3));
	    }
	  *l = 6;
	  return (AS2 (adiw,r26,%o0) CR_TAB
		  AS2 (st,X+,%A1)    CR_TAB
		  AS2 (st,X+,%B1)    CR_TAB
		  AS2 (st,X+,%C1)    CR_TAB
		  AS2 (st,X,%D1)     CR_TAB
		  AS2 (sbiw,r26,%o0+3));
	}
      return *l=4, (AS2 (std,%A0,%A1)    CR_TAB
		    AS2 (std,%B0,%B1) CR_TAB
		    AS2 (std,%C0,%C1) CR_TAB
		    AS2 (std,%D0,%D1));
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=4, (AS2 (st,%0,%D1) CR_TAB
		  AS2 (st,%0,%C1) CR_TAB
		  AS2 (st,%0,%B1) CR_TAB
		  AS2 (st,%0,%A1));
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return *l=4, (AS2 (st,%0,%A1)  CR_TAB
		  AS2 (st,%0,%B1) CR_TAB
		  AS2 (st,%0,%C1) CR_TAB
		  AS2 (st,%0,%D1));
  fatal_insn ("unknown move insn:",insn);
  return "";
}

const char *
output_movsisf (rtx insn, rtx operands[], rtx clobber_reg, int *l)
{
  int dummy;
  rtx dest = operands[0];
  rtx src = operands[1];
  int *real_l = l;
  
  if (!l)
    l = &dummy;
  
  if (register_operand (dest, VOIDmode))
    {
      if (register_operand (src, VOIDmode)) /* mov r,r */
	{
	  if (true_regnum (dest) > true_regnum (src))
	    {
	      if (AVR_HAVE_MOVW)
		{
		  *l = 2;
		  return (AS2 (movw,%C0,%C1) CR_TAB
			  AS2 (movw,%A0,%A1));
		}
	      *l = 4;
	      return (AS2 (mov,%D0,%D1) CR_TAB
		      AS2 (mov,%C0,%C1) CR_TAB
		      AS2 (mov,%B0,%B1) CR_TAB
		      AS2 (mov,%A0,%A1));
	    }
	  else
	    {
	      if (AVR_HAVE_MOVW)
		{
		  *l = 2;
		  return (AS2 (movw,%A0,%A1) CR_TAB
			  AS2 (movw,%C0,%C1));
		}
	      *l = 4;
	      return (AS2 (mov,%A0,%A1) CR_TAB
		      AS2 (mov,%B0,%B1) CR_TAB
		      AS2 (mov,%C0,%C1) CR_TAB
		      AS2 (mov,%D0,%D1));
	    }
	}
      else if (CONST_INT_P (src)
               || CONST_DOUBLE_P (src))
        {
          return output_reload_insisf (insn, operands, clobber_reg, real_l);
        }
      else if (CONSTANT_P (src))
	{
	  if (test_hard_reg_class (LD_REGS, dest)) /* ldi d,i */
	    {
	      *l = 4;
	      return (AS2 (ldi,%A0,lo8(%1))  CR_TAB
		      AS2 (ldi,%B0,hi8(%1))  CR_TAB
		      AS2 (ldi,%C0,hlo8(%1)) CR_TAB
		      AS2 (ldi,%D0,hhi8(%1)));
	    }
	  /* Last resort, better than loading from memory.  */
	  *l = 10;
	  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
		  AS2 (ldi,r31,lo8(%1))     CR_TAB
		  AS2 (mov,%A0,r31)         CR_TAB
		  AS2 (ldi,r31,hi8(%1))     CR_TAB
		  AS2 (mov,%B0,r31)         CR_TAB
		  AS2 (ldi,r31,hlo8(%1))    CR_TAB
		  AS2 (mov,%C0,r31)         CR_TAB
		  AS2 (ldi,r31,hhi8(%1))    CR_TAB
		  AS2 (mov,%D0,r31)         CR_TAB
		  AS2 (mov,r31,__tmp_reg__));
	}
      else if (GET_CODE (src) == MEM)
	return out_movsi_r_mr (insn, operands, real_l); /* mov r,m */
    }
  else if (GET_CODE (dest) == MEM)
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

const char *
out_movqi_mr_r (rtx insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (dest, 0);
  int dummy;

  if (!l)
    l = &dummy;
  
  if (CONSTANT_ADDRESS_P (x))
    {
      if (CONST_INT_P (x) && INTVAL (x) == SREG_ADDR)
	{
	  *l = 1;
	  return AS2 (out,__SREG__,%1);
	}
      if (optimize > 0 && io_address_operand (x, QImode))
	{
	  *l = 1;
	  return AS2 (out,%m0-0x20,%1);
	}
      *l = 2;
      return AS2 (sts,%m0,%1);
    }
  /* memory access by reg+disp */
  else if (GET_CODE (x) == PLUS	
      && REG_P (XEXP (x,0))
      && GET_CODE (XEXP (x,1)) == CONST_INT)
    {
      if ((INTVAL (XEXP (x,1)) - GET_MODE_SIZE (GET_MODE (dest))) >= 63)
	{
	  int disp = INTVAL (XEXP (x,1));
	  if (REGNO (XEXP (x,0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return *l = 3, (AS2 (adiw,r28,%o0-63) CR_TAB
			    AS2 (std,Y+63,%1)     CR_TAB
			    AS2 (sbiw,r28,%o0-63));

	  return *l = 5, (AS2 (subi,r28,lo8(-%o0)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o0)) CR_TAB
			  AS2 (st,Y,%1)            CR_TAB
			  AS2 (subi,r28,lo8(%o0))  CR_TAB
			  AS2 (sbci,r29,hi8(%o0)));
	}
      else if (REGNO (XEXP (x,0)) == REG_X)
	{
	  if (reg_overlap_mentioned_p (src, XEXP (x, 0)))
	    {
	      if (reg_unused_after (insn, XEXP (x,0)))
		return *l = 3, (AS2 (mov,__tmp_reg__,%1) CR_TAB
				AS2 (adiw,r26,%o0)       CR_TAB
				AS2 (st,X,__tmp_reg__));

	      return *l = 4, (AS2 (mov,__tmp_reg__,%1) CR_TAB
			      AS2 (adiw,r26,%o0)       CR_TAB
			      AS2 (st,X,__tmp_reg__)   CR_TAB
			      AS2 (sbiw,r26,%o0));
	    }
	  else
	    {
	      if (reg_unused_after (insn, XEXP (x,0)))
		return *l = 2, (AS2 (adiw,r26,%o0) CR_TAB
				AS2 (st,X,%1));

	      return *l = 3, (AS2 (adiw,r26,%o0) CR_TAB
			      AS2 (st,X,%1)      CR_TAB
			      AS2 (sbiw,r26,%o0));
	    }
	}
      *l = 1;
      return AS2 (std,%0,%1);
    }
  *l = 1;
  return AS2 (st,%0,%1);
}

const char *
out_movhi_mr_r (rtx insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);
  /* "volatile" forces writing high byte first, even if less efficient,
     for correct operation with 16-bit I/O registers.  */
  int mem_volatile_p = MEM_VOLATILE_P (dest);
  int tmp;

  if (!l)
    l = &tmp;
  if (CONSTANT_ADDRESS_P (base))
    {
      if (optimize > 0 && io_address_operand (base, HImode))
	{
	  *l = 2;
	  return (AS2 (out,%m0+1-0x20,%B1) CR_TAB
		  AS2 (out,%m0-0x20,%A1));
	}
      return *l = 4, (AS2 (sts,%m0+1,%B1) CR_TAB
		      AS2 (sts,%m0,%A1));
    }
  if (reg_base > 0)
    {
      if (reg_base == REG_X)
        {
          if (reg_src == REG_X)
            {
              /* "st X+,r26" and "st -X,r26" are undefined.  */
              if (!mem_volatile_p && reg_unused_after (insn, src))
		return *l=4, (AS2 (mov,__tmp_reg__,r27) CR_TAB
			      AS2 (st,X,r26)            CR_TAB
			      AS2 (adiw,r26,1)          CR_TAB
			      AS2 (st,X,__tmp_reg__));
              else
		return *l=5, (AS2 (mov,__tmp_reg__,r27) CR_TAB
			      AS2 (adiw,r26,1)          CR_TAB
			      AS2 (st,X,__tmp_reg__)    CR_TAB
                              AS2 (sbiw,r26,1)          CR_TAB
                              AS2 (st,X,r26));
            }
          else
            {
              if (!mem_volatile_p && reg_unused_after (insn, base))
                return *l=2, (AS2 (st,X+,%A1) CR_TAB
                              AS2 (st,X,%B1));
              else
                return *l=3, (AS2 (adiw,r26,1) CR_TAB
                              AS2 (st,X,%B1)   CR_TAB
                              AS2 (st,-X,%A1));
            }
        }
      else
        return  *l=2, (AS2 (std,%0+1,%B1) CR_TAB
                       AS2 (st,%0,%A1));
    }
  else if (GET_CODE (base) == PLUS)
    {
      int disp = INTVAL (XEXP (base, 1));
      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
	{
	  if (reg_base != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return *l = 4, (AS2 (adiw,r28,%o0-62) CR_TAB
			    AS2 (std,Y+63,%B1)    CR_TAB
			    AS2 (std,Y+62,%A1)    CR_TAB
			    AS2 (sbiw,r28,%o0-62));

	  return *l = 6, (AS2 (subi,r28,lo8(-%o0)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o0)) CR_TAB
			  AS2 (std,Y+1,%B1)        CR_TAB
			  AS2 (st,Y,%A1)           CR_TAB
			  AS2 (subi,r28,lo8(%o0))  CR_TAB
			  AS2 (sbci,r29,hi8(%o0)));
	}
      if (reg_base == REG_X)
	{
	  /* (X + d) = R */
	  if (reg_src == REG_X)
            {
	      *l = 7;
	      return (AS2 (mov,__tmp_reg__,r26)  CR_TAB
		      AS2 (mov,__zero_reg__,r27) CR_TAB
                      AS2 (adiw,r26,%o0+1)       CR_TAB
		      AS2 (st,X,__zero_reg__)    CR_TAB
		      AS2 (st,-X,__tmp_reg__)    CR_TAB
		      AS1 (clr,__zero_reg__)     CR_TAB
                      AS2 (sbiw,r26,%o0));
	    }
	  *l = 4;
          return (AS2 (adiw,r26,%o0+1) CR_TAB
                  AS2 (st,X,%B1)       CR_TAB
                  AS2 (st,-X,%A1)      CR_TAB
                  AS2 (sbiw,r26,%o0));
	}
      return *l=2, (AS2 (std,%B0,%B1)    CR_TAB
                    AS2 (std,%A0,%A1));
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=2, (AS2 (st,%0,%B1) CR_TAB
		  AS2 (st,%0,%A1));
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    {
      if (mem_volatile_p)
        {
          if (REGNO (XEXP (base, 0)) == REG_X)
            {
              *l = 4;
              return (AS2 (adiw,r26,1)  CR_TAB
                      AS2 (st,X,%B1)    CR_TAB
                      AS2 (st,-X,%A1)   CR_TAB
                      AS2 (adiw,r26,2));
            }
          else
            {
              *l = 3;
              return (AS2 (std,%p0+1,%B1) CR_TAB
                      AS2 (st,%p0,%A1)    CR_TAB
                      AS2 (adiw,%r0,2));
            }
        }

      *l = 2;
      return (AS2 (st,%0,%A1)  CR_TAB
            AS2 (st,%0,%B1));
    }
  fatal_insn ("unknown move insn:",insn);
  return "";
}

/* Return 1 if frame pointer for current function required.  */

bool
avr_frame_pointer_required_p (void)
{
  return (cfun->calls_alloca
	  || crtl->args.info.nregs == 0
  	  || get_frame_size () > 0);
}

/* Returns the condition of compare insn INSN, or UNKNOWN.  */

static RTX_CODE
compare_condition (rtx insn)
{
  rtx next = next_real_insn (insn);

  if (next && JUMP_P (next))
    {
      rtx pat = PATTERN (next);
      rtx src = SET_SRC (pat);
      
      if (IF_THEN_ELSE == GET_CODE (src))
        return GET_CODE (XEXP (src, 0));
    }
  
  return UNKNOWN;
}

/* Returns nonzero if INSN is a tst insn that only tests the sign.  */

static int
compare_sign_p (rtx insn)
{
  RTX_CODE cond = compare_condition (insn);
  return (cond == GE || cond == LT);
}

/* Returns nonzero if the next insn is a JUMP_INSN with a condition
   that needs to be swapped (GT, GTU, LE, LEU).  */

int
compare_diff_p (rtx insn)
{
  RTX_CODE cond = compare_condition (insn);
  return (cond == GT || cond == GTU || cond == LE || cond == LEU) ? cond : 0;
}

/* Returns nonzero if INSN is a compare insn with the EQ or NE condition.  */

int
compare_eq_p (rtx insn)
{
  RTX_CODE cond = compare_condition (insn);
  return (cond == EQ || cond == NE);
}


/* Output test instruction for HImode.  */

const char *
out_tsthi (rtx insn, rtx op, int *l)
{
  if (compare_sign_p (insn))
    {
      if (l) *l = 1;
      return AS1 (tst,%B0);
    }
  if (reg_unused_after (insn, op)
      && compare_eq_p (insn))
    {
      /* Faster than sbiw if we can clobber the operand.  */
      if (l) *l = 1;
      return "or %A0,%B0";
    }
  if (test_hard_reg_class (ADDW_REGS, op))
    {
      if (l) *l = 1;
      return AS2 (sbiw,%0,0);
    }
  if (l) *l = 2;
  return (AS2 (cp,%A0,__zero_reg__) CR_TAB
          AS2 (cpc,%B0,__zero_reg__));
}


/* Output test instruction for SImode.  */

const char *
out_tstsi (rtx insn, rtx op, int *l)
{
  if (compare_sign_p (insn))
    {
      if (l) *l = 1;
      return AS1 (tst,%D0);
    }
  if (test_hard_reg_class (ADDW_REGS, op))
    {
      if (l) *l = 3;
      return (AS2 (sbiw,%A0,0) CR_TAB
              AS2 (cpc,%C0,__zero_reg__) CR_TAB
              AS2 (cpc,%D0,__zero_reg__));
    }
  if (l) *l = 4;
  return (AS2 (cp,%A0,__zero_reg__) CR_TAB
          AS2 (cpc,%B0,__zero_reg__) CR_TAB
          AS2 (cpc,%C0,__zero_reg__) CR_TAB
          AS2 (cpc,%D0,__zero_reg__));
}


/* Generate asm equivalent for various shifts.
   Shift count is a CONST_INT, MEM or REG.
   This only handles cases that are not already
   carefully hand-optimized in ?sh??i3_out.  */

void
out_shift_with_cnt (const char *templ, rtx insn, rtx operands[],
		    int *len, int t_len)
{
  rtx op[10];
  char str[500];
  int second_label = 1;
  int saved_in_tmp = 0;
  int use_zero_reg = 0;

  op[0] = operands[0];
  op[1] = operands[1];
  op[2] = operands[2];
  op[3] = operands[3];
  str[0] = 0;

  if (len)
    *len = 1;

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL);
      int count = INTVAL (operands[2]);
      int max_len = 10;  /* If larger than this, always use a loop.  */

      if (count <= 0)
	{
	  if (len)
	    *len = 0;
	  return;
	}

      if (count < 8 && !scratch)
	use_zero_reg = 1;

      if (optimize_size)
	max_len = t_len + (scratch ? 3 : (use_zero_reg ? 4 : 5));

      if (t_len * count <= max_len)
	{
	  /* Output shifts inline with no loop - faster.  */
	  if (len)
	    *len = t_len * count;
	  else
	    {
	      while (count-- > 0)
		output_asm_insn (templ, op);
	    }

	  return;
	}

      if (scratch)
	{
	  if (!len)
	    strcat (str, AS2 (ldi,%3,%2));
	}
      else if (use_zero_reg)
	{
	  /* Hack to save one word: use __zero_reg__ as loop counter.
	     Set one bit, then shift in a loop until it is 0 again.  */

	  op[3] = zero_reg_rtx;
	  if (len)
	    *len = 2;
	  else
	    strcat (str, ("set" CR_TAB
			  AS2 (bld,%3,%2-1)));
	}
      else
	{
	  /* No scratch register available, use one from LD_REGS (saved in
	     __tmp_reg__) that doesn't overlap with registers to shift.  */

	  op[3] = gen_rtx_REG (QImode,
			   ((true_regnum (operands[0]) - 1) & 15) + 16);
	  op[4] = tmp_reg_rtx;
	  saved_in_tmp = 1;

	  if (len)
	    *len = 3;  /* Includes "mov %3,%4" after the loop.  */
	  else
	    strcat (str, (AS2 (mov,%4,%3) CR_TAB
			  AS2 (ldi,%3,%2)));
	}

      second_label = 0;
    }
  else if (GET_CODE (operands[2]) == MEM)
    {
      rtx op_mov[10];
      
      op[3] = op_mov[0] = tmp_reg_rtx;
      op_mov[1] = op[2];

      if (len)
	out_movqi_r_mr (insn, op_mov, len);
      else
	output_asm_insn (out_movqi_r_mr (insn, op_mov, NULL), op_mov);
    }
  else if (register_operand (operands[2], QImode))
    {
      if (reg_unused_after (insn, operands[2])
          && !reg_overlap_mentioned_p (operands[0], operands[2]))
        {
          op[3] = op[2];
        }
      else
	{
	  op[3] = tmp_reg_rtx;
	  if (!len)
	    strcat (str, (AS2 (mov,%3,%2) CR_TAB));
	}
    }
  else
    fatal_insn ("bad shift insn:", insn);

  if (second_label)
    {
      if (len)
	++*len;
      else
	strcat (str, AS1 (rjmp,2f));
    }

  if (len)
    *len += t_len + 2;  /* template + dec + brXX */
  else
    {
      strcat (str, "\n1:\t");
      strcat (str, templ);
      strcat (str, second_label ? "\n2:\t" : "\n\t");
      strcat (str, use_zero_reg ? AS1 (lsr,%3) : AS1 (dec,%3));
      strcat (str, CR_TAB);
      strcat (str, second_label ? AS1 (brpl,1b) : AS1 (brne,1b));
      if (saved_in_tmp)
	strcat (str, (CR_TAB AS2 (mov,%3,%4)));
      output_asm_insn (str, op);
    }
}


/* 8bit shift left ((char)x << i)   */

const char *
ashlqi3_out (rtx insn, rtx operands[], int *len)
{
  if (GET_CODE (operands[2]) == CONST_INT)
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
	  return AS1 (clr,%0);
	  
	case 1:
	  *len = 1;
	  return AS1 (lsl,%0);
	  
	case 2:
	  *len = 2;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 3:
	  *len = 3;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 4:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 2;
	      return (AS1 (swap,%0) CR_TAB
		      AS2 (andi,%0,0xf0));
	    }
	  *len = 4;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 5:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 3;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsl,%0)  CR_TAB
		      AS2 (andi,%0,0xe0));
	    }
	  *len = 5;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 6:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 4;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsl,%0)  CR_TAB
		      AS1 (lsl,%0)  CR_TAB
		      AS2 (andi,%0,0xc0));
	    }
	  *len = 6;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 7:
	  *len = 3;
	  return (AS1 (ror,%0) CR_TAB
		  AS1 (clr,%0) CR_TAB
		  AS1 (ror,%0));
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt (AS1 (lsl,%0),
		      insn, operands, len, 1);
  return "";
}


/* 16bit shift left ((short)x << i)   */

const char *
ashlhi3_out (rtx insn, rtx operands[], int *len)
{
  if (GET_CODE (operands[2]) == CONST_INT)
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
	  return (AS1 (clr,%B0) CR_TAB
		  AS1 (clr,%A0));

	case 4:
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    {
	      *len = 6;
	      return (AS1 (swap,%A0)      CR_TAB
		      AS1 (swap,%B0)      CR_TAB
		      AS2 (andi,%B0,0xf0) CR_TAB
		      AS2 (eor,%B0,%A0)   CR_TAB
		      AS2 (andi,%A0,0xf0) CR_TAB
		      AS2 (eor,%B0,%A0));
	    }
	  if (scratch)
	    {
	      *len = 7;
	      return (AS1 (swap,%A0)    CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS2 (ldi,%3,0xf0) CR_TAB
		      "and %B0,%3"      CR_TAB
		      AS2 (eor,%B0,%A0) CR_TAB
		      "and %A0,%3"      CR_TAB
		      AS2 (eor,%B0,%A0));
	    }
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    {
	      *len = 8;
	      return (AS1 (lsl,%A0)       CR_TAB
		      AS1 (rol,%B0)       CR_TAB
		      AS1 (swap,%A0)      CR_TAB
		      AS1 (swap,%B0)      CR_TAB
		      AS2 (andi,%B0,0xf0) CR_TAB
		      AS2 (eor,%B0,%A0)   CR_TAB
		      AS2 (andi,%A0,0xf0) CR_TAB
		      AS2 (eor,%B0,%A0));
	    }
	  if (scratch)
	    {
	      *len = 9;
	      return (AS1 (lsl,%A0)     CR_TAB
		      AS1 (rol,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS2 (ldi,%3,0xf0) CR_TAB
		      "and %B0,%3"      CR_TAB
		      AS2 (eor,%B0,%A0) CR_TAB
		      "and %A0,%3"      CR_TAB
		      AS2 (eor,%B0,%A0));
	    }
	  break;  /* 10 */

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 9;
	  return (AS1 (clr,__tmp_reg__) CR_TAB
		  AS1 (lsr,%B0)         CR_TAB
		  AS1 (ror,%A0)         CR_TAB
		  AS1 (ror,__tmp_reg__) CR_TAB
		  AS1 (lsr,%B0)         CR_TAB
		  AS1 (ror,%A0)         CR_TAB
		  AS1 (ror,__tmp_reg__) CR_TAB
		  AS2 (mov,%B0,%A0)     CR_TAB
		  AS2 (mov,%A0,__tmp_reg__));

	case 7:
	  *len = 5;
	  return (AS1 (lsr,%B0)     CR_TAB
		  AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (ror,%B0)     CR_TAB
		  AS1 (ror,%A0));

	case 8:
	  return *len = 2, (AS2 (mov,%B0,%A1) CR_TAB
			    AS1 (clr,%A0));

	case 9:
	  *len = 3;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0));

	case 10:
	  *len = 4;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0));

	case 11:
	  *len = 5;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0));

	case 12:
	  if (ldi_ok)
	    {
	      *len = 4;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS2 (andi,%B0,0xf0));
	    }
	  if (scratch)
	    {
	      *len = 5;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS2 (ldi,%3,0xf0) CR_TAB
		      "and %B0,%3");
	    }
	  *len = 6;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0));

	case 13:
	  if (ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS1 (lsl,%B0)     CR_TAB
		      AS2 (andi,%B0,0xe0));
	    }
	  if (AVR_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return (AS2 (ldi,%3,0x20) CR_TAB
		      AS2 (mul,%A0,%3)  CR_TAB
		      AS2 (mov,%B0,r0)  CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (scratch)
	    {
	      *len = 6;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS1 (lsl,%B0)     CR_TAB
		      AS2 (ldi,%3,0xe0) CR_TAB
		      "and %B0,%3");
	    }
	  if (AVR_HAVE_MUL)
	    {
	      *len = 6;
	      return ("set"            CR_TAB
		      AS2 (bld,r1,5)   CR_TAB
		      AS2 (mul,%A0,r1) CR_TAB
		      AS2 (mov,%B0,r0) CR_TAB
		      AS1 (clr,%A0)    CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  *len = 7;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0));

	case 14:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%B0,0x40) CR_TAB
		      AS2 (mul,%A0,%B0)  CR_TAB
		      AS2 (mov,%B0,r0)   CR_TAB
		      AS1 (clr,%A0)      CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (AVR_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return (AS2 (ldi,%3,0x40) CR_TAB
		      AS2 (mul,%A0,%3)  CR_TAB
		      AS2 (mov,%B0,r0)  CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS2 (ldi,%A0,6) "\n1:\t"
		      AS1 (lsl,%B0)     CR_TAB
		      AS1 (dec,%A0)     CR_TAB
		      AS1 (brne,1b));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return (AS1 (clr,%B0) CR_TAB
		  AS1 (lsr,%A0) CR_TAB
		  AS1 (ror,%B0) CR_TAB
		  AS1 (lsr,%A0) CR_TAB
		  AS1 (ror,%B0) CR_TAB
		  AS1 (clr,%A0));

	case 15:
	  *len = 4;
	  return (AS1 (clr,%B0) CR_TAB
		  AS1 (lsr,%A0) CR_TAB
		  AS1 (ror,%B0) CR_TAB
		  AS1 (clr,%A0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (lsl,%A0) CR_TAB
		       AS1 (rol,%B0)),
		       insn, operands, len, 2);
  return "";
}


/* 32bit shift left ((long)x << i)   */

const char *
ashlsi3_out (rtx insn, rtx operands[], int *len)
{
  if (GET_CODE (operands[2]) == CONST_INT)
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
	    return *len = 3, (AS1 (clr,%D0) CR_TAB
			      AS1 (clr,%C0) CR_TAB
			      AS2 (movw,%A0,%C0));
	  *len = 4;
	  return (AS1 (clr,%D0) CR_TAB
		  AS1 (clr,%C0) CR_TAB
		  AS1 (clr,%B0) CR_TAB
		  AS1 (clr,%A0));

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (reg0 >= reg1)
	      return (AS2 (mov,%D0,%C1)  CR_TAB
		      AS2 (mov,%C0,%B1)  CR_TAB
		      AS2 (mov,%B0,%A1)  CR_TAB
		      AS1 (clr,%A0));
	    else
	      return (AS1 (clr,%A0)      CR_TAB
		      AS2 (mov,%B0,%A1)  CR_TAB
		      AS2 (mov,%C0,%B1)  CR_TAB
		      AS2 (mov,%D0,%C1));
	  }

	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    if (reg0 + 2 == reg1)
	      return *len = 2, (AS1 (clr,%B0)      CR_TAB
				AS1 (clr,%A0));
	    if (AVR_HAVE_MOVW)
	      return *len = 3, (AS2 (movw,%C0,%A1) CR_TAB
				AS1 (clr,%B0)      CR_TAB
				AS1 (clr,%A0));
	    else
	      return *len = 4, (AS2 (mov,%C0,%A1)  CR_TAB
				AS2 (mov,%D0,%B1)  CR_TAB
				AS1 (clr,%B0)      CR_TAB
				AS1 (clr,%A0));
	  }

	case 24:
	  *len = 4;
	  return (AS2 (mov,%D0,%A1)  CR_TAB
		  AS1 (clr,%C0)      CR_TAB
		  AS1 (clr,%B0)      CR_TAB
		  AS1 (clr,%A0));

	case 31:
	  *len = 6;
	  return (AS1 (clr,%D0) CR_TAB
		  AS1 (lsr,%A0) CR_TAB
		  AS1 (ror,%D0) CR_TAB
		  AS1 (clr,%C0) CR_TAB
		  AS1 (clr,%B0) CR_TAB
		  AS1 (clr,%A0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (lsl,%A0) CR_TAB
		       AS1 (rol,%B0) CR_TAB
		       AS1 (rol,%C0) CR_TAB
		       AS1 (rol,%D0)),
		       insn, operands, len, 4);
  return "";
}

/* 8bit arithmetic shift right  ((signed char)x >> i) */

const char *
ashrqi3_out (rtx insn, rtx operands[], int *len)
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	case 1:
	  *len = 1;
	  return AS1 (asr,%0);

	case 2:
	  *len = 2;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));

	case 3:
	  *len = 3;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));

	case 4:
	  *len = 4;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));

	case 5:
	  *len = 5;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));

	case 6:
	  *len = 4;
	  return (AS2 (bst,%0,6)  CR_TAB
		  AS1 (lsl,%0)    CR_TAB
		  AS2 (sbc,%0,%0) CR_TAB
		  AS2 (bld,%0,0));

	default:
	  if (INTVAL (operands[2]) < 8)
	    break;

	  /* fall through */

	case 7:
	  *len = 2;
	  return (AS1 (lsl,%0) CR_TAB
		  AS2 (sbc,%0,%0));
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt (AS1 (asr,%0),
		      insn, operands, len, 1);
  return "";
}


/* 16bit arithmetic shift right  ((signed short)x >> i) */

const char *
ashrhi3_out (rtx insn, rtx operands[], int *len)
{
  if (GET_CODE (operands[2]) == CONST_INT)
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
	  return (AS2 (mov,__tmp_reg__,%A0) CR_TAB
		  AS2 (mov,%A0,%B0)         CR_TAB
		  AS1 (lsl,__tmp_reg__)     CR_TAB
		  AS1 (rol,%A0)             CR_TAB
		  AS2 (sbc,%B0,%B0)         CR_TAB
		  AS1 (lsl,__tmp_reg__)     CR_TAB
		  AS1 (rol,%A0)             CR_TAB
		  AS1 (rol,%B0));

	case 7:
	  *len = 4;
	  return (AS1 (lsl,%A0)     CR_TAB
		  AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (rol,%A0)     CR_TAB
		  AS2 (sbc,%B0,%B0));

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    if (reg0 == reg1)
	      return *len = 3, (AS2 (mov,%A0,%B0) CR_TAB
				AS1 (lsl,%B0)     CR_TAB
				AS2 (sbc,%B0,%B0));
	    else 
	      return *len = 4, (AS2 (mov,%A0,%B1) CR_TAB
			        AS1 (clr,%B0)     CR_TAB
			        AS2 (sbrc,%A0,7)  CR_TAB
			        AS1 (dec,%B0));
	  }

	case 9:
	  *len = 4;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)      CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0));

	case 10:
	  *len = 5;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0));

	case 11:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%A0,0x20) CR_TAB
		      AS2 (muls,%B0,%A0) CR_TAB
		      AS2 (mov,%A0,r1)   CR_TAB
		      AS2 (sbc,%B0,%B0)  CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0));

	case 12:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%A0,0x10) CR_TAB
		      AS2 (muls,%B0,%A0) CR_TAB
		      AS2 (mov,%A0,r1)   CR_TAB
		      AS2 (sbc,%B0,%B0)  CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 7;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0));

	case 13:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%A0,0x08) CR_TAB
		      AS2 (muls,%B0,%A0) CR_TAB
		      AS2 (mov,%A0,r1)   CR_TAB
		      AS2 (sbc,%B0,%B0)  CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size)
	    break;  /* scratch ? 5 : 7 */
	  *len = 8;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0));

	case 14:
	  *len = 5;
	  return (AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%A0,%A0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (rol,%A0));

	default:
	  if (INTVAL (operands[2]) < 16)
	    break;

	  /* fall through */

	case 15:
	  return *len = 3, (AS1 (lsl,%B0)     CR_TAB
			    AS2 (sbc,%A0,%A0) CR_TAB
			    AS2 (mov,%B0,%A0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (asr,%B0) CR_TAB
		       AS1 (ror,%A0)),
		       insn, operands, len, 2);
  return "";
}


/* 32bit arithmetic shift right  ((signed long)x >> i) */

const char *
ashrsi3_out (rtx insn, rtx operands[], int *len)
{
  if (GET_CODE (operands[2]) == CONST_INT)
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
	      return (AS2 (mov,%A0,%B1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%C0,7)  CR_TAB
		      AS1 (dec,%D0));
	    else
	      return (AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%D1,7)  CR_TAB
		      AS1 (dec,%D0)     CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%A0,%B1));
	  }
	  
	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    
	    if (reg0 == reg1 + 2)
	      return *len = 4, (AS1 (clr,%D0)     CR_TAB
				AS2 (sbrc,%B0,7)  CR_TAB
				AS1 (com,%D0)     CR_TAB
				AS2 (mov,%C0,%D0));
	    if (AVR_HAVE_MOVW)
	      return *len = 5, (AS2 (movw,%A0,%C1) CR_TAB
				AS1 (clr,%D0)      CR_TAB
				AS2 (sbrc,%B0,7)   CR_TAB
				AS1 (com,%D0)      CR_TAB
				AS2 (mov,%C0,%D0));
	    else 
	      return *len = 6, (AS2 (mov,%B0,%D1) CR_TAB
				AS2 (mov,%A0,%C1) CR_TAB
				AS1 (clr,%D0)     CR_TAB
				AS2 (sbrc,%B0,7)  CR_TAB
				AS1 (com,%D0)     CR_TAB
				AS2 (mov,%C0,%D0));
	  }

	case 24:
	  return *len = 6, (AS2 (mov,%A0,%D1) CR_TAB
			    AS1 (clr,%D0)     CR_TAB
			    AS2 (sbrc,%A0,7)  CR_TAB
			    AS1 (com,%D0)     CR_TAB
			    AS2 (mov,%B0,%D0) CR_TAB
			    AS2 (mov,%C0,%D0));

	default:
	  if (INTVAL (operands[2]) < 32)
	    break;

	  /* fall through */

	case 31:
	  if (AVR_HAVE_MOVW)
	    return *len = 4, (AS1 (lsl,%D0)     CR_TAB
			      AS2 (sbc,%A0,%A0) CR_TAB
			      AS2 (mov,%B0,%A0) CR_TAB
			      AS2 (movw,%C0,%A0));
	  else
	    return *len = 5, (AS1 (lsl,%D0)     CR_TAB
			      AS2 (sbc,%A0,%A0) CR_TAB
			      AS2 (mov,%B0,%A0) CR_TAB
			      AS2 (mov,%C0,%A0) CR_TAB
			      AS2 (mov,%D0,%A0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (asr,%D0) CR_TAB
		       AS1 (ror,%C0) CR_TAB
		       AS1 (ror,%B0) CR_TAB
		       AS1 (ror,%A0)),
		       insn, operands, len, 4);
  return "";
}

/* 8bit logic shift right ((unsigned char)x >> i) */

const char *
lshrqi3_out (rtx insn, rtx operands[], int *len)
{
  if (GET_CODE (operands[2]) == CONST_INT)
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
	  return AS1 (clr,%0);

	case 1:
	  *len = 1;
	  return AS1 (lsr,%0);

	case 2:
	  *len = 2;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	case 3:
	  *len = 3;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	  
	case 4:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len=2;
	      return (AS1 (swap,%0) CR_TAB
		      AS2 (andi,%0,0x0f));
	    }
	  *len = 4;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	  
	case 5:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 3;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsr,%0)  CR_TAB
		      AS2 (andi,%0,0x7));
	    }
	  *len = 5;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	  
	case 6:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 4;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsr,%0)  CR_TAB
		      AS1 (lsr,%0)  CR_TAB
		      AS2 (andi,%0,0x3));
	    }
	  *len = 6;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	  
	case 7:
	  *len = 3;
	  return (AS1 (rol,%0) CR_TAB
		  AS1 (clr,%0) CR_TAB
		  AS1 (rol,%0));
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);
  
  out_shift_with_cnt (AS1 (lsr,%0),
		      insn, operands, len, 1);
  return "";
}

/* 16bit logic shift right ((unsigned short)x >> i) */

const char *
lshrhi3_out (rtx insn, rtx operands[], int *len)
{
  if (GET_CODE (operands[2]) == CONST_INT)
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
	  return (AS1 (clr,%B0) CR_TAB
		  AS1 (clr,%A0));

	case 4:
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    {
	      *len = 6;
	      return (AS1 (swap,%B0)      CR_TAB
		      AS1 (swap,%A0)      CR_TAB
		      AS2 (andi,%A0,0x0f) CR_TAB
		      AS2 (eor,%A0,%B0)   CR_TAB
		      AS2 (andi,%B0,0x0f) CR_TAB
		      AS2 (eor,%A0,%B0));
	    }
	  if (scratch)
	    {
	      *len = 7;
	      return (AS1 (swap,%B0)    CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS2 (ldi,%3,0x0f) CR_TAB
		      "and %A0,%3"      CR_TAB
		      AS2 (eor,%A0,%B0) CR_TAB
		      "and %B0,%3"      CR_TAB
		      AS2 (eor,%A0,%B0));
	    }
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    {
	      *len = 8;
	      return (AS1 (lsr,%B0)       CR_TAB
		      AS1 (ror,%A0)       CR_TAB
		      AS1 (swap,%B0)      CR_TAB
		      AS1 (swap,%A0)      CR_TAB
		      AS2 (andi,%A0,0x0f) CR_TAB
		      AS2 (eor,%A0,%B0)   CR_TAB
		      AS2 (andi,%B0,0x0f) CR_TAB
		      AS2 (eor,%A0,%B0));
	    }
	  if (scratch)
	    {
	      *len = 9;
	      return (AS1 (lsr,%B0)     CR_TAB
		      AS1 (ror,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS2 (ldi,%3,0x0f) CR_TAB
		      "and %A0,%3"      CR_TAB
		      AS2 (eor,%A0,%B0) CR_TAB
		      "and %B0,%3"      CR_TAB
		      AS2 (eor,%A0,%B0));
	    }
	  break;  /* 10 */

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 9;
	  return (AS1 (clr,__tmp_reg__) CR_TAB
		  AS1 (lsl,%A0)         CR_TAB
		  AS1 (rol,%B0)         CR_TAB
		  AS1 (rol,__tmp_reg__) CR_TAB
		  AS1 (lsl,%A0)         CR_TAB
		  AS1 (rol,%B0)         CR_TAB
		  AS1 (rol,__tmp_reg__) CR_TAB
		  AS2 (mov,%A0,%B0)     CR_TAB
		  AS2 (mov,%B0,__tmp_reg__));

	case 7:
	  *len = 5;
	  return (AS1 (lsl,%A0)     CR_TAB
		  AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (rol,%A0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (neg,%B0));

	case 8:
	  return *len = 2, (AS2 (mov,%A0,%B1) CR_TAB
			    AS1 (clr,%B0));

	case 9:
	  *len = 3;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0));

	case 10:
	  *len = 4;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0));

	case 11:
	  *len = 5;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0));

	case 12:
	  if (ldi_ok)
	    {
	      *len = 4;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS2 (andi,%A0,0x0f));
	    }
	  if (scratch)
	    {
	      *len = 5;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS2 (ldi,%3,0x0f) CR_TAB
		      "and %A0,%3");
	    }
	  *len = 6;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0));

	case 13:
	  if (ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS1 (lsr,%A0)     CR_TAB
		      AS2 (andi,%A0,0x07));
	    }
	  if (AVR_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return (AS2 (ldi,%3,0x08) CR_TAB
		      AS2 (mul,%B0,%3)  CR_TAB
		      AS2 (mov,%A0,r1)  CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (scratch)
	    {
	      *len = 6;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS1 (lsr,%A0)     CR_TAB
		      AS2 (ldi,%3,0x07) CR_TAB
		      "and %A0,%3");
	    }
	  if (AVR_HAVE_MUL)
	    {
	      *len = 6;
	      return ("set"            CR_TAB
		      AS2 (bld,r1,3)   CR_TAB
		      AS2 (mul,%B0,r1) CR_TAB
		      AS2 (mov,%A0,r1) CR_TAB
		      AS1 (clr,%B0)    CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  *len = 7;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0));

	case 14:
	  if (AVR_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%A0,0x04) CR_TAB
		      AS2 (mul,%B0,%A0)  CR_TAB
		      AS2 (mov,%A0,r1)   CR_TAB
		      AS1 (clr,%B0)      CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (AVR_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return (AS2 (ldi,%3,0x04) CR_TAB
		      AS2 (mul,%B0,%3)  CR_TAB
		      AS2 (mov,%A0,r1)  CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS2 (ldi,%B0,6) "\n1:\t"
		      AS1 (lsr,%A0)     CR_TAB
		      AS1 (dec,%B0)     CR_TAB
		      AS1 (brne,1b));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return (AS1 (clr,%A0) CR_TAB
		  AS1 (lsl,%B0) CR_TAB
		  AS1 (rol,%A0) CR_TAB
		  AS1 (lsl,%B0) CR_TAB
		  AS1 (rol,%A0) CR_TAB
		  AS1 (clr,%B0));

	case 15:
	  *len = 4;
	  return (AS1 (clr,%A0) CR_TAB
		  AS1 (lsl,%B0) CR_TAB
		  AS1 (rol,%A0) CR_TAB
		  AS1 (clr,%B0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (lsr,%B0) CR_TAB
		       AS1 (ror,%A0)),
		       insn, operands, len, 2);
  return "";
}

/* 32bit logic shift right ((unsigned int)x >> i) */

const char *
lshrsi3_out (rtx insn, rtx operands[], int *len)
{
  if (GET_CODE (operands[2]) == CONST_INT)
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
	    return *len = 3, (AS1 (clr,%D0) CR_TAB
			      AS1 (clr,%C0) CR_TAB
			      AS2 (movw,%A0,%C0));
	  *len = 4;
	  return (AS1 (clr,%D0) CR_TAB
		  AS1 (clr,%C0) CR_TAB
		  AS1 (clr,%B0) CR_TAB
		  AS1 (clr,%A0));

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (reg0 <= reg1)
	      return (AS2 (mov,%A0,%B1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS1 (clr,%D0));
	    else
	      return (AS1 (clr,%D0)     CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%A0,%B1)); 
	  }
	  
	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    if (reg0 == reg1 + 2)
	      return *len = 2, (AS1 (clr,%C0)     CR_TAB
				AS1 (clr,%D0));
	    if (AVR_HAVE_MOVW)
	      return *len = 3, (AS2 (movw,%A0,%C1) CR_TAB
				AS1 (clr,%C0)      CR_TAB
				AS1 (clr,%D0));
	    else
	      return *len = 4, (AS2 (mov,%B0,%D1) CR_TAB
				AS2 (mov,%A0,%C1) CR_TAB
				AS1 (clr,%C0)     CR_TAB
				AS1 (clr,%D0));
	  }
	  
	case 24:
	  return *len = 4, (AS2 (mov,%A0,%D1) CR_TAB
			    AS1 (clr,%B0)     CR_TAB
			    AS1 (clr,%C0)     CR_TAB
			    AS1 (clr,%D0));

	case 31:
	  *len = 6;
	  return (AS1 (clr,%A0)    CR_TAB
		  AS2 (sbrc,%D0,7) CR_TAB
		  AS1 (inc,%A0)    CR_TAB
		  AS1 (clr,%B0)    CR_TAB
		  AS1 (clr,%C0)    CR_TAB
		  AS1 (clr,%D0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (lsr,%D0) CR_TAB
		       AS1 (ror,%C0) CR_TAB
		       AS1 (ror,%B0) CR_TAB
		       AS1 (ror,%A0)),
		      insn, operands, len, 4);
  return "";
}

/* Create RTL split patterns for byte sized rotate expressions.  This
  produces a series of move instructions and considers overlap situations.
  Overlapping non-HImode operands need a scratch register.  */

bool
avr_rotate_bytes (rtx operands[])
{
    int i, j;
    enum machine_mode mode = GET_MODE (operands[0]);
    bool overlapped = reg_overlap_mentioned_p (operands[0], operands[1]);
    bool same_reg = rtx_equal_p (operands[0], operands[1]);
    int num = INTVAL (operands[2]);
    rtx scratch = operands[3];
    /* Work out if byte or word move is needed.  Odd byte rotates need QImode.
       Word move if no scratch is needed, otherwise use size of scratch.  */
    enum machine_mode move_mode = QImode;
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
	for (i = 0; i < size; i++)
	  {
	    int from = i;
	    int to = (from + offset) % size;          
	    move[i].src = simplify_gen_subreg (move_mode, operands[1],
						mode, from * move_size);
	    move[i].dst = simplify_gen_subreg (move_mode, operands[0],
						mode, to   * move_size);
	    move[i].links = -1;
	   }
	/* Mark dependence where a dst of one move is the src of another move.
	   The first move is a conflict as it must wait until second is
	   performed.  We ignore moves to self - we catch this later.  */
	if (overlapped)
	  for (i = 0; i < size; i++)
	    if (reg_overlap_mentioned_p (move[i].dst, operands[1]))
	      for (j = 0; j < size; j++)
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
	    for (i = 0; i < size; i++)
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
		/* Make dependent on scratch move occuring.  */
		move[blocked].links = size; 
		size=size+1;
	      }
	  }
	while (blocked != -1);
      }
    return true;
}

/* Modifies the length assigned to instruction INSN
 LEN is the initially computed length of the insn.  */

int
adjust_insn_length (rtx insn, int len)
{
  rtx patt = PATTERN (insn);
  rtx set;

  if (GET_CODE (patt) == SET)
    {
      rtx op[10];
      op[1] = SET_SRC (patt);
      op[0] = SET_DEST (patt);
      if (general_operand (op[1], VOIDmode)
	  && general_operand (op[0], VOIDmode))
	{
	  switch (GET_MODE (op[0]))
	    {
	    case QImode:
	      output_movqi (insn, op, &len);
	      break;
	    case HImode:
	      output_movhi (insn, op, &len);
	      break;
	    case SImode:
	    case SFmode:
	      output_movsisf (insn, op, NULL_RTX, &len);
	      break;
	    default:
	      break;
	    }
	}
      else if (op[0] == cc0_rtx && REG_P (op[1]))
	{
	  switch (GET_MODE (op[1]))
	    {
	    case HImode: out_tsthi (insn, op[1], &len); break;
	    case SImode: out_tstsi (insn, op[1], &len); break;
	    default: break;
	    }
	}
      else if (GET_CODE (op[1]) == AND)
	{
	  if (GET_CODE (XEXP (op[1],1)) == CONST_INT)
	    {
	      HOST_WIDE_INT mask = INTVAL (XEXP (op[1],1));
	      if (GET_MODE (op[1]) == SImode)
		len = (((mask & 0xff) != 0xff)
		       + ((mask & 0xff00) != 0xff00)
		       + ((mask & 0xff0000L) != 0xff0000L)
		       + ((mask & 0xff000000L) != 0xff000000L));
	      else if (GET_MODE (op[1]) == HImode)
		len = (((mask & 0xff) != 0xff)
		       + ((mask & 0xff00) != 0xff00));
	    }
	}
      else if (GET_CODE (op[1]) == IOR)
	{
	  if (GET_CODE (XEXP (op[1],1)) == CONST_INT)
	    {
	      HOST_WIDE_INT mask = INTVAL (XEXP (op[1],1));
	      if (GET_MODE (op[1]) == SImode)
		len = (((mask & 0xff) != 0)
		       + ((mask & 0xff00) != 0)
		       + ((mask & 0xff0000L) != 0)
		       + ((mask & 0xff000000L) != 0));
	      else if (GET_MODE (op[1]) == HImode)
		len = (((mask & 0xff) != 0)
		       + ((mask & 0xff00) != 0));
	    }
	}
    }
  set = single_set (insn);
  if (set)
    {
      rtx op[10];

      op[1] = SET_SRC (set);
      op[0] = SET_DEST (set);

      if (GET_CODE (patt) == PARALLEL
	  && general_operand (op[1], VOIDmode)
	  && general_operand (op[0], VOIDmode))
	{
	  if (XVECLEN (patt, 0) == 2)
	    op[2] = XVECEXP (patt, 0, 1);

	  switch (GET_MODE (op[0]))
	    {
	    case QImode:
	      len = 2;
	      break;
	    case HImode:
	      output_reload_inhi (insn, op, &len);
	      break;
	    case SImode:
	    case SFmode:
	      output_reload_insisf (insn, op, XEXP (op[2], 0), &len);
	      break;
	    default:
	      break;
	    }
	}
      else if (GET_CODE (op[1]) == ASHIFT
	  || GET_CODE (op[1]) == ASHIFTRT
	  || GET_CODE (op[1]) == LSHIFTRT)
	{
	  rtx ops[10];
	  ops[0] = op[0];
	  ops[1] = XEXP (op[1],0);
	  ops[2] = XEXP (op[1],1);
	  switch (GET_CODE (op[1]))
	    {
	    case ASHIFT:
	      switch (GET_MODE (op[0]))
		{
		case QImode: ashlqi3_out (insn,ops,&len); break;
		case HImode: ashlhi3_out (insn,ops,&len); break;
		case SImode: ashlsi3_out (insn,ops,&len); break;
		default: break;
		}
	      break;
	    case ASHIFTRT:
	      switch (GET_MODE (op[0]))
		{
		case QImode: ashrqi3_out (insn,ops,&len); break;
		case HImode: ashrhi3_out (insn,ops,&len); break;
		case SImode: ashrsi3_out (insn,ops,&len); break;
		default: break;
		}
	      break;
	    case LSHIFTRT:
	      switch (GET_MODE (op[0]))
		{
		case QImode: lshrqi3_out (insn,ops,&len); break;
		case HImode: lshrhi3_out (insn,ops,&len); break;
		case SImode: lshrsi3_out (insn,ops,&len); break;
		default: break;
		}
	      break;
	    default:
	      break;
	    }
	}
    }
  return len;
}

/* Return nonzero if register REG dead after INSN.  */

int
reg_unused_after (rtx insn, rtx reg)
{
  return (dead_or_set_p (insn, reg)
	  || (REG_P(reg) && _reg_unused_after (insn, reg)));
}

/* Return nonzero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels.  It may live past calls or jumps though.  */

int
_reg_unused_after (rtx insn, rtx reg)
{
  enum rtx_code code;
  rtx set;

  /* If the reg is set by this instruction, then it is safe for our
     case.  Disregard the case where this is a store to memory, since
     we are checking a register used in the store address.  */
  set = single_set (insn);
  if (set && GET_CODE (SET_DEST (set)) != MEM
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
	  int i;
	  int retval = 0;

	  for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	    {
	      rtx this_insn = XVECEXP (PATTERN (insn), 0, i);
	      rtx set = single_set (this_insn);

	      if (GET_CODE (this_insn) == CALL_INSN)
		code = CALL_INSN;
	      else if (GET_CODE (this_insn) == JUMP_INSN)
		{
		  if (INSN_ANNULLED_BRANCH_P (this_insn))
		    return 0;
		  code = JUMP_INSN;
		}

	      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
		return 0;
	      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
		{
		  if (GET_CODE (SET_DEST (set)) != MEM)
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
	return GET_CODE (SET_DEST (set)) != MEM;
      if (set == 0 && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	return 0;
    }
  return 1;
}

/* Target hook for assembling integer objects.  The AVR version needs
   special handling for references to certain labels.  */

static bool
avr_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  if (size == POINTER_SIZE / BITS_PER_UNIT && aligned_p
      && text_segment_operand (x, VOIDmode) )
    {
      fputs ("\t.word\tgs(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")\n", asm_out_file);
      return true;
    }
  return default_assemble_integer (x, size, aligned_p);
}

/* Worker function for ASM_DECLARE_FUNCTION_NAME.  */

void
avr_asm_declare_function_name (FILE *file, const char *name, tree decl)
{

  /* If the function has the 'signal' or 'interrupt' attribute, test to
     make sure that the name of the function is "__vector_NN" so as to
     catch when the user misspells the interrupt vector name.  */

  if (cfun->machine->is_interrupt)
    {
      if (!STR_PREFIX_P (name, "__vector"))
        {
          warning_at (DECL_SOURCE_LOCATION (decl), 0,
                      "%qs appears to be a misspelled interrupt handler",
                      name);
        }
    }
  else if (cfun->machine->is_signal)
    {
      if (!STR_PREFIX_P (name, "__vector"))
        {
           warning_at (DECL_SOURCE_LOCATION (decl), 0,
                       "%qs appears to be a misspelled signal handler",
                       name);
        }
    }

  ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");
  ASM_OUTPUT_LABEL (file, name);
}


/* Return value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.  */

static bool
avr_class_likely_spilled_p (reg_class_t c)
{
  return (c != ALL_REGS && c != ADDW_REGS);
}

/* Valid attributes:
   progmem - put data to program memory;
   signal - make a function to be hardware interrupt. After function
   prologue interrupts are disabled;
   interrupt - make a function to be hardware interrupt. After function
   prologue interrupts are enabled;
   naked     - don't generate function prologue/epilogue and `ret' command.

   Only `progmem' attribute valid for type.  */

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

/* Look for attribute `progmem' in DECL
   if found return 1, otherwise 0.  */

int
avr_progmem_p (tree decl, tree attributes)
{
  tree a;

  if (TREE_CODE (decl) != VAR_DECL)
    return 0;

  if (NULL_TREE
      != lookup_attribute ("progmem", attributes))
    return 1;

  a=decl;
  do
    a = TREE_TYPE(a);
  while (TREE_CODE (a) == ARRAY_TYPE);

  if (a == error_mark_node)
    return 0;

  if (NULL_TREE != lookup_attribute ("progmem", TYPE_ATTRIBUTES (a)))
    return 1;
  
  return 0;
}

/* Add the section attribute if the variable is in progmem.  */

static void
avr_insert_attributes (tree node, tree *attributes)
{
  if (TREE_CODE (node) == VAR_DECL
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
      
      if (!TYPE_READONLY (node0))
        {
          error ("variable %q+D must be const in order to be put into"
                 " read-only section by means of %<__attribute__((progmem))%>",
                 node);
        }
    }
}


/* Implement `ASM_OUTPUT_ALIGNED_DECL_LOCAL'.  */
/* Implement `ASM_OUTPUT_ALIGNED_DECL_COMMON'.  */
/* Track need of __do_clear_bss.  */

void
avr_asm_output_aligned_decl_common (FILE * stream, const_tree decl ATTRIBUTE_UNUSED,
                                    const char *name, unsigned HOST_WIDE_INT size,
                                    unsigned int align, bool local_p)
{
  avr_need_clear_bss_p = true;

  if (local_p)
    ASM_OUTPUT_ALIGNED_LOCAL (stream, name, size, align);
  else
    ASM_OUTPUT_ALIGNED_COMMON (stream, name, size, align);
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


/* Implement `TARGET_ASM_INIT_SECTIONS'.  */

static void
avr_asm_init_sections (void)
{
  /* Set up a section for jump tables.  Alignment is handled by
     ASM_OUTPUT_BEFORE_CASE_LABEL.  */
  
  if (AVR_HAVE_JMP_CALL)
    {
      progmem_swtable_section
        = get_unnamed_section (0, output_section_asm_op,
                               "\t.section\t.progmem.gcc_sw_table"
                               ",\"a\",@progbits");
    }
  else
    {
      progmem_swtable_section
        = get_unnamed_section (SECTION_CODE, output_section_asm_op,
                               "\t.section\t.progmem.gcc_sw_table"
                               ",\"ax\",@progbits");
    }

  progmem_section
    = get_unnamed_section (0, output_section_asm_op,
                           "\t.section\t.progmem.data,\"a\",@progbits");
  
  /* Override section callbacks to keep track of `avr_need_clear_bss_p'
     resp. `avr_need_copy_data_p'.  */
  
  readonly_data_section->unnamed.callback = avr_output_data_section_asm_op;
  data_section->unnamed.callback = avr_output_data_section_asm_op;
  bss_section->unnamed.callback = avr_output_bss_section_asm_op;
}


/* Implement `TARGET_ASM_FUNCTION_RODATA_SECTION'.  */

static section*
avr_asm_function_rodata_section (tree decl)
{
  /* If a function is unused and optimized out by -ffunction-sections
     and --gc-sections, ensure that the same will happen for its jump
     tables by putting them into individual sections.  */

  unsigned int flags;
  section * frodata;

  /* Get the frodata section from the default function in varasm.c
     but treat function-associated data-like jump tables as code
     rather than as user defined data.  AVR has no constant pools.  */
  {
    int fdata = flag_data_sections;

    flag_data_sections = flag_function_sections;
    frodata = default_function_rodata_section (decl);
    flag_data_sections = fdata;
    flags = frodata->common.flags;
  }

  if (frodata != readonly_data_section
      && flags & SECTION_NAMED)
    {
      /* Adjust section flags and replace section name prefix.  */

      unsigned int i;

      static const char* const prefix[] =
        {
          ".rodata",          ".progmem.gcc_sw_table",
          ".gnu.linkonce.r.", ".gnu.linkonce.t."
        };

      for (i = 0; i < sizeof (prefix) / sizeof (*prefix); i += 2)
        {
          const char * old_prefix = prefix[i];
          const char * new_prefix = prefix[i+1];
          const char * name = frodata->named.name;

          if (STR_PREFIX_P (name, old_prefix))
            {
              const char *rname = avr_replace_prefix (name, old_prefix, new_prefix);

              flags &= ~SECTION_CODE;
              flags |= AVR_HAVE_JMP_CALL ? 0 : SECTION_CODE;
              
              return get_section (rname, flags, frodata->named.decl);
            }
        }
    }
        
  return progmem_swtable_section;
}


/* Implement `TARGET_ASM_NAMED_SECTION'.  */
/* Track need of __do_clear_bss, __do_copy_data for named sections.  */

static void
avr_asm_named_section (const char *name, unsigned int flags, tree decl)
{
  if (flags & AVR_SECTION_PROGMEM)
    {
      const char *old_prefix = ".rodata";
      const char *new_prefix = ".progmem.data";
      const char *sname = new_prefix;
      
      if (STR_PREFIX_P (name, old_prefix))
        {
          sname = avr_replace_prefix (name, old_prefix, new_prefix);
        }

      default_elf_asm_named_section (sname, flags, decl);

      return;
    }
  
  if (!avr_need_copy_data_p)
    avr_need_copy_data_p = (STR_PREFIX_P (name, ".data")
                            || STR_PREFIX_P (name, ".rodata")
                            || STR_PREFIX_P (name, ".gnu.linkonce.d"));
  
  if (!avr_need_clear_bss_p)
    avr_need_clear_bss_p = STR_PREFIX_P (name, ".bss");
  
  default_elf_asm_named_section (name, flags, decl);
}

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
      flags &= ~SECTION_WRITE;
      flags |= AVR_SECTION_PROGMEM;
    }
  
  return flags;
}


/* Implement `TARGET_ENCODE_SECTION_INFO'.  */

static void
avr_encode_section_info (tree decl, rtx rtl,
                         int new_decl_p)
{
  /* In avr_handle_progmem_attribute, DECL_INITIAL is not yet
     readily available, see PR34734.  So we postpone the warning
     about uninitialized data in program memory section until here.  */
   
  if (new_decl_p
      && decl && DECL_P (decl)
      && NULL_TREE == DECL_INITIAL (decl)
      && avr_progmem_p (decl, DECL_ATTRIBUTES (decl)))
    {
      warning (OPT_Wuninitialized,
               "uninitialized variable %q+D put into "
               "program memory area", decl);
    }

  default_encode_section_info (decl, rtl, new_decl_p);
}


/* Implement `TARGET_ASM_SELECT_SECTION' */

static section *
avr_asm_select_section (tree decl, int reloc, unsigned HOST_WIDE_INT align)
{
  section * sect = default_elf_select_section (decl, reloc, align);
  
  if (decl && DECL_P (decl)
      && avr_progmem_p (decl, DECL_ATTRIBUTES (decl)))
    {
      if (sect->common.flags & SECTION_NAMED)
        {
          const char * name = sect->named.name;
          const char * old_prefix = ".rodata";
          const char * new_prefix = ".progmem.data";

          if (STR_PREFIX_P (name, old_prefix))
            {
              const char *sname = avr_replace_prefix (name, old_prefix, new_prefix);

              return get_section (sname, sect->common.flags, sect->named.decl);
            }
        }
          
      return progmem_section;
    }

  return sect;
}

/* Implement `TARGET_ASM_FILE_START'.  */
/* Outputs some appropriate text to go at the start of an assembler
   file.  */

static void
avr_file_start (void)
{
  if (avr_current_arch->asm_only)
    error ("MCU %qs supported for assembler only", avr_current_device->name);

  default_file_start ();

/*  fprintf (asm_out_file, "\t.arch %s\n", avr_current_device->name);*/
  fputs ("__SREG__ = 0x3f\n"
	 "__SP_H__ = 0x3e\n"
	 "__SP_L__ = 0x3d\n", asm_out_file);
  
  fputs ("__tmp_reg__ = 0\n" 
         "__zero_reg__ = 1\n", asm_out_file);
}


/* Implement `TARGET_ASM_FILE_END'.  */
/* Outputs to the stdio stream FILE some
   appropriate text to go at the end of an assembler file.  */

static void
avr_file_end (void)
{
  /* Output these only if there is anything in the
     .data* / .rodata* / .gnu.linkonce.* resp. .bss*
     input section(s) - some code size can be saved by not
     linking in the initialization code from libgcc if resp.
     sections are empty.  */

  if (avr_need_copy_data_p)
    fputs (".global __do_copy_data\n", asm_out_file);

  if (avr_need_clear_bss_p)
    fputs (".global __do_clear_bss\n", asm_out_file);
}

/* Choose the order in which to allocate hard registers for
   pseudo-registers local to a basic block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.  */

void
order_regs_for_local_alloc (void)
{
  unsigned int i;
  static const int order_0[] = {
    24,25,
    18,19,
    20,21,
    22,23,
    30,31,
    26,27,
    28,29,
    17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,
    0,1,
    32,33,34,35
  };
  static const int order_1[] = {
    18,19,
    20,21,
    22,23,
    24,25,
    30,31,
    26,27,
    28,29,
    17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,
    0,1,
    32,33,34,35
  };
  static const int order_2[] = {
    25,24,
    23,22,
    21,20,
    19,18,
    30,31,
    26,27,
    28,29,
    17,16,
    15,14,13,12,11,10,9,8,7,6,5,4,3,2,
    1,0,
    32,33,34,35
  };
  
  const int *order = (TARGET_ORDER_1 ? order_1 :
		      TARGET_ORDER_2 ? order_2 :
		      order_0);
  for (i=0; i < ARRAY_SIZE (order_0); ++i)
      reg_alloc_order[i] = order[i];
}


/* Implement `TARGET_REGISTER_MOVE_COST' */

static int
avr_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
                        reg_class_t from, reg_class_t to)
{
  return (from == STACK_REG ? 6
          : to == STACK_REG ? 12
          : 2);
}


/* Implement `TARGET_MEMORY_MOVE_COST' */

static int
avr_memory_move_cost (enum machine_mode mode, reg_class_t rclass ATTRIBUTE_UNUSED,
                      bool in ATTRIBUTE_UNUSED)
{
  return (mode == QImode ? 2
          : mode == HImode ? 4
          : mode == SImode ? 8
          : mode == SFmode ? 8
          : 16);
}


/* Mutually recursive subroutine of avr_rtx_cost for calculating the
   cost of an RTX operand given its context.  X is the rtx of the
   operand, MODE is its mode, and OUTER is the rtx_code of this
   operand's parent operator.  */

static int
avr_operand_rtx_cost (rtx x, enum machine_mode mode, enum rtx_code outer,
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
    case CONST_DOUBLE:
      return COSTS_N_INSNS (GET_MODE_SIZE (mode));

    default:
      break;
    }

  total = 0;
  avr_rtx_costs (x, code, outer, opno, &total, speed);
  return total;
}

/* The AVR backend's rtx_cost function.  X is rtx expression whose cost
   is to be calculated.  Return true if the complete cost has been
   computed, and false if subexpressions should be scanned.  In either
   case, *TOTAL contains the cost result.  */

static bool
avr_rtx_costs (rtx x, int codearg, int outer_code ATTRIBUTE_UNUSED,
	       int opno ATTRIBUTE_UNUSED, int *total, bool speed)
{
  enum rtx_code code = (enum rtx_code) codearg;
  enum machine_mode mode = GET_MODE (x);
  HOST_WIDE_INT val;

  switch (code)
    {
    case CONST_INT:
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
	case QImode:
	case SFmode:
	  *total = COSTS_N_INSNS (1);
	  break;

	case HImode:
	  *total = COSTS_N_INSNS (3);
	  break;

	case SImode:
	  *total = COSTS_N_INSNS (7);
	  break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case ABS:
      switch (mode)
	{
	case QImode:
	case SFmode:
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
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case SIGN_EXTEND:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode) + 2
			      - GET_MODE_SIZE (GET_MODE (XEXP (x, 0))));
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case PLUS:
      switch (mode)
	{
	case QImode:
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
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	  break;

	case HImode:
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
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    {
	      *total = COSTS_N_INSNS (2);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else if (INTVAL (XEXP (x, 1)) >= -63 && INTVAL (XEXP (x, 1)) <= 63)
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (2);
	  break;

	case SImode:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    {
	      *total = COSTS_N_INSNS (4);
	      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1,
					      speed);
	    }
	  else if (INTVAL (XEXP (x, 1)) >= -63 && INTVAL (XEXP (x, 1)) <= 63)
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
    case AND:
    case IOR:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode));
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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
	case QImode:
	  if (AVR_HAVE_MUL)
	    *total = COSTS_N_INSNS (!speed ? 3 : 4);
	  else if (!speed)
	    *total = COSTS_N_INSNS (AVR_HAVE_JMP_CALL ? 2 : 1);
	  else
	    return false;
	  break;

	case HImode:
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

	case SImode:
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
	return false;
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
      return true;

    case ROTATE:
      switch (mode)
	{
	case QImode:
	  if (CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) == 4)
	    *total = COSTS_N_INSNS (1);

	  break;

	case HImode:
	  if (CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) == 8)
	    *total = COSTS_N_INSNS (3);

	  break;

	case SImode:
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
	case QImode:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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

	case HImode:
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
          
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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

	case SImode:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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
	case QImode:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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

	case HImode:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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

	case SImode:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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
      switch (mode)
	{
	case QImode:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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

	case HImode:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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

	case SImode:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
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
	case QImode:
	  *total = COSTS_N_INSNS (1);
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	  break;

        case HImode:
	  *total = COSTS_N_INSNS (2);
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
            *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	  else if (INTVAL (XEXP (x, 1)) != 0)
	    *total += COSTS_N_INSNS (1);
          break;

        case SImode:
          *total = COSTS_N_INSNS (4);
          if (GET_CODE (XEXP (x, 1)) != CONST_INT)
            *total += avr_operand_rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	  else if (INTVAL (XEXP (x, 1)) != 0)
	    *total += COSTS_N_INSNS (3);
          break;

	default:
	  return false;
	}
      *total += avr_operand_rtx_cost (XEXP (x, 0), mode, code, 0, speed);
      return true;

    case TRUNCATE:
      if (AVR_HAVE_MUL
          && LSHIFTRT == GET_CODE (XEXP (x, 0))
          && MULT == GET_CODE (XEXP (XEXP (x, 0), 0))
          && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
        {
          if (QImode == mode || HImode == mode)
            {
              *total = COSTS_N_INSNS (2);
              return true;
            }
        }
      break;

    default:
      break;
    }
  return false;
}

/* Calculate the cost of a memory address.  */

static int
avr_address_cost (rtx x, bool speed ATTRIBUTE_UNUSED)
{
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x,1)) == CONST_INT
      && (REG_P (XEXP (x,0)) || GET_CODE (XEXP (x,0)) == SUBREG)
      && INTVAL (XEXP (x,1)) >= 61)
    return 18;
  if (CONSTANT_ADDRESS_P (x))
    {
      if (optimize > 0 && io_address_operand (x, QImode))
	return 2;
      return 4;
    }
  return 4;
}

/* Test for extra memory constraint 'Q'.
   It's a memory address based on Y or Z pointer with valid displacement.  */

int
extra_constraint_Q (rtx x)
{
  if (GET_CODE (XEXP (x,0)) == PLUS
      && REG_P (XEXP (XEXP (x,0), 0))
      && GET_CODE (XEXP (XEXP (x,0), 1)) == CONST_INT
      && (INTVAL (XEXP (XEXP (x,0), 1))
	  <= MAX_LD_OFFSET (GET_MODE (x))))
    {
      rtx xx = XEXP (XEXP (x,0), 0);
      int regno = REGNO (xx);
      if (TARGET_ALL_DEBUG)
	{
	  fprintf (stderr, ("extra_constraint:\n"
			    "reload_completed: %d\n"
			    "reload_in_progress: %d\n"),
		   reload_completed, reload_in_progress);
	  debug_rtx (x);
	}
      if (regno >= FIRST_PSEUDO_REGISTER)
	return 1;		/* allocate pseudos */
      else if (regno == REG_Z || regno == REG_Y)
	return 1;		/* strictly check */
      else if (xx == frame_pointer_rtx
	       || xx == arg_pointer_rtx)
	return 1;		/* XXX frame & arg pointer checks */
    }
  return 0;
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
avr_compare_pattern (rtx insn)
{
  rtx pattern = single_set (insn);

  if (pattern
      && NONJUMP_INSN_P (insn)
      && SET_DEST (pattern) == cc0_rtx
      && GET_CODE (SET_SRC (pattern)) == COMPARE)
    {
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
avr_reorg_remove_redundant_compare (rtx insn1)
{
  rtx comp1, ifelse1, xcond1, branch1;
  rtx comp2, ifelse2, xcond2, branch2, insn2;
  enum rtx_code code;
  rtx jump, target, cond;
  
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
  rtx insn = get_insns();
  
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
          
          rtx next = next_real_insn (insn);
          rtx pat = PATTERN (next);

          pattern = SET_SRC (pattern);
          
          if (true_regnum (XEXP (pattern, 0)) >= 0
              && true_regnum (XEXP (pattern, 1)) >= 0)
            {
              rtx x = XEXP (pattern, 0);
              rtx src = SET_SRC (pat);
              rtx t = XEXP (src,0);
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
              rtx t = XEXP (src,0);
    
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
              rtx t = XEXP (src,0);
              enum machine_mode mode = GET_MODE (XEXP (pattern, 0));
              
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

/* Worker function for TARGET_FUNCTION_VALUE_REGNO_P.  */

static bool
avr_function_value_regno_p (const unsigned int regno)
{
  return (regno == avr_ret_register ());
}

/* Create an RTX representing the place where a
   library function returns a value of mode MODE.  */

static rtx
avr_libcall_value (enum machine_mode mode,
		   const_rtx func ATTRIBUTE_UNUSED)
{
  int offs = GET_MODE_SIZE (mode);
  if (offs < 2)
    offs = 2;
  return gen_rtx_REG (mode, avr_ret_register () + 2 - offs);
}

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


int
jump_over_one_insn_p (rtx insn, rtx dest)
{
  int uid = INSN_UID (GET_CODE (dest) == LABEL_REF
		      ? XEXP (dest, 0)
		      : dest);
  int jump_addr = INSN_ADDRESSES (INSN_UID (insn));
  int dest_addr = INSN_ADDRESSES (uid);
  return dest_addr - jump_addr == get_attr_length (insn) + 1;
}

/* Returns 1 if a value of mode MODE can be stored starting with hard
   register number REGNO.  On the enhanced core, anything larger than
   1 byte must start in even numbered register for "movw" to work
   (this way we don't have to check for odd registers everywhere).  */

int
avr_hard_regno_mode_ok (int regno, enum machine_mode mode)
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
    return 1;

  /* FIXME: Ideally, the following test is not needed.
        However, it turned out that it can reduce the number
        of spill fails.  AVR and it's poor endowment with
        address registers is extreme stress test for reload.  */
  
  if (GET_MODE_SIZE (mode) >= 4
      && regno >= REG_X)
    return 0;

  /* All modes larger than 8 bits should start in an even register.  */
  
  return !(regno & 1);
}

const char *
output_reload_inhi (rtx insn ATTRIBUTE_UNUSED, rtx *operands, int *len)
{
  int tmp;
  if (!len)
    len = &tmp;
      
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int val = INTVAL (operands[1]);
      if ((val & 0xff) == 0)
	{
	  *len = 3;
	  return (AS2 (mov,%A0,__zero_reg__) CR_TAB
		  AS2 (ldi,%2,hi8(%1))       CR_TAB
		  AS2 (mov,%B0,%2));
	}
      else if ((val & 0xff00) == 0)
	{
	  *len = 3;
	  return (AS2 (ldi,%2,lo8(%1)) CR_TAB
		  AS2 (mov,%A0,%2)     CR_TAB
		  AS2 (mov,%B0,__zero_reg__));
	}
      else if ((val & 0xff) == ((val & 0xff00) >> 8))
	{
	  *len = 3;
	  return (AS2 (ldi,%2,lo8(%1)) CR_TAB
		  AS2 (mov,%A0,%2)     CR_TAB
		  AS2 (mov,%B0,%2));
	}
    }
  *len = 4;
  return (AS2 (ldi,%2,lo8(%1)) CR_TAB
	  AS2 (mov,%A0,%2)     CR_TAB
	  AS2 (ldi,%2,hi8(%1)) CR_TAB
	  AS2 (mov,%B0,%2));
}


/* Reload a SI or SF compile time constant (OP[1]) into a GPR (OP[0]).
   CLOBBER_REG is a QI clobber reg needed to move vast majority of consts
   into a NO_LD_REGS.  If CLOBBER_REG is NULL_RTX we either don't need a
   clobber reg or have to cook one up.

   LEN == NULL: Output instructions.
   
   LEN != NULL: Output nothing.  Increment *LEN by number of words occupied
                by the insns printed.

   Return "".  */

const char *
output_reload_insisf (rtx insn ATTRIBUTE_UNUSED,
                      rtx *op, rtx clobber_reg, int *len)
{
  rtx src = op[1];
  rtx dest = op[0];
  rtx xval, xdest[4];
  int ival[4];
  int clobber_val = 1234;
  bool cooked_clobber_p = false;
  bool set_p = false;
  unsigned int n;
  enum machine_mode mode = GET_MODE (dest);
  
  gcc_assert (REG_P (dest));

  if (len)
    *len = 0;
  
  /* (REG:SI 14) is special: It's neither in LD_REGS nor in NO_LD_REGS
     but has some subregs that are in LD_REGS.  Use the MSB (REG:QI 17).  */
  
  if (14 == REGNO (dest))
    {
      clobber_reg = gen_rtx_REG (QImode, 17);
    }

  /* We might need a clobber reg but don't have one.  Look at the value
     to be loaded more closely.  A clobber is only needed if it contains
     a byte that is neither 0, -1 or a power of 2.  */
  
  if (NULL_RTX == clobber_reg
      && !test_hard_reg_class (LD_REGS, dest))
    {
      for (n = 0; n < GET_MODE_SIZE (mode); n++)
        {
          xval = simplify_gen_subreg (QImode, src, mode, n);

          if (!(const0_rtx == xval
                || constm1_rtx == xval
                || single_one_operand (xval, QImode)))
            {
              /* We have no clobber reg but need one.  Cook one up.
                 That's cheaper than loading from constant pool.  */
              
              cooked_clobber_p = true;
              clobber_reg = gen_rtx_REG (QImode, REG_Z + 1);
              avr_asm_len ("mov __tmp_reg__,%0", &clobber_reg, len, 1);
              break;
            }
        }
    }

  /* Now start filling DEST from LSB to MSB.  */
  
  for (n = 0; n < GET_MODE_SIZE (mode); n++)
    {
      bool done_byte = false;
      unsigned int j;
      rtx xop[3];

      /* Crop the n-th sub-byte.  */
      
      xval = simplify_gen_subreg (QImode, src, mode, n);
      xdest[n] = simplify_gen_subreg (QImode, dest, mode, n);
      ival[n] = INTVAL (xval);

      /* Look if we can reuse the low word by means of MOVW.  */
      
      if (n == 2
          && AVR_HAVE_MOVW)
        {
          rtx lo16 = simplify_gen_subreg (HImode, src, mode, 0);
          rtx hi16 = simplify_gen_subreg (HImode, src, mode, 2);

          if (INTVAL (lo16) == INTVAL (hi16))
            {
              avr_asm_len ("movw %C0,%A0", &op[0], len, 1);
              break;
            }
        }

      /* Use CLR to zero a value so that cc0 is set as expected
         for zero.  */
      
      if (ival[n] == 0)
        {
          avr_asm_len ("clr %0", &xdest[n], len, 1);
          continue;
        }

      if (clobber_val == ival[n]
          && REGNO (clobber_reg) == REGNO (xdest[n]))
        {
          continue;
        }

      /* LD_REGS can use LDI to move a constant value */
      
      if (test_hard_reg_class (LD_REGS, xdest[n]))
        {
          xop[0] = xdest[n];
          xop[1] = xval;
          avr_asm_len ("ldi %0,lo8(%1)", xop, len, 1);
          continue;
        }

      /* Try to reuse value already loaded in some lower byte. */
      
      for (j = 0; j < n; j++)
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
          avr_asm_len ("clr %0" CR_TAB
                       "dec %0", &xdest[n], len, 2);
          continue;
        }

      /* Use T flag or INC to manage powers of 2 if we have
         no clobber reg.  */

      if (NULL_RTX == clobber_reg
          && single_one_operand (xval, QImode))
        {
          if (1 == ival[n])
            {
              avr_asm_len ("clr %0" CR_TAB
                           "inc %0", &xdest[n], len, 2);
              continue;
            }
          
          xop[0] = xdest[n];
          xop[1] = GEN_INT (exact_log2 (ival[n] & GET_MODE_MASK (QImode)));

          gcc_assert (constm1_rtx != xop[1]);

          if (!set_p)
            {
              set_p = true;
              avr_asm_len ("set", xop, len, 1);
            }

          avr_asm_len ("clr %0" CR_TAB
                       "bld %0,%1", xop, len, 2);
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
  
  return "";
}

void
avr_output_bld (rtx operands[], int bit_nr)
{
  static char s[] = "bld %A0,0";

  s[5] = 'A' + (bit_nr >> 3);
  s[8] = '0' + (bit_nr & 7);
  output_asm_insn (s, operands);
}

void
avr_output_addr_vec_elt (FILE *stream, int value)
{
  if (AVR_HAVE_JMP_CALL)
    fprintf (stream, "\t.word gs(.L%d)\n", value);
  else
    fprintf (stream, "\trjmp .L%d\n", value);
}

/* Returns true if SCRATCH are safe to be allocated as a scratch
   registers (for a define_peephole2) in the current function.  */

bool
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

const char *
avr_out_sbxx_branch (rtx insn, rtx operands[])
{
  enum rtx_code comp = GET_CODE (operands[0]);
  int long_jump = (get_attr_length (insn) >= 4);
  int reverse = long_jump || jump_over_one_insn_p (insn, operands[3]);

  if (comp == GE)
    comp = EQ;
  else if (comp == LT)
    comp = NE;

  if (reverse)
    comp = reverse_condition (comp);

  if (GET_CODE (operands[1]) == CONST_INT)
    {
      if (INTVAL (operands[1]) < 0x40)
	{
	  if (comp == EQ)
	    output_asm_insn (AS2 (sbis,%m1-0x20,%2), operands);
	  else
	    output_asm_insn (AS2 (sbic,%m1-0x20,%2), operands);
	}
      else
	{
	  output_asm_insn (AS2 (in,__tmp_reg__,%m1-0x20), operands);
	  if (comp == EQ)
	    output_asm_insn (AS2 (sbrs,__tmp_reg__,%2), operands);
	  else
	    output_asm_insn (AS2 (sbrc,__tmp_reg__,%2), operands);
	}
    }
  else  /* GET_CODE (operands[1]) == REG */
    {
      if (GET_MODE (operands[1]) == QImode)
	{
	  if (comp == EQ)
	    output_asm_insn (AS2 (sbrs,%1,%2), operands);
	  else
	    output_asm_insn (AS2 (sbrc,%1,%2), operands);
	}
      else  /* HImode or SImode */
	{
	  static char buf[] = "sbrc %A1,0";
	  int bit_nr = INTVAL (operands[2]);
	  buf[3] = (comp == EQ) ? 's' : 'c';
	  buf[6] = 'A' + (bit_nr >> 3);
	  buf[9] = '0' + (bit_nr & 7);
	  output_asm_insn (buf, operands);
	}
    }

  if (long_jump)
    return (AS1 (rjmp,.+4) CR_TAB
	    AS1 (jmp,%x3));
  if (!reverse)
    return AS1 (rjmp,%x3);
  return "";
}

/* Worker function for TARGET_ASM_CONSTRUCTOR.  */

static void
avr_asm_out_ctor (rtx symbol, int priority)
{
  fputs ("\t.global __do_global_ctors\n", asm_out_file);
  default_ctor_section_asm_out_constructor (symbol, priority);
}

/* Worker function for TARGET_ASM_DESTRUCTOR.  */

static void
avr_asm_out_dtor (rtx symbol, int priority)
{
  fputs ("\t.global __do_global_dtors\n", asm_out_file);
  default_dtor_section_asm_out_destructor (symbol, priority);
}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
avr_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  if (TYPE_MODE (type) == BLKmode)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      return (size == -1 || size > 8);
    }
  else
    return false;
}

/* Worker function for CASE_VALUES_THRESHOLD.  */

unsigned int avr_case_values_threshold (void)
{
  return (!AVR_HAVE_JMP_CALL || TARGET_CALL_PROLOGUES) ? 8 : 17;
}

/* Helper for __builtin_avr_delay_cycles */

static void
avr_expand_delay_cycles (rtx operands0)
{
  unsigned HOST_WIDE_INT cycles = UINTVAL (operands0);
  unsigned HOST_WIDE_INT cycles_used;
  unsigned HOST_WIDE_INT loop_count;
  
  if (IN_RANGE (cycles, 83886082, 0xFFFFFFFF))
    {
      loop_count = ((cycles - 9) / 6) + 1;
      cycles_used = ((loop_count - 1) * 6) + 9;
      emit_insn (gen_delay_cycles_4 (gen_int_mode (loop_count, SImode)));
      cycles -= cycles_used;
    }
  
  if (IN_RANGE (cycles, 262145, 83886081))
    {
      loop_count = ((cycles - 7) / 5) + 1;
      if (loop_count > 0xFFFFFF)
        loop_count = 0xFFFFFF;
      cycles_used = ((loop_count - 1) * 5) + 7;
      emit_insn (gen_delay_cycles_3 (gen_int_mode (loop_count, SImode)));
      cycles -= cycles_used;
    }
  
  if (IN_RANGE (cycles, 768, 262144))
    {
      loop_count = ((cycles - 5) / 4) + 1;
      if (loop_count > 0xFFFF)
        loop_count = 0xFFFF;
      cycles_used = ((loop_count - 1) * 4) + 5;
      emit_insn (gen_delay_cycles_2 (gen_int_mode (loop_count, HImode)));
      cycles -= cycles_used;
    }
  
  if (IN_RANGE (cycles, 6, 767))
    {
      loop_count = cycles / 3;
      if (loop_count > 255) 
        loop_count = 255;
      cycles_used = loop_count * 3;
      emit_insn (gen_delay_cycles_1 (gen_int_mode (loop_count, QImode)));
      cycles -= cycles_used;
      }
  
  while (cycles >= 2)
    {
      emit_insn (gen_nopv (GEN_INT(2)));
      cycles -= 2;
    }

  if (cycles == 1)
    {
      emit_insn (gen_nopv (GEN_INT(1)));
      cycles--;
    }
}

/* IDs for all the AVR builtins.  */

enum avr_builtin_id
  {
    AVR_BUILTIN_NOP,
    AVR_BUILTIN_SEI,
    AVR_BUILTIN_CLI,
    AVR_BUILTIN_WDR,
    AVR_BUILTIN_SLEEP,
    AVR_BUILTIN_SWAP,
    AVR_BUILTIN_FMUL,
    AVR_BUILTIN_FMULS,
    AVR_BUILTIN_FMULSU,
    AVR_BUILTIN_DELAY_CYCLES
  };

#define DEF_BUILTIN(NAME, TYPE, CODE)                                   \
  do                                                                    \
    {                                                                   \
      add_builtin_function ((NAME), (TYPE), (CODE), BUILT_IN_MD,        \
                            NULL, NULL_TREE);                           \
    } while (0)


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

  DEF_BUILTIN ("__builtin_avr_nop", void_ftype_void, AVR_BUILTIN_NOP);
  DEF_BUILTIN ("__builtin_avr_sei", void_ftype_void, AVR_BUILTIN_SEI);
  DEF_BUILTIN ("__builtin_avr_cli", void_ftype_void, AVR_BUILTIN_CLI);
  DEF_BUILTIN ("__builtin_avr_wdr", void_ftype_void, AVR_BUILTIN_WDR);
  DEF_BUILTIN ("__builtin_avr_sleep", void_ftype_void, AVR_BUILTIN_SLEEP);
  DEF_BUILTIN ("__builtin_avr_swap", uchar_ftype_uchar, AVR_BUILTIN_SWAP);
  DEF_BUILTIN ("__builtin_avr_delay_cycles", void_ftype_ulong, 
               AVR_BUILTIN_DELAY_CYCLES);

  DEF_BUILTIN ("__builtin_avr_fmul", uint_ftype_uchar_uchar, 
               AVR_BUILTIN_FMUL);
  DEF_BUILTIN ("__builtin_avr_fmuls", int_ftype_char_char, 
               AVR_BUILTIN_FMULS);
  DEF_BUILTIN ("__builtin_avr_fmulsu", int_ftype_char_uchar, 
               AVR_BUILTIN_FMULSU);
}

#undef DEF_BUILTIN

struct avr_builtin_description
{
  const enum insn_code icode;
  const char *const name;
  const enum avr_builtin_id id;
};

static const struct avr_builtin_description
bdesc_1arg[] =
  {
    { CODE_FOR_rotlqi3_4, "__builtin_avr_swap", AVR_BUILTIN_SWAP }
  };

static const struct avr_builtin_description
bdesc_2arg[] =
  {
    { CODE_FOR_fmul, "__builtin_avr_fmul", AVR_BUILTIN_FMUL },
    { CODE_FOR_fmuls, "__builtin_avr_fmuls", AVR_BUILTIN_FMULS },
    { CODE_FOR_fmulsu, "__builtin_avr_fmulsu", AVR_BUILTIN_FMULSU }
  };

/* Subroutine of avr_expand_builtin to take care of unop insns.  */

static rtx
avr_expand_unop_builtin (enum insn_code icode, tree exp,
                         rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  enum machine_mode op0mode = GET_MODE (op0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    {
      target = gen_reg_rtx (tmode);
    }

  if (op0mode == SImode && mode0 == HImode)
    {
      op0mode = HImode;
      op0 = gen_lowpart (HImode, op0);
    }
  
  gcc_assert (op0mode == mode0 || op0mode == VOIDmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return 0;
  
  emit_insn (pat);
  
  return target;
}


/* Subroutine of avr_expand_builtin to take care of binop insns.  */

static rtx
avr_expand_binop_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  enum machine_mode op0mode = GET_MODE (op0);
  enum machine_mode op1mode = GET_MODE (op1);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    {
      target = gen_reg_rtx (tmode);
    }

  if ((op0mode == SImode || op0mode == VOIDmode) && mode0 == HImode)
    {
      op0mode = HImode;
      op0 = gen_lowpart (HImode, op0);
    }
  
  if ((op1mode == SImode || op1mode == VOIDmode) && mode1 == HImode)
    {
      op1mode = HImode;
      op1 = gen_lowpart (HImode, op1);
    }
  
  /* In case the insn wants input operands in modes different from
     the result, abort.  */
  
  gcc_assert ((op0mode == mode0 || op0mode == VOIDmode)
              && (op1mode == mode1 || op1mode == VOIDmode));

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (target, op0, op1);
  
  if (! pat)
    return 0;

  emit_insn (pat);
  return target;
}


/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
avr_expand_builtin (tree exp, rtx target,
                    rtx subtarget ATTRIBUTE_UNUSED,
                    enum machine_mode mode ATTRIBUTE_UNUSED,
                    int ignore ATTRIBUTE_UNUSED)
{
  size_t i;
  const struct avr_builtin_description *d;
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int id = DECL_FUNCTION_CODE (fndecl);
  tree arg0;
  rtx op0;

  switch (id)
    {
    case AVR_BUILTIN_NOP:
      emit_insn (gen_nopv (GEN_INT(1)));
      return 0;
      
    case AVR_BUILTIN_SEI:
      emit_insn (gen_enable_interrupt ());
      return 0;
      
    case AVR_BUILTIN_CLI:
      emit_insn (gen_disable_interrupt ());
      return 0;
      
    case AVR_BUILTIN_WDR:
      emit_insn (gen_wdr ());
      return 0;
      
    case AVR_BUILTIN_SLEEP:
      emit_insn (gen_sleep ());
      return 0;
      
    case AVR_BUILTIN_DELAY_CYCLES:
      {
        arg0 = CALL_EXPR_ARG (exp, 0);
        op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);

        if (! CONST_INT_P (op0))
          error ("__builtin_avr_delay_cycles expects a compile time integer constant.");

        avr_expand_delay_cycles (op0);
        return 0;
      }
    }

  for (i = 0, d = bdesc_1arg; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    if (d->id == id)
      return avr_expand_unop_builtin (d->icode, exp, target);

  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->id == id)
      return avr_expand_binop_builtin (d->icode, exp, target);

  gcc_unreachable ();
}


#include "gt-avr.h"
