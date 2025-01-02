/* Copyright (C) 2016-2025 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* {{{ Includes.  */

/* We want GET_MODE_SIZE et al to return integers, please.  */
#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "memmodel.h"
#include "rtl.h"
#include "tree.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "insn-attr.h"
#include "fold-const.h"
#include "calls.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "cfgrtl.h"
#include "langhooks.h"
#include "builtins.h"
#include "omp-general.h"
#include "print-rtl.h"
#include "attribs.h"
#include "varasm.h"
#include "intl.h"
#include "rtl-iter.h"
#include "dwarf2.h"
#include "gimple.h"
#include "cgraph.h"
#include "case-cfn-macros.h"

/* This file should be included last.  */
#include "target-def.h"

/* }}}  */
/* {{{ Global variables.  */

/* Constants used by FP instructions.  */

static REAL_VALUE_TYPE dconst4, dconst1over2pi;
static bool ext_gcn_constants_init = 0;

/* Holds the ISA variant, derived from the command line parameters.  */

enum gcn_isa gcn_isa = ISA_GCN5;	/* Default to GCN5.  */

/* Reserve this much space for LDS (for propagating variables from
   worker-single mode to worker-partitioned mode), per workgroup.  Global
   analysis could calculate an exact bound, but we don't do that yet.

   We want to permit full occupancy, so size accordingly.  */

/* Use this as a default, but allow it to grow if the user requests a large
   amount of gang-private shared-memory space.  */
static int acc_lds_size = 0x600;

#define OMP_LDS_SIZE 0x600    /* 0x600 is 1/40 total, rounded down.  */
#define ACC_LDS_SIZE acc_lds_size
#define OTHER_LDS_SIZE 65536  /* If in doubt, reserve all of it.  */

#define LDS_SIZE (flag_openacc ? ACC_LDS_SIZE \
		  : flag_openmp ? OMP_LDS_SIZE \
		  : OTHER_LDS_SIZE)

static int gang_private_hwm = 32;
static hash_map<tree, int> lds_allocs;

/* The number of registers usable by normal non-kernel functions.
   The SGPR count includes any special extra registers such as VCC.  */

#define MAX_NORMAL_SGPR_COUNT	62  // i.e. 64 with VCC
#define MAX_NORMAL_VGPR_COUNT	24
#define MAX_NORMAL_AVGPR_COUNT	24

/* Import all the data from gcn-devices.def.
   The PROCESSOR_GFXnnn should be indices for this table.  */
const struct gcn_device_def gcn_devices[] = {
#define GCN_DEVICE(name, NAME, ELF, ISA, XNACK, SRAMECC, WAVE64, CU, VGPRS, GEN_VER,ARCH_FAM) \
    {PROCESSOR_ ## NAME, #name, #NAME, ISA, XNACK, SRAMECC, WAVE64, CU, VGPRS, \
     GEN_VER, #ARCH_FAM},
#include "gcn-devices.def"
};

/* }}}  */
/* {{{ Initialization and options.  */

/* Initialize machine_function.  */

static struct machine_function *
gcn_init_machine_status (void)
{
  struct machine_function *f;

  f = ggc_cleared_alloc<machine_function> ();

  // FIXME: re-enable global addressing with safety for LDS-flat addresses
  //if (TARGET_GCN3)
    f->use_flat_addressing = true;

  return f;
}

/* Implement TARGET_OPTION_OVERRIDE.

   Override option settings where defaults are variable, or we have specific
   needs to consider.  */

static void
gcn_option_override (void)
{
  init_machine_status = gcn_init_machine_status;

  /* The HSA runtime does not respect ELF load addresses, so force PIE.  */
  if (!flag_pie)
    flag_pie = 2;
  if (!flag_pic)
    flag_pic = flag_pie;

  gcc_assert (gcn_arch >= 0 && gcn_arch < PROCESSOR_COUNT);
  gcn_isa = gcn_devices[gcn_arch].isa;

  /* Reserve 1Kb (somewhat arbitrarily) of LDS space for reduction results and
     worker broadcasts.  */
  if (gang_private_size_opt == -1)
    gang_private_size_opt = 512;
  else if (gang_private_size_opt < gang_private_hwm)
    gang_private_size_opt = gang_private_hwm;
  else if (gang_private_size_opt >= acc_lds_size - 1024)
    {
      /* We need some space for reductions and worker broadcasting.  If the
	 user requests a large amount of gang-private LDS space, we might not
	 have enough left for the former.  Increase the LDS allocation in that
	 case, although this may reduce the maximum occupancy on the
	 hardware.  */
      acc_lds_size = gang_private_size_opt + 1024;
      if (acc_lds_size > 32768)
	acc_lds_size = 32768;
    }

  /* gfx1030 and gfx1100 do not support XNACK.  */
  if (gcn_devices[gcn_arch].xnack_default == HSACO_ATTR_UNSUPPORTED)
    {
      if (flag_xnack == HSACO_ATTR_ON)
	error ("%<-mxnack=on%> is incompatible with %<-march=%s%>",
	       gcn_devices[gcn_arch].name);
      /* Allow HSACO_ATTR_ANY silently.  */
      flag_xnack = HSACO_ATTR_UNSUPPORTED;
    }

  /* There's no need for XNACK on devices without USM, and there are register
     allocation problems caused by the early-clobber when AVGPR spills are not
     available.
     FIXME: can the regalloc mean the default can be really "any"?  */
  if (flag_xnack == HSACO_ATTR_DEFAULT)
    flag_xnack = gcn_devices[gcn_arch].xnack_default;

  if (flag_sram_ecc == HSACO_ATTR_DEFAULT)
    flag_sram_ecc = gcn_devices[gcn_arch].sramecc_default;
}

/* }}}  */
/* {{{ Attributes.  */

/* This table defines the arguments that are permitted in
   __attribute__ ((amdgpu_hsa_kernel (...))).

   The names and values correspond to the HSA metadata that is encoded
   into the assembler file and binary.  */

static const struct gcn_kernel_arg_type
{
  const char *name;
  const char *header_pseudo;
  machine_mode mode;

  /* This should be set to -1 or -2 for a dynamically allocated register
     number.  Use -1 if this argument contributes to the user_sgpr_count,
     -2 otherwise.  */
  int fixed_regno;
} gcn_kernel_arg_types[] = {
  {"exec", NULL, DImode, EXEC_REG},
#define PRIVATE_SEGMENT_BUFFER_ARG 1
  {"private_segment_buffer",
    ".amdhsa_user_sgpr_private_segment_buffer", TImode, -1},
#define DISPATCH_PTR_ARG 2
  {"dispatch_ptr", ".amdhsa_user_sgpr_dispatch_ptr", DImode, -1},
#define QUEUE_PTR_ARG 3
  {"queue_ptr", ".amdhsa_user_sgpr_queue_ptr", DImode, -1},
#define KERNARG_SEGMENT_PTR_ARG 4
  {"kernarg_segment_ptr", ".amdhsa_user_sgpr_kernarg_segment_ptr", DImode, -1},
  {"dispatch_id", ".amdhsa_user_sgpr_dispatch_id", DImode, -1},
#define FLAT_SCRATCH_INIT_ARG 6
  {"flat_scratch_init", ".amdhsa_user_sgpr_flat_scratch_init", DImode, -1},
#define FLAT_SCRATCH_SEGMENT_SIZE_ARG 7
  {"private_segment_size", ".amdhsa_user_sgpr_private_segment_size", SImode, -1},
#define WORKGROUP_ID_X_ARG 8
  {"workgroup_id_X", ".amdhsa_system_sgpr_workgroup_id_x", SImode, -2},
  {"workgroup_id_Y", ".amdhsa_system_sgpr_workgroup_id_y", SImode, -2},
  {"workgroup_id_Z", ".amdhsa_system_sgpr_workgroup_id_z", SImode, -2},
  {"workgroup_info", ".amdhsa_system_sgpr_workgroup_info", SImode, -1},
#define PRIVATE_SEGMENT_WAVE_OFFSET_ARG 12
  {"private_segment_wave_offset",
    ".amdhsa_system_sgpr_private_segment_wavefront_offset", SImode, -2},
#define WORK_ITEM_ID_X_ARG 13
  {"work_item_id_X", NULL, V64SImode, FIRST_VGPR_REG},
#define WORK_ITEM_ID_Y_ARG 14
  {"work_item_id_Y", NULL, V64SImode, FIRST_VGPR_REG + 1},
#define WORK_ITEM_ID_Z_ARG 15
  {"work_item_id_Z", NULL, V64SImode, FIRST_VGPR_REG + 2}
};

static const long default_requested_args
	= (1 << DISPATCH_PTR_ARG)
	  | (1 << QUEUE_PTR_ARG)
	  | (1 << KERNARG_SEGMENT_PTR_ARG)
	  | (1 << WORKGROUP_ID_X_ARG)
	  | (1 << WORK_ITEM_ID_X_ARG)
	  | (1 << WORK_ITEM_ID_Y_ARG)
	  | (1 << WORK_ITEM_ID_Z_ARG);

/* Extract parameter settings from __attribute__((amdgpu_hsa_kernel ())).
   This function also sets the default values for some arguments.

   Return true on success, with ARGS populated.  */

static bool
gcn_parse_amdgpu_hsa_kernel_attribute (struct gcn_kernel_args *args,
				       tree list)
{
  bool err = false;
  args->requested = default_requested_args;
  args->nargs = 0;

  for (int a = 0; a < GCN_KERNEL_ARG_TYPES; a++)
    args->reg[a] = -1;

  for (; list; list = TREE_CHAIN (list))
    {
      const char *str;
      if (TREE_CODE (TREE_VALUE (list)) != STRING_CST)
	{
	  error ("%<amdgpu_hsa_kernel%> attribute requires string constant "
		 "arguments");
	  break;
	}
      str = TREE_STRING_POINTER (TREE_VALUE (list));
      int a;
      for (a = 0; a < GCN_KERNEL_ARG_TYPES; a++)
	{
	  if (!strcmp (str, gcn_kernel_arg_types[a].name))
	    break;
	}
      if (a == GCN_KERNEL_ARG_TYPES)
	{
	  error ("unknown specifier %qs in %<amdgpu_hsa_kernel%> attribute",
		 str);
	  err = true;
	  break;
	}
      if (args->requested & (1 << a))
	{
	  error ("duplicated parameter specifier %qs in %<amdgpu_hsa_kernel%> "
		 "attribute", str);
	  err = true;
	  break;
	}
      args->requested |= (1 << a);
      args->order[args->nargs++] = a;
    }

  /* Requesting WORK_ITEM_ID_Z_ARG implies requesting WORK_ITEM_ID_X_ARG and
     WORK_ITEM_ID_Y_ARG.  Similarly, requesting WORK_ITEM_ID_Y_ARG implies
     requesting WORK_ITEM_ID_X_ARG.  */
  if (args->requested & (1 << WORK_ITEM_ID_Z_ARG))
    args->requested |= (1 << WORK_ITEM_ID_Y_ARG);
  if (args->requested & (1 << WORK_ITEM_ID_Y_ARG))
    args->requested |= (1 << WORK_ITEM_ID_X_ARG);

  int sgpr_regno = FIRST_SGPR_REG;
  args->nsgprs = 0;
  for (int a = 0; a < GCN_KERNEL_ARG_TYPES; a++)
    {
      if (!(args->requested & (1 << a)))
	continue;

      if (gcn_kernel_arg_types[a].fixed_regno >= 0)
	args->reg[a] = gcn_kernel_arg_types[a].fixed_regno;
      else
	{
	  int reg_count;

	  switch (gcn_kernel_arg_types[a].mode)
	    {
	    case E_SImode:
	      reg_count = 1;
	      break;
	    case E_DImode:
	      reg_count = 2;
	      break;
	    case E_TImode:
	      reg_count = 4;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  args->reg[a] = sgpr_regno;
	  sgpr_regno += reg_count;
	  if (gcn_kernel_arg_types[a].fixed_regno == -1)
	    args->nsgprs += reg_count;
	}
    }
  if (sgpr_regno > FIRST_SGPR_REG + 16)
    {
      error ("too many arguments passed in sgpr registers");
    }
  return err;
}

/* Referenced by TARGET_ATTRIBUTE_TABLE.

   Validates target specific attributes.  */

static tree
gcn_handle_amdgpu_hsa_kernel_attribute (tree *node, tree name,
					tree args, int, bool *no_add_attrs)
{
  if (!FUNC_OR_METHOD_TYPE_P (*node))
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Can combine regparm with all attributes but fastcall, and thiscall.  */
  if (is_attribute_p ("gcnhsa_kernel", name))
    {
      struct gcn_kernel_args kernelarg;

      if (gcn_parse_amdgpu_hsa_kernel_attribute (&kernelarg, args))
	*no_add_attrs = true;

      return NULL_TREE;
    }

  return NULL_TREE;
}

/* Implement TARGET_ATTRIBUTE_TABLE.

   Create target-specific __attribute__ types.  */

TARGET_GNU_ATTRIBUTES (gcn_attribute_table, {
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
     affects_type_identity } */
  {"amdgpu_hsa_kernel", 0, GCN_KERNEL_ARG_TYPES, false, true,
   true, true, gcn_handle_amdgpu_hsa_kernel_attribute, NULL}
});

/* }}}  */
/* {{{ Registers and modes.  */

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P.  */

bool
gcn_scalar_mode_supported_p (scalar_mode mode)
{
  return (mode == BImode
	  || mode == QImode
	  || mode == HImode /* || mode == HFmode  */
	  || mode == SImode || mode == SFmode
	  || mode == DImode || mode == DFmode
	  || mode == TImode);
}

/* Return a vector mode with N lanes of MODE.  */

static machine_mode
VnMODE (int n, machine_mode mode)
{
  switch (mode)
    {
    case E_QImode:
      switch (n)
	{
	case 2: return V2QImode;
	case 4: return V4QImode;
	case 8: return V8QImode;
	case 16: return V16QImode;
	case 32: return V32QImode;
	case 64: return V64QImode;
	}
      break;
    case E_HImode:
      switch (n)
	{
	case 2: return V2HImode;
	case 4: return V4HImode;
	case 8: return V8HImode;
	case 16: return V16HImode;
	case 32: return V32HImode;
	case 64: return V64HImode;
	}
      break;
    case E_HFmode:
      switch (n)
	{
	case 2: return V2HFmode;
	case 4: return V4HFmode;
	case 8: return V8HFmode;
	case 16: return V16HFmode;
	case 32: return V32HFmode;
	case 64: return V64HFmode;
	}
      break;
    case E_SImode:
      switch (n)
	{
	case 2: return V2SImode;
	case 4: return V4SImode;
	case 8: return V8SImode;
	case 16: return V16SImode;
	case 32: return V32SImode;
	case 64: return V64SImode;
	}
      break;
    case E_SFmode:
      switch (n)
	{
	case 2: return V2SFmode;
	case 4: return V4SFmode;
	case 8: return V8SFmode;
	case 16: return V16SFmode;
	case 32: return V32SFmode;
	case 64: return V64SFmode;
	}
      break;
    case E_DImode:
      switch (n)
	{
	case 2: return V2DImode;
	case 4: return V4DImode;
	case 8: return V8DImode;
	case 16: return V16DImode;
	case 32: return V32DImode;
	case 64: return V64DImode;
	}
      break;
    case E_DFmode:
      switch (n)
	{
	case 2: return V2DFmode;
	case 4: return V4DFmode;
	case 8: return V8DFmode;
	case 16: return V16DFmode;
	case 32: return V32DFmode;
	case 64: return V64DFmode;
	}
      break;
    default:
      break;
    }

  return VOIDmode;
}

/* Implement TARGET_CLASS_MAX_NREGS.

   Return the number of hard registers needed to hold a value of MODE in
   a register of class RCLASS.  */

static unsigned char
gcn_class_max_nregs (reg_class_t rclass, machine_mode mode)
{
  /* Scalar registers are 32bit, vector registers are in fact tuples of
     64 lanes.  */
  if (rclass == VGPR_REGS || rclass == AVGPR_REGS
      || rclass == ALL_VGPR_REGS)
    {
      if (vgpr_1reg_mode_p (mode))
	return 1;
      if (vgpr_2reg_mode_p (mode))
	return 2;
      /* TImode is used by DImode compare_and_swap.  */
      if (vgpr_4reg_mode_p (mode))
	return 4;
    }
  else if (rclass == VCC_CONDITIONAL_REG && mode == BImode)
    return 2;

  /* Vector modes in SGPRs are not supposed to happen (disallowed by
     gcn_hard_regno_mode_ok), but there are some patterns that have an "Sv"
     constraint and are used by splitters, post-reload.
     This ensures that we don't accidentally mark the following 63 scalar
     registers as "live".  */
  if (rclass == SGPR_REGS && VECTOR_MODE_P (mode))
    return CEIL (GET_MODE_SIZE (GET_MODE_INNER (mode)), 4);

  return CEIL (GET_MODE_SIZE (mode), 4);
}

/* Implement TARGET_HARD_REGNO_NREGS.

   Return the number of hard registers needed to hold a value of MODE in
   REGNO.  */

unsigned int
gcn_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  return gcn_class_max_nregs (REGNO_REG_CLASS (regno), mode);
}

/* Implement TARGET_HARD_REGNO_MODE_OK.

   Return true if REGNO can hold value in MODE.  */

bool
gcn_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  /* Treat a complex mode as if it were a scalar mode of the same overall
     size for the purposes of allocating hard registers.  */
  if (COMPLEX_MODE_P (mode))
    switch (mode)
      {
      case E_CQImode:
      case E_CHImode:
	mode = SImode;
	break;
      case E_CSImode:
	mode = DImode;
	break;
      case E_CDImode:
	mode = TImode;
	break;
      case E_HCmode:
	mode = SFmode;
	break;
      case E_SCmode:
	mode = DFmode;
	break;
      default:
	/* Not supported.  */
	return false;
      }

  switch (regno)
    {
    case FLAT_SCRATCH_LO_REG:
    case XNACK_MASK_LO_REG:
    case TBA_LO_REG:
    case TMA_LO_REG:
      return (mode == SImode || mode == DImode);
    case VCC_LO_REG:
    case EXEC_LO_REG:
      return (mode == BImode || mode == SImode || mode == DImode);
    case M0_REG:
    case FLAT_SCRATCH_HI_REG:
    case XNACK_MASK_HI_REG:
    case TBA_HI_REG:
    case TMA_HI_REG:
      return mode == SImode;
    case VCC_HI_REG:
      return false;
    case EXEC_HI_REG:
      return mode == SImode /*|| mode == V32BImode */ ;
    case SCC_REG:
    case VCCZ_REG:
    case EXECZ_REG:
      return mode == BImode;
    }
  if (regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM)
    return true;
  if (SGPR_REGNO_P (regno))
    /* We restrict double register values to aligned registers.  */
    return (sgpr_1reg_mode_p (mode)
	    || (!((regno - FIRST_SGPR_REG) & 1) && sgpr_2reg_mode_p (mode))
	    || (((regno - FIRST_SGPR_REG) & 3) == 0 && mode == TImode));
  if (VGPR_REGNO_P (regno) || (AVGPR_REGNO_P (regno) && TARGET_AVGPRS))
    /* Vector instructions do not care about the alignment of register
       pairs, but where there is no 64-bit instruction, many of the
       define_split do not work if the input and output registers partially
       overlap.  We tried to fix this with early clobber and match
       constraints, but it was bug prone, added complexity, and conflicts
       with the 'U0' constraints on vec_merge.
       Therefore, we restrict ourselved to aligned registers.  */
    return (vgpr_1reg_mode_p (mode)
	    || (!((regno - FIRST_VGPR_REG) & 1) && vgpr_2reg_mode_p (mode))
	    /* TImode is used by DImode compare_and_swap,
	       and by DIVMOD V64DImode libfuncs.  */
	    || (!((regno - FIRST_VGPR_REG) & 3) && vgpr_4reg_mode_p (mode)));
  return false;
}

/* Implement REGNO_REG_CLASS via gcn.h.

   Return smallest class containing REGNO.  */

enum reg_class
gcn_regno_reg_class (int regno)
{
  switch (regno)
    {
    case SCC_REG:
      return SCC_CONDITIONAL_REG;
    case VCC_LO_REG:
    case VCC_HI_REG:
      return VCC_CONDITIONAL_REG;
    case VCCZ_REG:
      return VCCZ_CONDITIONAL_REG;
    case EXECZ_REG:
      return EXECZ_CONDITIONAL_REG;
    case EXEC_LO_REG:
    case EXEC_HI_REG:
      return EXEC_MASK_REG;
    }
  if (VGPR_REGNO_P (regno))
    return VGPR_REGS;
  if (AVGPR_REGNO_P (regno))
    return AVGPR_REGS;
  if (SGPR_REGNO_P (regno))
    return SGPR_REGS;
  if (regno < FIRST_VGPR_REG)
    return GENERAL_REGS;
  if (regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM)
    return AFP_REGS;
  return ALL_REGS;
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.

   GCC assumes that lowpart contains first part of value as stored in memory.
   This is not the case for vector registers.  */

bool
gcn_can_change_mode_class (machine_mode from, machine_mode to,
			   reg_class_t regclass)
{
  if (!vgpr_vector_mode_p (from) && !vgpr_vector_mode_p (to))
    return true;

  /* Vector conversions are only valid when changing mode with a fixed number
     of lanes, or changing number of lanes with a fixed mode.  Anything else
     would require actual data movement.  */
  if (VECTOR_MODE_P (from) && VECTOR_MODE_P (to)
      && GET_MODE_NUNITS (from) != GET_MODE_NUNITS (to)
      && GET_MODE_INNER (from) != GET_MODE_INNER (to))
    return false;

  /* Vector/scalar conversions are only permitted when the scalar mode
     is the same or smaller than the inner vector mode.  */
  if ((VECTOR_MODE_P (from) && !VECTOR_MODE_P (to)
       && GET_MODE_SIZE (to) >= GET_MODE_SIZE (GET_MODE_INNER (from)))
      || (VECTOR_MODE_P (to) && !VECTOR_MODE_P (from)
	  && GET_MODE_SIZE (from) >= GET_MODE_SIZE (GET_MODE_INNER (to))))
    return false;

  return (gcn_class_max_nregs (regclass, from)
	  == gcn_class_max_nregs (regclass, to));
}

/* Implement TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P.

   When this hook returns true for MODE, the compiler allows
   registers explicitly used in the rtl to be used as spill registers
   but prevents the compiler from extending the lifetime of these
   registers.  */

bool
gcn_small_register_classes_for_mode_p (machine_mode mode)
{
  /* We allocate into exec and vcc regs.  Those make small register class.  */
  return mode == DImode || mode == SImode;
}

/* Implement TARGET_CLASS_LIKELY_SPILLED_P.

   Returns true if pseudos that have been assigned to registers of class RCLASS
   would likely be spilled because registers of RCLASS are needed for spill
   registers.  */

static bool
gcn_class_likely_spilled_p (reg_class_t rclass)
{
  return (rclass == EXEC_MASK_REG
	  || reg_classes_intersect_p (ALL_CONDITIONAL_REGS, rclass));
}

/* Implement TARGET_MODES_TIEABLE_P.

   Returns true if a value of MODE1 is accessible in MODE2 without
   copying.  */

bool
gcn_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if (VECTOR_MODE_P (mode1) || VECTOR_MODE_P (mode2))
    {
      int vf1 = (VECTOR_MODE_P (mode1) ? GET_MODE_NUNITS (mode1) : 1);
      int vf2 = (VECTOR_MODE_P (mode2) ? GET_MODE_NUNITS (mode2) : 1);
      machine_mode inner1 = (vf1 > 1 ? GET_MODE_INNER (mode1) : mode1);
      machine_mode inner2 = (vf2 > 1 ? GET_MODE_INNER (mode2) : mode2);

      return (vf1 == vf2 || (inner1 == inner2 && vf2 <= vf1));
    }

  return (GET_MODE_BITSIZE (mode1) <= MAX_FIXED_MODE_SIZE
	  && GET_MODE_BITSIZE (mode2) <= MAX_FIXED_MODE_SIZE);
}

/* Implement TARGET_TRULY_NOOP_TRUNCATION.

   Returns true if it is safe to “convert” a value of INPREC bits to one of
   OUTPREC bits (where OUTPREC is smaller than INPREC) by merely operating on
   it as if it had only OUTPREC bits.  */

bool
gcn_truly_noop_truncation (poly_uint64 outprec, poly_uint64 inprec)
{
  return ((inprec <= 32) && (outprec <= inprec));
}

/* Return N-th part of value occupying multiple registers.  */

rtx
gcn_operand_part (machine_mode mode, rtx op, int n)
{
  int vf = VECTOR_MODE_P (mode) ? GET_MODE_NUNITS (mode) : 1;

  if (vf > 1)
    {
      machine_mode vsimode = VnMODE (vf, SImode);

      if (REG_P (op))
	{
	  gcc_assert (REGNO (op) + n < FIRST_PSEUDO_REGISTER);
	  return gen_rtx_REG (vsimode, REGNO (op) + n);
	}
      if (GET_CODE (op) == CONST_VECTOR)
	{
	  int units = GET_MODE_NUNITS (mode);
	  rtvec v = rtvec_alloc (units);

	  for (int i = 0; i < units; ++i)
	    RTVEC_ELT (v, i) = gcn_operand_part (GET_MODE_INNER (mode),
						 CONST_VECTOR_ELT (op, i), n);

	  return gen_rtx_CONST_VECTOR (vsimode, v);
	}
      if (GET_CODE (op) == UNSPEC && XINT (op, 1) == UNSPEC_VECTOR)
	return gcn_gen_undef (vsimode);
      gcc_unreachable ();
    }
  else if (GET_MODE_SIZE (mode) == 8 && REG_P (op))
    {
      gcc_assert (REGNO (op) + n < FIRST_PSEUDO_REGISTER);
      return gen_rtx_REG (SImode, REGNO (op) + n);
    }
  else
    {
      if (GET_CODE (op) == UNSPEC && XINT (op, 1) == UNSPEC_VECTOR)
	return gcn_gen_undef (SImode);

      /* If it's a constant then let's assume it is of the largest mode
	 available, otherwise simplify_gen_subreg will fail.  */
      if (mode == VOIDmode && CONST_INT_P (op))
	mode = DImode;
      return simplify_gen_subreg (SImode, op, mode, n * 4);
    }
}

/* Return N-th part of value occupying multiple registers.  */

rtx
gcn_operand_doublepart (machine_mode mode, rtx op, int n)
{
  return simplify_gen_subreg (DImode, op, mode, n * 8);
}

/* Return true if OP can be split into subregs or high/low parts.
   This is always true for scalars, but not normally true for vectors.
   However, for vectors in hardregs we can use the low and high registers.  */

bool
gcn_can_split_p (machine_mode, rtx op)
{
  if (vgpr_vector_mode_p (GET_MODE (op)))
    {
      if (GET_CODE (op) == SUBREG)
	op = SUBREG_REG (op);
      if (!REG_P (op))
	return true;
      return REGNO (op) <= FIRST_PSEUDO_REGISTER;
    }
  return true;
}

/* Implement TARGET_SPILL_CLASS.

   Return class of registers which could be used for pseudo of MODE
   and of class RCLASS for spilling instead of memory.  Return NO_REGS
   if it is not possible or non-profitable.  */

static reg_class_t
gcn_spill_class (reg_class_t c, machine_mode /*mode */ )
{
  if (reg_classes_intersect_p (ALL_CONDITIONAL_REGS, c)
      || c == VCC_CONDITIONAL_REG || c == EXEC_MASK_REG)
    return SGPR_REGS;
  else
    return c == VGPR_REGS && TARGET_AVGPRS ? AVGPR_REGS : NO_REGS;
}

/* Implement TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS.

   Change allocno class for given pseudo from allocno and best class
   calculated by IRA.  */

static reg_class_t
gcn_ira_change_pseudo_allocno_class (int regno, reg_class_t cl,
				     reg_class_t best_cl)
{
  /* Avoid returning classes that contain both vgpr and sgpr registers.  */
  if (cl != ALL_REGS && cl != SRCDST_REGS && cl != ALL_GPR_REGS)
    return cl;
  if (best_cl != ALL_REGS && best_cl != SRCDST_REGS
      && best_cl != ALL_GPR_REGS)
    return best_cl;

  machine_mode mode = PSEUDO_REGNO_MODE (regno);
  if (vgpr_vector_mode_p (mode))
    return VGPR_REGS;

  return GENERAL_REGS;
}

/* Create a new DImode pseudo reg and emit an instruction to initialize
   it to VAL.  */

rtx
get_exec (int64_t val)
{
  rtx reg = gen_reg_rtx (DImode);
  emit_insn (gen_rtx_SET (reg, gen_int_mode (val, DImode)));
  return reg;
}

rtx
get_exec (machine_mode mode)
{
  int vf = (VECTOR_MODE_P (mode) ? GET_MODE_NUNITS (mode) : 1);
  return get_exec (0xffffffffffffffffUL >> (64-vf));
}

/* }}}  */
/* {{{ Immediate constants.  */

/* Initialize shared numeric constants.  */

static void
init_ext_gcn_constants (void)
{
  real_from_integer (&dconst4, DFmode, 4, SIGNED);

  /* FIXME: this constant probably does not match what hardware really loads.
     Reality check it eventually.  */
  real_from_string (&dconst1over2pi,
		    "0.15915494309189532");
  real_convert (&dconst1over2pi, SFmode, &dconst1over2pi);

  ext_gcn_constants_init = 1;
}

REAL_VALUE_TYPE
gcn_dconst1over2pi (void)
{
  if (!ext_gcn_constants_init)
    init_ext_gcn_constants ();
  return dconst1over2pi;
}

/* Return non-zero if X is a constant that can appear as an inline operand.
   This is 0, 0.5, -0.5, 1, -1, 2, -2, 4,-4, 1/(2*pi)
   Or a vector of those.
   The value returned should be the encoding of this constant.  */

int
gcn_inline_fp_constant_p (rtx x, bool allow_vector)
{
  machine_mode mode = GET_MODE (x);
  int vf = VECTOR_MODE_P (mode) ? GET_MODE_NUNITS (mode) : 1;

  if (vf > 1)
    mode = GET_MODE_INNER (mode);

  if (vf > 1
      && (mode == HFmode || mode == SFmode || mode == DFmode)
      && allow_vector)
    {
      int n;
      if (GET_CODE (x) != CONST_VECTOR)
	return 0;
      n = gcn_inline_fp_constant_p (CONST_VECTOR_ELT (x, 0), false);
      if (!n)
	return 0;
      for (int i = 1; i < vf; i++)
	if (CONST_VECTOR_ELT (x, i) != CONST_VECTOR_ELT (x, 0))
	  return 0;
      return 1;
    }

  if (mode != HFmode && mode != SFmode && mode != DFmode)
    return 0;

  const REAL_VALUE_TYPE *r;

  if (x == CONST0_RTX (mode))
    return 128;
  if (x == CONST1_RTX (mode))
    return 242;

  r = CONST_DOUBLE_REAL_VALUE (x);

  if (real_identical (r, &dconstm1))
    return 243;

  if (real_identical (r, &dconsthalf))
    return 240;
  if (real_identical (r, &dconstm1))
    return 243;
  if (real_identical (r, &dconst2))
    return 244;
  if (real_identical (r, &dconst4))
    return 246;
  if (real_identical (r, &dconst1over2pi))
    return 248;
  if (!ext_gcn_constants_init)
    init_ext_gcn_constants ();
  real_value_negate (r);
  if (real_identical (r, &dconsthalf))
    return 241;
  if (real_identical (r, &dconst2))
    return 245;
  if (real_identical (r, &dconst4))
    return 247;

  /* FIXME: add 4, -4 and 1/(2*PI).  */

  return 0;
}

/* Return non-zero if X is a constant that can appear as an immediate operand.
   This is 0, 0.5, -0.5, 1, -1, 2, -2, 4,-4, 1/(2*pi)
   Or a vector of those.
   The value returned should be the encoding of this constant.  */

bool
gcn_fp_constant_p (rtx x, bool allow_vector)
{
  machine_mode mode = GET_MODE (x);
  int vf = VECTOR_MODE_P (mode) ? GET_MODE_NUNITS (mode) : 1;

  if (vf > 1)
    mode = GET_MODE_INNER (mode);

  if (vf > 1
      && (mode == HFmode || mode == SFmode || mode == DFmode)
      && allow_vector)
    {
      int n;
      if (GET_CODE (x) != CONST_VECTOR)
	return false;
      n = gcn_fp_constant_p (CONST_VECTOR_ELT (x, 0), false);
      if (!n)
	return false;
      for (int i = 1; i < vf; i++)
	if (CONST_VECTOR_ELT (x, i) != CONST_VECTOR_ELT (x, 0))
	  return false;
      return true;
    }
  if (mode != HFmode && mode != SFmode && mode != DFmode)
    return false;

  if (gcn_inline_fp_constant_p (x, false))
    return true;
  /* FIXME: It is not clear how 32bit immediates are interpreted here.  */
  return (mode != DFmode);
}

/* Return true if X is a constant representable as an inline immediate
   constant in a 32-bit instruction encoding.  */

bool
gcn_inline_constant_p (rtx x)
{
  if (GET_CODE (x) == CONST_INT)
    return INTVAL (x) >= -16 && INTVAL (x) <= 64;
  if (GET_CODE (x) == CONST_DOUBLE)
    return gcn_inline_fp_constant_p (x, false);
  if (GET_CODE (x) == CONST_VECTOR)
    {
      int n;
      if (!vgpr_vector_mode_p (GET_MODE (x)))
	return false;
      n = gcn_inline_constant_p (CONST_VECTOR_ELT (x, 0));
      if (!n)
	return false;
      for (int i = 1; i < 64; i++)
	if (CONST_VECTOR_ELT (x, i) != CONST_VECTOR_ELT (x, 0))
	  return false;
      return 1;
    }
  return false;
}

/* Return true if X is a constant representable as an immediate constant
   in a 32 or 64-bit instruction encoding.  */

bool
gcn_constant_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      return true;

    case CONST_DOUBLE:
      return gcn_fp_constant_p (x, false);

    case CONST_VECTOR:
      {
	int n;
	if (!vgpr_vector_mode_p (GET_MODE (x)))
	  return false;
	n = gcn_constant_p (CONST_VECTOR_ELT (x, 0));
	if (!n)
	  return false;
	for (int i = 1; i < 64; i++)
	  if (CONST_VECTOR_ELT (x, i) != CONST_VECTOR_ELT (x, 0))
	    return false;
	return true;
      }

    case SYMBOL_REF:
    case LABEL_REF:
      return true;

    default:
      ;
    }

  return false;
}

/* Return true if X is a constant representable as two inline immediate
   constants in a 64-bit instruction that is split into two 32-bit
   instructions.
   When MIXED is set, the low-part is permitted to use the full 32-bits.  */

bool
gcn_inline_constant64_p (rtx x, bool mixed)
{
  if (GET_CODE (x) == CONST_VECTOR)
    {
      if (!vgpr_vector_mode_p (GET_MODE (x)))
	return false;
      if (!gcn_inline_constant64_p (CONST_VECTOR_ELT (x, 0), mixed))
	return false;
      for (int i = 1; i < 64; i++)
	if (CONST_VECTOR_ELT (x, i) != CONST_VECTOR_ELT (x, 0))
	  return false;

      return true;
    }

  if (GET_CODE (x) != CONST_INT)
    return false;

  rtx val_lo = gcn_operand_part (DImode, x, 0);
  rtx val_hi = gcn_operand_part (DImode, x, 1);
  return ((mixed || gcn_inline_constant_p (val_lo))
	  && gcn_inline_constant_p (val_hi));
}

/* Return true if X is a constant representable as an immediate constant
   in a 32 or 64-bit instruction encoding where the hardware will
   extend the immediate to 64-bits.  */

bool
gcn_constant64_p (rtx x)
{
  if (!gcn_constant_p (x))
    return false;

  if (GET_CODE (x) != CONST_INT)
    return true;

  /* Negative numbers are only allowed if they can be encoded within src0,
     because the 32-bit immediates do not get sign-extended.
     Unsigned numbers must not be encodable as 32-bit -1..-16, because the
     assembler will use a src0 inline immediate and that will get
     sign-extended.  */
  HOST_WIDE_INT val = INTVAL (x);
  return (((val & 0xffffffff) == val	/* Positive 32-bit.  */
	   && (val & 0xfffffff0) != 0xfffffff0)	/* Not -1..-16.  */
	  || gcn_inline_constant_p (x));	/* Src0.  */
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.

   Returns true if X is a legitimate constant for a MODE immediate operand.  */

bool
gcn_legitimate_constant_p (machine_mode, rtx x)
{
  return gcn_constant_p (x);
}

/* Return true if X is a CONST_VECTOR of single constant.  */

static bool
single_cst_vector_p (rtx x)
{
  if (GET_CODE (x) != CONST_VECTOR)
    return false;
  for (int i = 1; i < 64; i++)
    if (CONST_VECTOR_ELT (x, i) != CONST_VECTOR_ELT (x, 0))
      return false;
  return true;
}

/* Create a CONST_VECTOR of duplicated value A.  */

rtx
gcn_vec_constant (machine_mode mode, int a)
{
  /*if (!a)
    return CONST0_RTX (mode);
  if (a == -1)
    return CONSTM1_RTX (mode);
  if (a == 1)
    return CONST1_RTX (mode);
  if (a == 2)
    return CONST2_RTX (mode);*/

  int units = GET_MODE_NUNITS (mode);
  machine_mode innermode = GET_MODE_INNER (mode);

  rtx tem;
  if (FLOAT_MODE_P (innermode))
    {
      REAL_VALUE_TYPE rv;
      real_from_integer (&rv, VOIDmode, a, SIGNED);
      tem = const_double_from_real_value (rv, innermode);
    }
  else
    tem = gen_int_mode (a, innermode);

  rtvec v = rtvec_alloc (units);
  for (int i = 0; i < units; ++i)
    RTVEC_ELT (v, i) = tem;

  return gen_rtx_CONST_VECTOR (mode, v);
}

/* Create a CONST_VECTOR of duplicated value A.  */

rtx
gcn_vec_constant (machine_mode mode, rtx a)
{
  int units = GET_MODE_NUNITS (mode);
  rtvec v = rtvec_alloc (units);

  for (int i = 0; i < units; ++i)
    RTVEC_ELT (v, i) = a;

  return gen_rtx_CONST_VECTOR (mode, v);
}

/* Create an undefined vector value, used where an insn operand is
   optional.  */

rtx
gcn_gen_undef (machine_mode mode)
{
  return gen_rtx_UNSPEC (mode, gen_rtvec (1, const0_rtx), UNSPEC_VECTOR);
}

/* }}}  */
/* {{{ Utility functions.  */

/*  Generalised accessor functions for instruction patterns.
    The machine desription '@' prefix does something similar, but as of
    GCC 10 is incompatible with define_subst, and anyway it doesn't
    auto-handle the exec feature.

    Four macros are provided; each function only needs one:

    GEN_VN         - create accessor functions for all sizes of one mode
    GEN_VNM        - create accessor functions for all sizes of all modes
    GEN_VN_NOEXEC  - for insns without "_exec" variants
    GEN_VNM_NOEXEC - likewise

    E.g.  add<mode>3
      GEN_VNM (add, 3, A(rtx dest, rtx s1, rtx s2), A(dest, s1, s2)

      gen_addvNsi3 (dst, a, b)
        -> calls gen_addv64si3, or gen_addv32si3, etc.

      gen_addvNm3 (dst, a, b)
        -> calls gen_addv64qi3, or gen_addv2di3, etc.

    The mode is determined from the first parameter, which must be called
    "dest" (or else the macro doesn't work).

    Each function has two optional parameters at the end: merge_src and exec.
    If exec is non-null, the function will call the "_exec" variant of the
    insn.  If exec is non-null but merge_src is null then an undef unspec
    will be created.

    E.g. cont.
      gen_addvNsi3 (v64sidst, a, b, oldval, exec)
        -> calls gen_addv64si3_exec (v64sidst, a, b, oldval, exec)

      gen_addvNm3 (v2qidst, a, b, NULL, exec)
        -> calls gen_addv2qi3_exec (v2qidst, a, b,
                                    gcn_gen_undef (V2QImode), exec)
   */

#define A(...) __VA_ARGS__
#define GEN_VN_NOEXEC(PREFIX, SUFFIX, PARAMS, ARGS) \
static rtx \
gen_##PREFIX##vN##SUFFIX (PARAMS) \
{ \
  machine_mode mode = GET_MODE (dest); \
  int n = GET_MODE_NUNITS (mode); \
  \
  switch (n) \
    { \
    case 2: return gen_##PREFIX##v2##SUFFIX (ARGS); \
    case 4: return gen_##PREFIX##v4##SUFFIX (ARGS); \
    case 8: return gen_##PREFIX##v8##SUFFIX (ARGS); \
    case 16: return gen_##PREFIX##v16##SUFFIX (ARGS); \
    case 32: return gen_##PREFIX##v32##SUFFIX (ARGS); \
    case 64: return gen_##PREFIX##v64##SUFFIX (ARGS); \
    } \
  \
  gcc_unreachable (); \
  return NULL_RTX; \
}

#define GEN_VNM_NOEXEC(PREFIX, SUFFIX, PARAMS, ARGS) \
GEN_VN_NOEXEC (PREFIX, qi##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN_NOEXEC (PREFIX, hi##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN_NOEXEC (PREFIX, hf##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN_NOEXEC (PREFIX, si##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN_NOEXEC (PREFIX, sf##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN_NOEXEC (PREFIX, di##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN_NOEXEC (PREFIX, df##SUFFIX, A(PARAMS), A(ARGS)) \
static rtx \
gen_##PREFIX##vNm##SUFFIX (PARAMS) \
{ \
  machine_mode mode = GET_MODE_INNER (GET_MODE (dest)); \
  \
  switch (mode) \
    { \
    case E_QImode: return gen_##PREFIX##vNqi##SUFFIX (ARGS); \
    case E_HImode: return gen_##PREFIX##vNhi##SUFFIX (ARGS); \
    case E_HFmode: return gen_##PREFIX##vNhf##SUFFIX (ARGS); \
    case E_SImode: return gen_##PREFIX##vNsi##SUFFIX (ARGS); \
    case E_SFmode: return gen_##PREFIX##vNsf##SUFFIX (ARGS); \
    case E_DImode: return gen_##PREFIX##vNdi##SUFFIX (ARGS); \
    case E_DFmode: return gen_##PREFIX##vNdf##SUFFIX (ARGS); \
    default: \
      break; \
    } \
  \
  gcc_unreachable (); \
  return NULL_RTX; \
}

#define GEN_VN(PREFIX, SUFFIX, PARAMS, ARGS) \
static rtx \
gen_##PREFIX##vN##SUFFIX (PARAMS, rtx merge_src=NULL, rtx exec=NULL) \
{ \
  machine_mode mode = GET_MODE (dest); \
  int n = GET_MODE_NUNITS (mode); \
  \
  if (exec && !merge_src) \
	merge_src = gcn_gen_undef (mode); \
      \
  if (exec) \
    switch (n) \
      { \
      case 2: return gen_##PREFIX##v2##SUFFIX##_exec (ARGS, merge_src, exec); \
      case 4: return gen_##PREFIX##v4##SUFFIX##_exec (ARGS, merge_src, exec); \
      case 8: return gen_##PREFIX##v8##SUFFIX##_exec (ARGS, merge_src, exec); \
      case 16: return gen_##PREFIX##v16##SUFFIX##_exec (ARGS, merge_src, exec); \
      case 32: return gen_##PREFIX##v32##SUFFIX##_exec (ARGS, merge_src, exec); \
      case 64: return gen_##PREFIX##v64##SUFFIX##_exec (ARGS, merge_src, exec); \
      } \
  else \
    switch (n) \
      { \
      case 2: return gen_##PREFIX##v2##SUFFIX (ARGS); \
      case 4: return gen_##PREFIX##v4##SUFFIX (ARGS); \
      case 8: return gen_##PREFIX##v8##SUFFIX (ARGS); \
      case 16: return gen_##PREFIX##v16##SUFFIX (ARGS); \
      case 32: return gen_##PREFIX##v32##SUFFIX (ARGS); \
      case 64: return gen_##PREFIX##v64##SUFFIX (ARGS); \
      } \
  \
  gcc_unreachable (); \
  return NULL_RTX; \
}

#define GEN_VNM(PREFIX, SUFFIX, PARAMS, ARGS) \
GEN_VN (PREFIX, qi##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN (PREFIX, hi##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN (PREFIX, hf##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN (PREFIX, si##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN (PREFIX, sf##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN (PREFIX, di##SUFFIX, A(PARAMS), A(ARGS)) \
GEN_VN (PREFIX, df##SUFFIX, A(PARAMS), A(ARGS)) \
USE_TI (GEN_VN (PREFIX, ti##SUFFIX, A(PARAMS), A(ARGS))) \
static rtx \
gen_##PREFIX##vNm##SUFFIX (PARAMS, rtx merge_src=NULL, rtx exec=NULL) \
{ \
  machine_mode mode = GET_MODE_INNER (GET_MODE (dest)); \
  \
  switch (mode) \
    { \
    case E_QImode: return gen_##PREFIX##vNqi##SUFFIX (ARGS, merge_src, exec); \
    case E_HImode: return gen_##PREFIX##vNhi##SUFFIX (ARGS, merge_src, exec); \
    case E_HFmode: return gen_##PREFIX##vNhf##SUFFIX (ARGS, merge_src, exec); \
    case E_SImode: return gen_##PREFIX##vNsi##SUFFIX (ARGS, merge_src, exec); \
    case E_SFmode: return gen_##PREFIX##vNsf##SUFFIX (ARGS, merge_src, exec); \
    case E_DImode: return gen_##PREFIX##vNdi##SUFFIX (ARGS, merge_src, exec); \
    case E_DFmode: return gen_##PREFIX##vNdf##SUFFIX (ARGS, merge_src, exec); \
    case E_TImode: \
	USE_TI (return gen_##PREFIX##vNti##SUFFIX (ARGS, merge_src, exec);) \
    default: \
      break; \
    } \
  \
  gcc_unreachable (); \
  return NULL_RTX; \
}

/* These have TImode support.  */
#define USE_TI(ARGS) ARGS
GEN_VNM (mov,, A(rtx dest, rtx src), A(dest, src))
GEN_VNM (vec_duplicate,, A(rtx dest, rtx src), A(dest, src))

/* These do not have TImode support.  */
#undef USE_TI
#define USE_TI(ARGS)
GEN_VNM (add,3, A(rtx dest, rtx src1, rtx src2), A(dest, src1, src2))
GEN_VN (add,si3_dup, A(rtx dest, rtx src1, rtx src2), A(dest, src1, src2))
GEN_VN (add,si3_vcc_dup, A(rtx dest, rtx src1, rtx src2, rtx vcc),
	A(dest, src1, src2, vcc))
GEN_VN (add,di3_sext_dup2, A(rtx dest, rtx src1, rtx src2), A(dest, src1, src2))
GEN_VN (add,di3_vcc_zext_dup, A(rtx dest, rtx src1, rtx src2, rtx vcc),
	A(dest, src1, src2, vcc))
GEN_VN (add,di3_zext_dup2, A(rtx dest, rtx src1, rtx src2), A(dest, src1, src2))
GEN_VN (add,di3_vcc_zext_dup2, A(rtx dest, rtx src1, rtx src2, rtx vcc),
	A(dest, src1, src2, vcc))
GEN_VN (addc,si3, A(rtx dest, rtx src1, rtx src2, rtx vccout, rtx vccin),
	A(dest, src1, src2, vccout, vccin))
GEN_VN (and,si3, A(rtx dest, rtx src1, rtx src2), A(dest, src1, src2))
GEN_VN (ashl,si3, A(rtx dest, rtx src, rtx shift), A(dest, src, shift))
GEN_VNM_NOEXEC (ds_bpermute,, A(rtx dest, rtx addr, rtx src, rtx exec),
		A(dest, addr, src, exec))
GEN_VNM (gather,_expr, A(rtx dest, rtx addr, rtx as, rtx vol),
	 A(dest, addr, as, vol))
GEN_VN (mul,si3_dup, A(rtx dest, rtx src1, rtx src2), A(dest, src1, src2))
GEN_VN (sub,si3, A(rtx dest, rtx src1, rtx src2), A(dest, src1, src2))
GEN_VN_NOEXEC (vec_series,si, A(rtx dest, rtx x, rtx c), A(dest, x, c))

#undef USE_TI
#undef GEN_VNM
#undef GEN_VN
#undef GET_VN_FN
#undef A

/* Return true if OP is a PARALLEL of CONST_INTs that form a linear
   series with step STEP.  */

bool
gcn_stepped_zero_int_parallel_p (rtx op, int step)
{
  if (GET_CODE (op) != PARALLEL || !CONST_INT_P (XVECEXP (op, 0, 0)))
    return false;

  unsigned HOST_WIDE_INT base = 0;
  for (int i = 0; i < XVECLEN (op, 0); ++i)
    if (!CONST_INT_P (XVECEXP (op, 0, i))
	|| UINTVAL (XVECEXP (op, 0, i)) != base + i * step)
      return false;

  return true;
}

/* }}}  */
/* {{{ Addresses, pointers and moves.  */

/* Return true is REG is a valid place to store a pointer,
   for instructions that require an SGPR.
   FIXME rename. */

static bool
gcn_address_register_p (rtx reg, machine_mode mode, bool strict)
{
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return false;

  if (GET_MODE (reg) != mode)
    return false;

  int regno = REGNO (reg);

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict)
	return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }

  return (SGPR_REGNO_P (regno) || regno == M0_REG
	  || regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM);
}

/* Return true is REG is a valid place to store a pointer,
   for instructions that require a VGPR.  */

static bool
gcn_vec_address_register_p (rtx reg, machine_mode mode, bool strict)
{
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return false;

  if (GET_MODE (reg) != mode)
    return false;

  int regno = REGNO (reg);

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict)
	return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }

  return VGPR_REGNO_P (regno);
}

/* Return true if X would be valid inside a MEM using the Flat address
   space.  */

bool
gcn_flat_address_p (rtx x, machine_mode mode)
{
  bool vec_mode = (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
		   || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT);

  if (vec_mode && gcn_address_register_p (x, DImode, false))
    return true;

  if (!vec_mode && gcn_vec_address_register_p (x, DImode, false))
    return true;

  if (GET_CODE (x) == PLUS
      && gcn_vec_address_register_p (XEXP (x, 0), DImode, false)
      && CONST_INT_P (XEXP (x, 1)))
    return true;

  return false;
}

/* Return true if X would be valid inside a MEM using the Scalar Flat
   address space.  */

bool
gcn_scalar_flat_address_p (rtx x)
{
  if (gcn_address_register_p (x, DImode, false))
    return true;

  if (GET_CODE (x) == PLUS
      && gcn_address_register_p (XEXP (x, 0), DImode, false)
      && CONST_INT_P (XEXP (x, 1)))
    return true;

  return false;
}

/* Return true if MEM X would be valid for the Scalar Flat address space.  */

bool
gcn_scalar_flat_mem_p (rtx x)
{
  if (!MEM_P (x))
    return false;

  if (GET_MODE_SIZE (GET_MODE (x)) < 4)
    return false;

  return gcn_scalar_flat_address_p (XEXP (x, 0));
}

/* Return true if X would be valid inside a MEM using the LDS or GDS
   address spaces.  */

bool
gcn_ds_address_p (rtx x)
{
  if (gcn_vec_address_register_p (x, SImode, false))
    return true;

  if (GET_CODE (x) == PLUS
      && gcn_vec_address_register_p (XEXP (x, 0), SImode, false)
      && CONST_INT_P (XEXP (x, 1)))
    return true;

  return false;
}

/* Return true if ADDR would be valid inside a MEM using the Global
   address space.  */

bool
gcn_global_address_p (rtx addr)
{
  if (gcn_address_register_p (addr, DImode, false)
      || gcn_vec_address_register_p (addr, DImode, false))
    return true;

  if (GET_CODE (addr) == PLUS)
    {
      rtx base = XEXP (addr, 0);
      rtx offset = XEXP (addr, 1);
      int offsetbits = (TARGET_11BIT_GLOBAL_OFFSET ? 11 : 12);
      bool immediate_p = (CONST_INT_P (offset)
			  && INTVAL (offset) >= -(1 << offsetbits)
			  && INTVAL (offset) < (1 << offsetbits));

      if ((gcn_address_register_p (base, DImode, false)
	   || gcn_vec_address_register_p (base, DImode, false))
	  && immediate_p)
	/* SGPR + CONST or VGPR + CONST  */
	return true;

      if (gcn_address_register_p (base, DImode, false)
	  && gcn_vgpr_register_operand (offset, SImode))
	/* SPGR + VGPR  */
	return true;

      if (GET_CODE (base) == PLUS
	  && gcn_address_register_p (XEXP (base, 0), DImode, false)
	  && gcn_vgpr_register_operand (XEXP (base, 1), SImode)
	  && immediate_p)
	/* (SGPR + VGPR) + CONST  */
	return true;
    }

  return false;
}

/* Implement TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P.

   Recognizes RTL expressions that are valid memory addresses for an
   instruction.  The MODE argument is the machine mode for the MEM
   expression that wants to use this address.

   It only recognizes address in canonical form.  LEGITIMIZE_ADDRESS should
   convert common non-canonical forms to canonical form so that they will
   be recognized.  */

static bool
gcn_addr_space_legitimate_address_p (machine_mode mode, rtx x, bool strict,
				     addr_space_t as, code_helper = ERROR_MARK)
{
  if (AS_SCALAR_FLAT_P (as))
    {
      if (mode == QImode || mode == HImode)
	return 0;

      switch (GET_CODE (x))
	{
	case REG:
	  return gcn_address_register_p (x, DImode, strict);
	/* Addresses are in the form BASE+OFFSET
	   OFFSET is either 20bit unsigned immediate, SGPR or M0.
	   Writes and atomics do not accept SGPR.  */
	case PLUS:
	  {
	    rtx x0 = XEXP (x, 0);
	    rtx x1 = XEXP (x, 1);
	    if (!gcn_address_register_p (x0, DImode, strict))
	      return false;
	    /* FIXME: This is disabled because of the mode mismatch between
	       SImode (for the address or m0 register) and the DImode PLUS.
	       We'll need a zero_extend or similar.

	    if (gcn_m0_register_p (x1, SImode, strict)
		|| gcn_address_register_p (x1, SImode, strict))
	      return true;
	    else*/
	    if (GET_CODE (x1) == CONST_INT)
	      {
		if (INTVAL (x1) >= 0 && INTVAL (x1) < (1 << 20)
		    /* The low bits of the offset are ignored, even when
		       they're meant to realign the pointer.  */
		    && !(INTVAL (x1) & 0x3))
		  return true;
	      }
	    return false;
	  }

	default:
	  break;
	}
    }
  else if (AS_SCRATCH_P (as))
    return gcn_address_register_p (x, SImode, strict);
  else if (AS_FLAT_P (as) || AS_FLAT_SCRATCH_P (as))
    {
      if (GET_CODE (x) == REG)
       return ((GET_MODE_CLASS (mode) == MODE_VECTOR_INT
		|| GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
	       ? gcn_address_register_p (x, DImode, strict)
	       : gcn_vec_address_register_p (x, DImode, strict));
      else
	{
	  if (GET_CODE (x) == PLUS)
	    {
	      rtx x1 = XEXP (x, 1);

	      if (VECTOR_MODE_P (mode)
		  ? !gcn_address_register_p (x, DImode, strict)
		  : !gcn_vec_address_register_p (x, DImode, strict))
		return false;

	      if (GET_CODE (x1) == CONST_INT)
		{
		  if (INTVAL (x1) >= 0 && INTVAL (x1) < (1 << 12)
		      /* The low bits of the offset are ignored, even when
		         they're meant to realign the pointer.  */
		      && !(INTVAL (x1) & 0x3))
		    return true;
		}
	    }
	  return false;
	}
    }
  else if (AS_GLOBAL_P (as))
    {
      if (GET_CODE (x) == REG)
       return (gcn_address_register_p (x, DImode, strict)
	       || (!VECTOR_MODE_P (mode)
		   && gcn_vec_address_register_p (x, DImode, strict)));
      else if (GET_CODE (x) == PLUS)
	{
	  rtx base = XEXP (x, 0);
	  rtx offset = XEXP (x, 1);

	  int offsetbits = (TARGET_11BIT_GLOBAL_OFFSET ? 11 : 12);
	  bool immediate_p = (GET_CODE (offset) == CONST_INT
			      /* Signed 12/13-bit immediate.  */
			      && INTVAL (offset) >= -(1 << offsetbits)
			      && INTVAL (offset) < (1 << offsetbits)
			      /* The low bits of the offset are ignored, even
			         when they're meant to realign the pointer.  */
			      && !(INTVAL (offset) & 0x3));

	  if (!VECTOR_MODE_P (mode))
	    {
	      if ((gcn_address_register_p (base, DImode, strict)
		   || gcn_vec_address_register_p (base, DImode, strict))
		  && immediate_p)
		/* SGPR + CONST or VGPR + CONST  */
		return true;

	      if (gcn_address_register_p (base, DImode, strict)
		  && gcn_vgpr_register_operand (offset, SImode))
		/* SGPR + VGPR  */
		return true;

	      if (GET_CODE (base) == PLUS
		  && gcn_address_register_p (XEXP (base, 0), DImode, strict)
		  && gcn_vgpr_register_operand (XEXP (base, 1), SImode)
		  && immediate_p)
		/* (SGPR + VGPR) + CONST  */
		return true;
	    }
	  else
	    {
	      if (gcn_address_register_p (base, DImode, strict)
		  && immediate_p)
		/* SGPR + CONST  */
		return true;
	    }
	}
      else
	return false;
    }
  else if (AS_ANY_DS_P (as))
    switch (GET_CODE (x))
      {
      case REG:
	return (VECTOR_MODE_P (mode)
		? gcn_address_register_p (x, SImode, strict)
		: gcn_vec_address_register_p (x, SImode, strict));
      /* Addresses are in the form BASE+OFFSET
	 OFFSET is either 20bit unsigned immediate, SGPR or M0.
	 Writes and atomics do not accept SGPR.  */
      case PLUS:
	{
	  rtx x0 = XEXP (x, 0);
	  rtx x1 = XEXP (x, 1);
	  if (!gcn_vec_address_register_p (x0, DImode, strict))
	    return false;
	  if (GET_CODE (x1) == REG)
	    {
	      if (GET_CODE (x1) != REG
		  || (REGNO (x1) <= FIRST_PSEUDO_REGISTER
		      && !gcn_ssrc_register_operand (x1, DImode)))
		return false;
	    }
	  else if (GET_CODE (x1) == CONST_VECTOR
		   && GET_CODE (CONST_VECTOR_ELT (x1, 0)) == CONST_INT
		   && single_cst_vector_p (x1))
	    {
	      x1 = CONST_VECTOR_ELT (x1, 0);
	      if (INTVAL (x1) >= 0 && INTVAL (x1) < (1 << 20))
		return true;
	    }
	  return false;
	}

      default:
	break;
      }
  else
    gcc_unreachable ();
  return false;
}

/* Implement TARGET_ADDR_SPACE_POINTER_MODE.

   Return the appropriate mode for a named address pointer.  */

static scalar_int_mode
gcn_addr_space_pointer_mode (addr_space_t addrspace)
{
  switch (addrspace)
    {
    case ADDR_SPACE_SCRATCH:
    case ADDR_SPACE_LDS:
    case ADDR_SPACE_GDS:
      return SImode;
    case ADDR_SPACE_DEFAULT:
    case ADDR_SPACE_FLAT:
    case ADDR_SPACE_FLAT_SCRATCH:
    case ADDR_SPACE_SCALAR_FLAT:
      return DImode;
    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_ADDR_SPACE_ADDRESS_MODE.

   Return the appropriate mode for a named address space address.  */

static scalar_int_mode
gcn_addr_space_address_mode (addr_space_t addrspace)
{
  return gcn_addr_space_pointer_mode (addrspace);
}

/* Implement TARGET_ADDR_SPACE_SUBSET_P.

   Determine if one named address space is a subset of another.  */

static bool
gcn_addr_space_subset_p (addr_space_t subset, addr_space_t superset)
{
  if (subset == superset)
    return true;
  /* FIXME is this true?  */
  if (AS_FLAT_P (superset) || AS_SCALAR_FLAT_P (superset))
    return true;
  return false;
}

/* Convert from one address space to another.  */

static rtx
gcn_addr_space_convert (rtx op, tree from_type, tree to_type)
{
  gcc_assert (POINTER_TYPE_P (from_type));
  gcc_assert (POINTER_TYPE_P (to_type));

  addr_space_t as_from = TYPE_ADDR_SPACE (TREE_TYPE (from_type));
  addr_space_t as_to = TYPE_ADDR_SPACE (TREE_TYPE (to_type));

  if (AS_LDS_P (as_from) && AS_FLAT_P (as_to))
    {
      /* The high bits of the QUEUE_PTR_ARG register are used by
	 GCN_BUILTIN_FIRST_CALL_THIS_THREAD_P, so mask them out.  */
      rtx queue_reg = gen_rtx_REG (DImode,
				   cfun->machine->args.reg[QUEUE_PTR_ARG]);
      rtx queue_ptr = gen_reg_rtx (DImode);
      emit_insn (gen_anddi3 (queue_ptr, queue_reg, GEN_INT (0xffffffffffff)));
      rtx group_seg_aperture_hi = gen_rtx_MEM (SImode,
				     gen_rtx_PLUS (DImode, queue_ptr,
						   gen_int_mode (64, SImode)));
      rtx tmp = gen_reg_rtx (DImode);

      emit_move_insn (gen_lowpart (SImode, tmp), op);
      emit_move_insn (gen_highpart_mode (SImode, DImode, tmp),
		      group_seg_aperture_hi);

      return tmp;
    }
  else if (as_from == as_to)
    return op;
  else
    gcc_unreachable ();
}

/* Implement TARGET_ADDR_SPACE_DEBUG.

   Return the dwarf address space class for each hardware address space.  */

static int
gcn_addr_space_debug (addr_space_t as)
{
  switch (as)
    {
      case ADDR_SPACE_DEFAULT:
      case ADDR_SPACE_FLAT:
      case ADDR_SPACE_SCALAR_FLAT:
      case ADDR_SPACE_FLAT_SCRATCH:
	return DW_ADDR_none;
      case ADDR_SPACE_GLOBAL:
	return 1;      // DW_ADDR_LLVM_global
      case ADDR_SPACE_LDS:
	return 3;      // DW_ADDR_LLVM_group
      case ADDR_SPACE_SCRATCH:
	return 4;      // DW_ADDR_LLVM_private
      case ADDR_SPACE_GDS:
	return 0x8000; // DW_ADDR_AMDGPU_region
    }
  gcc_unreachable ();
}


/* Implement REGNO_MODE_CODE_OK_FOR_BASE_P via gcn.h

   Retun true if REGNO is OK for memory adressing.  */

bool
gcn_regno_mode_code_ok_for_base_p (int regno,
				   machine_mode, addr_space_t as, int, int)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (reg_renumber)
	regno = reg_renumber[regno];
      else
	return true;
    }
  if (AS_FLAT_P (as))
    return (VGPR_REGNO_P (regno)
	    || regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM);
  else if (AS_SCALAR_FLAT_P (as))
    return (SGPR_REGNO_P (regno)
	    || regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM);
  else if (AS_GLOBAL_P (as))
    {
      return (SGPR_REGNO_P (regno)
	      || VGPR_REGNO_P (regno)
	      || regno == ARG_POINTER_REGNUM
	      || regno == FRAME_POINTER_REGNUM);
    }
  else
    /* For now.  */
    return false;
}

/* Implement MODE_CODE_BASE_REG_CLASS via gcn.h.

   Return a suitable register class for memory addressing.  */

reg_class
gcn_mode_code_base_reg_class (machine_mode mode, addr_space_t as, int oc,
			      int ic)
{
  switch (as)
    {
    case ADDR_SPACE_DEFAULT:
      return gcn_mode_code_base_reg_class (mode, DEFAULT_ADDR_SPACE, oc, ic);
    case ADDR_SPACE_SCALAR_FLAT:
    case ADDR_SPACE_SCRATCH:
      return SGPR_REGS;
      break;
    case ADDR_SPACE_FLAT:
    case ADDR_SPACE_FLAT_SCRATCH:
    case ADDR_SPACE_LDS:
    case ADDR_SPACE_GDS:
      return ((GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	       || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
	      ? SGPR_REGS : VGPR_REGS);
    case ADDR_SPACE_GLOBAL:
      return ((GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	       || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
	      ? SGPR_REGS : ALL_GPR_REGS);
    }
  gcc_unreachable ();
}

/* Implement REGNO_OK_FOR_INDEX_P via gcn.h.

   Return true if REGNO is OK for index of memory addressing.  */

bool
regno_ok_for_index_p (int regno)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (reg_renumber)
	regno = reg_renumber[regno];
      else
	return true;
    }
  return regno == M0_REG || VGPR_REGNO_P (regno);
}

/* Expand vector init of OP0 by VEC.
   Implements vec_init instruction pattern.  */

void
gcn_expand_vector_init (rtx op0, rtx vec)
{
  rtx val[64];
  machine_mode mode = GET_MODE (op0);
  int vf = GET_MODE_NUNITS (mode);
  machine_mode addrmode = VnMODE (vf, DImode);
  machine_mode offsetmode = VnMODE (vf, SImode);

  int64_t mem_mask = 0;
  int64_t item_mask[64];
  rtx ramp = gen_reg_rtx (offsetmode);
  rtx addr = gen_reg_rtx (addrmode);

  int unit_size = GET_MODE_SIZE (GET_MODE_INNER (GET_MODE (op0)));
  emit_insn (gen_mulvNsi3_dup (ramp, gen_rtx_REG (offsetmode, VGPR_REGNO (1)),
			       GEN_INT (unit_size)));

  bool simple_repeat = true;

  /* Expand nested vectors into one vector.  */
  int item_count = XVECLEN (vec, 0);
  for (int i = 0, j = 0; i < item_count; i++)
    {
      rtx item = XVECEXP (vec, 0, i);
      machine_mode mode = GET_MODE (item);
      int units = VECTOR_MODE_P (mode) ? GET_MODE_NUNITS (mode) : 1;
      item_mask[j] = (((uint64_t)-1)>>(64-units)) << j;

      if (simple_repeat && i != 0)
	simple_repeat = item == XVECEXP (vec, 0, i-1);

      /* If its a vector of values then copy them into the final location.  */
      if (GET_CODE (item) == CONST_VECTOR)
	{
	  for (int k = 0; k < units; k++)
	    val[j++] = XVECEXP (item, 0, k);
	  continue;
	}
      /* Otherwise, we have a scalar or an expression that expands...  */

      if (MEM_P (item))
	{
	  rtx base = XEXP (item, 0);
	  if (MEM_ADDR_SPACE (item) == DEFAULT_ADDR_SPACE
	      && REG_P (base))
	    {
	      /* We have a simple vector load.  We can put the addresses in
		 the vector, combine it with any other such MEMs, and load it
		 all with a single gather at the end.  */
	      int64_t mask = ((0xffffffffffffffffUL
			       >> (64-GET_MODE_NUNITS (mode)))
			      << j);
	      rtx exec = get_exec (mask);
	      emit_insn (gen_subvNsi3
			 (ramp, ramp,
			  gcn_vec_constant (offsetmode, j*unit_size),
			  ramp, exec));
	      emit_insn (gen_addvNdi3_zext_dup2
			 (addr, ramp, base,
			  (mem_mask ? addr : gcn_gen_undef (addrmode)),
			  exec));
	      mem_mask |= mask;
	    }
	  else
	    /* The MEM is non-trivial, so let's load it independently.  */
	    item = force_reg (mode, item);
	}
      else if (!CONST_INT_P (item) && !CONST_DOUBLE_P (item))
	/* The item may be a symbol_ref, or something else non-trivial.  */
	item = force_reg (mode, item);

      /* Duplicate the vector across each item.
	 It is either a smaller vector register that needs shifting,
	 or a MEM that needs loading.  */
      val[j] = item;
      j += units;
    }

  int64_t initialized_mask = 0;
  rtx prev = NULL;

  if (mem_mask)
    {
      emit_insn (gen_gathervNm_expr
		 (op0, gen_rtx_PLUS (addrmode, addr,
				     gen_rtx_VEC_DUPLICATE (addrmode,
							    const0_rtx)),
		  GEN_INT (DEFAULT_ADDR_SPACE), GEN_INT (0),
		  NULL, get_exec (mem_mask)));
      prev = op0;
      initialized_mask = mem_mask;
    }

  if (simple_repeat && item_count > 1 && !prev)
    {
      /* Special case for instances of {A, B, A, B, A, B, ....}, etc.  */
      rtx src = gen_rtx_SUBREG (mode, val[0], 0);
      rtx input_vf_mask = GEN_INT (GET_MODE_NUNITS (GET_MODE (val[0]))-1);

      rtx permutation = gen_reg_rtx (VnMODE (vf, SImode));
      emit_insn (gen_vec_seriesvNsi (permutation, GEN_INT (0), GEN_INT (1)));
      rtx mask_dup = gen_reg_rtx (VnMODE (vf, SImode));
      emit_insn (gen_vec_duplicatevNsi (mask_dup, input_vf_mask));
      emit_insn (gen_andvNsi3 (permutation, permutation, mask_dup));
      emit_insn (gen_ashlvNsi3 (permutation, permutation, GEN_INT (2)));
      emit_insn (gen_ds_bpermutevNm (op0, permutation, src, get_exec (mode)));
      return;
    }

  /* Write each value, elementwise, but coalesce matching values into one
     instruction, where possible.  */
  for (int i = 0; i < vf; i++)
    if (!(initialized_mask & ((int64_t) 1 << i)))
      {
	if (gcn_constant_p (val[i]))
	  emit_insn (gen_movvNm (op0, gcn_vec_constant (mode, val[i]), prev,
				 get_exec (item_mask[i])));
	else if (VECTOR_MODE_P (GET_MODE (val[i]))
		 && (GET_MODE_NUNITS (GET_MODE (val[i])) == vf
		     || i == 0))
	  emit_insn (gen_movvNm (op0, gen_rtx_SUBREG (mode, val[i], 0), prev,
				 get_exec (item_mask[i])));
	else if (VECTOR_MODE_P (GET_MODE (val[i])))
	  {
	    rtx permutation = gen_reg_rtx (VnMODE (vf, SImode));
	    emit_insn (gen_vec_seriesvNsi (permutation, GEN_INT (-i*4),
					   GEN_INT (4)));
	    rtx tmp = gen_reg_rtx (mode);
	    emit_insn (gen_ds_bpermutevNm (tmp, permutation,
					   gen_rtx_SUBREG (mode, val[i], 0),
					   get_exec (-1)));
	    emit_insn (gen_movvNm (op0, tmp, prev, get_exec (item_mask[i])));
	  }
	else
	  {
	    rtx reg = force_reg (GET_MODE_INNER (mode), val[i]);
	    emit_insn (gen_vec_duplicatevNm (op0, reg, prev,
					     get_exec (item_mask[i])));
	  }

	initialized_mask |= item_mask[i];
	prev = op0;
      }
}

/* Load vector constant where n-th lane contains BASE+n*VAL.  */

static rtx
strided_constant (machine_mode mode, int base, int val)
{
  rtx x = gen_reg_rtx (mode);
  emit_move_insn (x, gcn_vec_constant (mode, base));
  emit_insn (gen_addvNm3 (x, x, gcn_vec_constant (mode, val * 32),
			  x, get_exec (0xffffffff00000000)));
  emit_insn (gen_addvNm3 (x, x, gcn_vec_constant (mode, val * 16),
			  x, get_exec (0xffff0000ffff0000)));
  emit_insn (gen_addvNm3 (x, x, gcn_vec_constant (mode, val * 8),
			  x, get_exec (0xff00ff00ff00ff00)));
  emit_insn (gen_addvNm3 (x, x, gcn_vec_constant (mode, val * 4),
			  x, get_exec (0xf0f0f0f0f0f0f0f0)));
  emit_insn (gen_addvNm3 (x, x, gcn_vec_constant (mode, val * 2),
			  x, get_exec (0xcccccccccccccccc)));
  emit_insn (gen_addvNm3 (x, x, gcn_vec_constant (mode, val * 1),
			  x, get_exec (0xaaaaaaaaaaaaaaaa)));
  return x;
}

/* Implement TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS.  */

static rtx
gcn_addr_space_legitimize_address (rtx x, rtx old, machine_mode mode,
				   addr_space_t as)
{
  switch (as)
    {
    case ADDR_SPACE_DEFAULT:
      return gcn_addr_space_legitimize_address (x, old, mode,
						DEFAULT_ADDR_SPACE);
    case ADDR_SPACE_SCALAR_FLAT:
    case ADDR_SPACE_SCRATCH:
      /* Instructions working on vectors need the address to be in
         a register.  */
      if (vgpr_vector_mode_p (mode))
	return force_reg (GET_MODE (x), x);

      return x;
    case ADDR_SPACE_FLAT:
    case ADDR_SPACE_FLAT_SCRATCH:
    case ADDR_SPACE_GLOBAL:
      return x;
    case ADDR_SPACE_LDS:
    case ADDR_SPACE_GDS:
      /* FIXME: LDS support offsets, handle them!.  */
      if (vgpr_vector_mode_p (mode)
	  && GET_MODE_INNER (GET_MODE (x)) != SImode)
	{
	  machine_mode simode = VnMODE (GET_MODE_NUNITS (mode), SImode);
	  rtx addrs = gen_reg_rtx (simode);
	  rtx base = force_reg (SImode, x);
	  rtx offsets = strided_constant (simode, 0,
					  GET_MODE_UNIT_SIZE (mode));

	  emit_insn (gen_vec_duplicatevNsi (addrs, base));
	  emit_insn (gen_addvNsi3 (addrs, offsets, addrs));
	  return addrs;
	}
      return x;
    }
  gcc_unreachable ();
}

/* Convert a (mem:<MODE> (reg:DI)) to (mem:<MODE> (reg:VnDI)) with the
   proper vector of stepped addresses.

   MEM will be a DImode address of a vector in an SGPR.
   TMP will be a VnDImode VGPR pair or (scratch:VnDI).  */

rtx
gcn_expand_scalar_to_vector_address (machine_mode mode, rtx exec, rtx mem,
				     rtx tmp)
{
  machine_mode pmode = VnMODE (GET_MODE_NUNITS (mode), DImode);
  machine_mode offmode = VnMODE (GET_MODE_NUNITS (mode), SImode);
  gcc_assert (MEM_P (mem));
  rtx mem_base = XEXP (mem, 0);
  rtx mem_index = NULL_RTX;

  if (GET_CODE (mem_base) == PLUS)
    {
      mem_index = XEXP (mem_base, 1);
      mem_base = XEXP (mem_base, 0);
    }

  /* RF and RM base registers for vector modes should be always an SGPR.  */
  gcc_assert (SGPR_REGNO_P (REGNO (mem_base))
	      || REGNO (mem_base) >= FIRST_PSEUDO_REGISTER);

  machine_mode inner = GET_MODE_INNER (mode);
  int shift = exact_log2 (GET_MODE_SIZE (inner));
  rtx ramp = gen_rtx_REG (offmode, VGPR_REGNO (1));
  rtx new_base = NULL_RTX;
  addr_space_t as = MEM_ADDR_SPACE (mem);

  rtx tmplo = (REG_P (tmp)
	       ? gcn_operand_part (pmode, tmp, 0)
	       : gen_reg_rtx (offmode));

  /* tmplo[:] = ramp[:] << shift  */
  emit_insn (gen_ashlvNsi3 (tmplo, ramp,
			    gen_int_mode (shift, SImode),
			    NULL, exec));

  if (AS_FLAT_P (as))
    {
      rtx vcc = gen_rtx_REG (DImode, CC_SAVE_REG);

      if (REG_P (tmp))
	{
	  rtx mem_base_lo = gcn_operand_part (DImode, mem_base, 0);
	  rtx mem_base_hi = gcn_operand_part (DImode, mem_base, 1);
	  rtx tmphi = gcn_operand_part (pmode, tmp, 1);

	  /* tmphi[:] = mem_base_hi  */
	  emit_insn (gen_vec_duplicatevNsi (tmphi, mem_base_hi, NULL, exec));

	  /* tmp[:] += zext (mem_base)  */
	  if (exec)
	    {
	      emit_insn (gen_addvNsi3_vcc_dup (tmplo, mem_base_lo, tmplo,
					       vcc, NULL, exec));
	      emit_insn (gen_addcvNsi3 (tmphi, tmphi, const0_rtx,
				        vcc, vcc, NULL, exec));
	    }
	  else
	    emit_insn (gen_addvNdi3_vcc_zext_dup (tmp, mem_base_lo, tmp, vcc));
	}
      else
	{
	  tmp = gen_reg_rtx (pmode);
	  emit_insn (gen_addvNdi3_vcc_zext_dup2 (tmp, tmplo, mem_base, vcc,
						 NULL, exec));
	}

      new_base = tmp;
    }
  else if (AS_ANY_DS_P (as))
    {
      emit_insn (gen_addvNsi3_dup (tmplo, tmplo, mem_base, NULL, exec));
      new_base = tmplo;
    }
  else
    {
      mem_base = gen_rtx_VEC_DUPLICATE (pmode, mem_base);
      new_base = gen_rtx_PLUS (pmode, mem_base,
			       gen_rtx_SIGN_EXTEND (pmode, tmplo));
    }

  return gen_rtx_PLUS (GET_MODE (new_base), new_base,
		       gen_rtx_VEC_DUPLICATE (GET_MODE (new_base),
					      (mem_index ? mem_index
					       : const0_rtx)));
}

/* Convert a BASE address, a vector of OFFSETS, and a SCALE, to addresses
   suitable for the given address space.  This is indented for use in
   gather/scatter patterns.

   The offsets may be signed or unsigned, according to UNSIGNED_P.
   If EXEC is set then _exec patterns will be used, otherwise plain.

   Return values.
     ADDR_SPACE_FLAT   - return VnDImode vector of absolute addresses.
     ADDR_SPACE_GLOBAL - return VnSImode vector of offsets.  */

rtx
gcn_expand_scaled_offsets (addr_space_t as, rtx base, rtx offsets, rtx scale,
			   bool unsigned_p, rtx exec)
{
  int vf = GET_MODE_NUNITS (GET_MODE (offsets));
  rtx tmpsi = gen_reg_rtx (VnMODE (vf, SImode));
  rtx tmpdi = gen_reg_rtx (VnMODE (vf, DImode));

  if (CONST_INT_P (scale)
      && INTVAL (scale) > 0
      && exact_log2 (INTVAL (scale)) >= 0)
    emit_insn (gen_ashlvNsi3 (tmpsi, offsets,
			      GEN_INT (exact_log2 (INTVAL (scale))),
			      NULL, exec));
  else
     emit_insn (gen_mulvNsi3_dup (tmpsi, offsets, scale, NULL, exec));

  /* "Global" instructions do not support negative register offsets.  */
  if (as == ADDR_SPACE_FLAT || !unsigned_p)
    {
      if (unsigned_p)
	 emit_insn (gen_addvNdi3_zext_dup2 (tmpdi, tmpsi, base, NULL, exec));
      else
	 emit_insn (gen_addvNdi3_sext_dup2 (tmpdi, tmpsi, base, NULL, exec));
      return tmpdi;
    }
  else if (as == ADDR_SPACE_GLOBAL)
    return tmpsi;

  gcc_unreachable ();
}

/* Return true if move from OP0 to OP1 is known to be executed in vector
   unit.  */

bool
gcn_vgpr_move_p (rtx op0, rtx op1)
{
  if (MEM_P (op0) && AS_SCALAR_FLAT_P (MEM_ADDR_SPACE (op0)))
    return true;
  if (MEM_P (op1) && AS_SCALAR_FLAT_P (MEM_ADDR_SPACE (op1)))
    return true;
  return ((REG_P (op0) && VGPR_REGNO_P (REGNO (op0)))
	  || (REG_P (op1) && VGPR_REGNO_P (REGNO (op1)))
	  || vgpr_vector_mode_p (GET_MODE (op0)));
}

/* Return true if move from OP0 to OP1 is known to be executed in scalar
   unit.  Used in the machine description.  */

bool
gcn_sgpr_move_p (rtx op0, rtx op1)
{
  if (MEM_P (op0) && AS_SCALAR_FLAT_P (MEM_ADDR_SPACE (op0)))
    return true;
  if (MEM_P (op1) && AS_SCALAR_FLAT_P (MEM_ADDR_SPACE (op1)))
    return true;
  if (!REG_P (op0)
      || REGNO (op0) >= FIRST_PSEUDO_REGISTER
      || VGPR_REGNO_P (REGNO (op0))
      || AVGPR_REGNO_P (REGNO (op0)))
    return false;
  if (REG_P (op1)
      && REGNO (op1) < FIRST_PSEUDO_REGISTER
      && !VGPR_REGNO_P (REGNO (op1))
      && !AVGPR_REGNO_P (REGNO (op1)))
    return true;
  return immediate_operand (op1, VOIDmode) || memory_operand (op1, VOIDmode);
}

/* Implement TARGET_SECONDARY_RELOAD.

   The address space determines which registers can be used for loads and
   stores.  */

static reg_class_t
gcn_secondary_reload (bool in_p, rtx x, reg_class_t rclass,
		      machine_mode reload_mode, secondary_reload_info *sri)
{
  reg_class_t result = NO_REGS;
  bool spilled_pseudo =
    (REG_P (x) || GET_CODE (x) == SUBREG) && true_regnum (x) == -1;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "gcn_secondary_reload: ");
      dump_value_slim (dump_file, x, 1);
      fprintf (dump_file, " %s %s:%s", (in_p ? "->" : "<-"),
	       reg_class_names[rclass], GET_MODE_NAME (reload_mode));
      if (REG_P (x) || GET_CODE (x) == SUBREG)
	fprintf (dump_file, " (true regnum: %d \"%s\")", true_regnum (x),
		 (true_regnum (x) >= 0
		  && true_regnum (x) < FIRST_PSEUDO_REGISTER
		  ? reg_names[true_regnum (x)]
		  : (spilled_pseudo ? "stack spill" : "??")));
      fprintf (dump_file, "\n");
    }

  /* Some callers don't use or initialize icode.  */
  sri->icode = CODE_FOR_nothing;

  if (MEM_P (x) || spilled_pseudo)
    {
      addr_space_t as = DEFAULT_ADDR_SPACE;

      /* If we have a spilled pseudo, we can't find the address space
	 directly, but we know it's in ADDR_SPACE_FLAT space for GCN3 or
	 ADDR_SPACE_GLOBAL for GCN5.  */
      if (MEM_P (x))
	as = MEM_ADDR_SPACE (x);

      if (as == ADDR_SPACE_DEFAULT)
	as = DEFAULT_ADDR_SPACE;

      switch (as)
	{
	case ADDR_SPACE_SCALAR_FLAT:
	  result =
	    ((!MEM_P (x) || rclass == SGPR_REGS) ? NO_REGS : SGPR_REGS);
	  break;
	case ADDR_SPACE_FLAT:
	case ADDR_SPACE_FLAT_SCRATCH:
	case ADDR_SPACE_GLOBAL:
	  if (GET_MODE_CLASS (reload_mode) == MODE_VECTOR_INT
	      || GET_MODE_CLASS (reload_mode) == MODE_VECTOR_FLOAT)
	    {
	      sri->icode = code_for_mov_sgprbase (reload_mode);
	      break;
	    }
	  /* Fallthrough.  */
	case ADDR_SPACE_LDS:
	case ADDR_SPACE_GDS:
	case ADDR_SPACE_SCRATCH:
	  result = (rclass == VGPR_REGS ? NO_REGS : VGPR_REGS);
	  break;
	}

      /* CDNA1 doesn't have an instruction for going between the accumulator
	 registers and memory.  Go via a VGPR in this case.  */
      if (!TARGET_AVGPR_MEMOPS
	  && rclass == AVGPR_REGS && result != VGPR_REGS)
	result = VGPR_REGS;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "   <= %s (icode: %s)\n", reg_class_names[result],
	     get_insn_name (sri->icode));

  return result;
}

/* Update register usage after having seen the compiler flags and kernel
   attributes.  We typically want to fix registers that contain values
   set by the HSA runtime.  */

static void
gcn_conditional_register_usage (void)
{
  /* Some architectures have a register allocation granularity that does not
     permit use of the full register count.  */
  for (int i = 256 - (256 % TARGET_VGPR_GRANULARITY);
       i < 256;
       i++)
    fixed_regs[VGPR_REGNO (i)] = call_used_regs[VGPR_REGNO (i)] = 1;

  if (!cfun || !cfun->machine)
    return;

  if (cfun->machine->normal_function)
    {
      /* Restrict the set of SGPRs, VGPRs and AVGPRs used by non-kernel
	 functions.  */
      for (int i = SGPR_REGNO (MAX_NORMAL_SGPR_COUNT);
	   i <= LAST_SGPR_REG; i++)
	fixed_regs[i] = 1, call_used_regs[i] = 1;

      for (int i = VGPR_REGNO (MAX_NORMAL_VGPR_COUNT);
	   i <= LAST_VGPR_REG; i++)
	fixed_regs[i] = 1, call_used_regs[i] = 1;

      for (int i = AVGPR_REGNO (MAX_NORMAL_AVGPR_COUNT);
	   i <= LAST_AVGPR_REG; i++)
	fixed_regs[i] = 1, call_used_regs[i] = 1;
      return;
    }

  /* If the set of requested args is the default set, nothing more needs to
     be done.  */
  if (cfun->machine->args.requested == default_requested_args)
    return;

  /* Requesting a set of args different from the default violates the ABI.  */
  if (!leaf_function_p ())
    warning (0, "A non-default set of initial values has been requested, "
		"which violates the ABI");

  for (int i = SGPR_REGNO (0); i < SGPR_REGNO (14); i++)
    fixed_regs[i] = 0;

  /* Fix the runtime argument register containing values that may be
     needed later.  DISPATCH_PTR_ARG and FLAT_SCRATCH_* should not be
     needed after the prologue so there's no need to fix them.  */
  if (cfun->machine->args.reg[PRIVATE_SEGMENT_WAVE_OFFSET_ARG] >= 0)
    fixed_regs[cfun->machine->args.reg[PRIVATE_SEGMENT_WAVE_OFFSET_ARG]] = 1;
  if (cfun->machine->args.reg[PRIVATE_SEGMENT_BUFFER_ARG] >= 0)
    {
      /* The upper 32-bits of the 64-bit descriptor are not used, so allow
	the containing registers to be used for other purposes.  */
      fixed_regs[cfun->machine->args.reg[PRIVATE_SEGMENT_BUFFER_ARG]] = 1;
      fixed_regs[cfun->machine->args.reg[PRIVATE_SEGMENT_BUFFER_ARG] + 1] = 1;
    }
  if (cfun->machine->args.reg[KERNARG_SEGMENT_PTR_ARG] >= 0)
    {
      fixed_regs[cfun->machine->args.reg[KERNARG_SEGMENT_PTR_ARG]] = 1;
      fixed_regs[cfun->machine->args.reg[KERNARG_SEGMENT_PTR_ARG] + 1] = 1;
    }
  if (cfun->machine->args.reg[DISPATCH_PTR_ARG] >= 0)
    {
      fixed_regs[cfun->machine->args.reg[DISPATCH_PTR_ARG]] = 1;
      fixed_regs[cfun->machine->args.reg[DISPATCH_PTR_ARG] + 1] = 1;
    }
  if (cfun->machine->args.reg[QUEUE_PTR_ARG] >= 0)
    {
      fixed_regs[cfun->machine->args.reg[QUEUE_PTR_ARG]] = 1;
      fixed_regs[cfun->machine->args.reg[QUEUE_PTR_ARG] + 1] = 1;
    }
  if (cfun->machine->args.reg[WORKGROUP_ID_X_ARG] >= 0)
    fixed_regs[cfun->machine->args.reg[WORKGROUP_ID_X_ARG]] = 1;
  if (cfun->machine->args.reg[WORK_ITEM_ID_X_ARG] >= 0)
    fixed_regs[cfun->machine->args.reg[WORK_ITEM_ID_X_ARG]] = 1;
  if (cfun->machine->args.reg[WORK_ITEM_ID_Y_ARG] >= 0)
    fixed_regs[cfun->machine->args.reg[WORK_ITEM_ID_Y_ARG]] = 1;
  if (cfun->machine->args.reg[WORK_ITEM_ID_Z_ARG] >= 0)
    fixed_regs[cfun->machine->args.reg[WORK_ITEM_ID_Z_ARG]] = 1;
}

static bool
gcn_vgpr_equivalent_register_operand (rtx x, machine_mode mode)
{
  if (gcn_vgpr_register_operand (x, mode))
    return true;
  if (TARGET_AVGPR_MEMOPS && gcn_avgpr_register_operand (x, mode))
    return true;
  return false;
}

/* Determine if a load or store is valid, according to the register classes
   and address space.  Used primarily by the machine description to decide
   when to split a move into two steps.  */

bool
gcn_valid_move_p (machine_mode mode, rtx dest, rtx src)
{
  if (!MEM_P (dest) && !MEM_P (src))
    {
      if (gcn_vgpr_register_operand (src, mode)
	  && gcn_avgpr_register_operand (dest, mode))
	return true;
      if (gcn_avgpr_register_operand (src, mode)
	  && gcn_vgpr_register_operand (dest, mode))
	return true;
      if (TARGET_AVGPR_MEMOPS
	  && gcn_avgpr_register_operand (src, mode)
	  && gcn_avgpr_register_operand (dest, mode))
	return true;
      if (gcn_avgpr_hard_register_operand (src, mode)
	  || gcn_avgpr_hard_register_operand (dest, mode))
	return false;
      return true;
    }

  if (MEM_P (dest)
      && AS_FLAT_P (MEM_ADDR_SPACE (dest))
      && (gcn_flat_address_p (XEXP (dest, 0), mode)
	  || GET_CODE (XEXP (dest, 0)) == SYMBOL_REF
	  || GET_CODE (XEXP (dest, 0)) == LABEL_REF)
      && gcn_vgpr_equivalent_register_operand (src, mode))
    return true;
  else if (MEM_P (src)
	   && AS_FLAT_P (MEM_ADDR_SPACE (src))
	   && (gcn_flat_address_p (XEXP (src, 0), mode)
	       || GET_CODE (XEXP (src, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (src, 0)) == LABEL_REF)
	   && gcn_vgpr_equivalent_register_operand (dest, mode))
    return true;

  if (MEM_P (dest)
      && AS_GLOBAL_P (MEM_ADDR_SPACE (dest))
      && (gcn_global_address_p (XEXP (dest, 0))
	  || GET_CODE (XEXP (dest, 0)) == SYMBOL_REF
	  || GET_CODE (XEXP (dest, 0)) == LABEL_REF)
      && gcn_vgpr_equivalent_register_operand (src, mode))
    return true;
  else if (MEM_P (src)
	   && AS_GLOBAL_P (MEM_ADDR_SPACE (src))
	   && (gcn_global_address_p (XEXP (src, 0))
	       || GET_CODE (XEXP (src, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (src, 0)) == LABEL_REF)
	   && gcn_vgpr_equivalent_register_operand (dest, mode))
    return true;

  if (MEM_P (dest)
      && MEM_ADDR_SPACE (dest) == ADDR_SPACE_SCALAR_FLAT
      && (gcn_scalar_flat_address_p (XEXP (dest, 0))
	  || GET_CODE (XEXP (dest, 0)) == SYMBOL_REF
	  || GET_CODE (XEXP (dest, 0)) == LABEL_REF)
      && gcn_ssrc_register_operand (src, mode))
    return true;
  else if (MEM_P (src)
	   && MEM_ADDR_SPACE (src) == ADDR_SPACE_SCALAR_FLAT
	   && (gcn_scalar_flat_address_p (XEXP (src, 0))
	       || GET_CODE (XEXP (src, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (src, 0)) == LABEL_REF)
	   && gcn_sdst_register_operand (dest, mode))
    return true;

  if (MEM_P (dest)
      && AS_ANY_DS_P (MEM_ADDR_SPACE (dest))
      && gcn_ds_address_p (XEXP (dest, 0))
      && gcn_vgpr_equivalent_register_operand (src, mode))
    return true;
  else if (MEM_P (src)
	   && AS_ANY_DS_P (MEM_ADDR_SPACE (src))
	   && gcn_ds_address_p (XEXP (src, 0))
	   && gcn_vgpr_equivalent_register_operand (dest, mode))
    return true;

  return false;
}

/* }}}  */
/* {{{ Functions and ABI.  */

/* Implement TARGET_FUNCTION_VALUE.

   Define how to find the value returned by a function.
   The register location is always the same, but the mode depends on
   VALTYPE.  */

static rtx
gcn_function_value (const_tree valtype, const_tree, bool)
{
  machine_mode mode = TYPE_MODE (valtype);

  if (INTEGRAL_TYPE_P (valtype)
      && GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) < 4)
    mode = SImode;

  return gen_rtx_REG (mode, RETURN_VALUE_REG);
}

/* Implement TARGET_FUNCTION_VALUE_REGNO_P.

   Return true if N is a possible register number for the function return
   value.  */

static bool
gcn_function_value_regno_p (const unsigned int n)
{
  return n == RETURN_VALUE_REG;
}

/* Calculate the number of registers required to hold function argument
   ARG.  */

static int
num_arg_regs (const function_arg_info &arg)
{
  if (targetm.calls.must_pass_in_stack (arg))
    return 0;

  int size = arg.promoted_size_in_bytes ();
  int regsize = UNITS_PER_WORD * (VECTOR_MODE_P (arg.mode)
				  ? GET_MODE_NUNITS (arg.mode) : 1);
  return (size + regsize - 1) / regsize;
}

/* Implement TARGET_STRICT_ARGUMENT_NAMING.

   Return true if the location where a function argument is passed
   depends on whether or not it is a named argument

   For gcn, we know how to handle functions declared as stdarg: by
   passing an extra pointer to the unnamed arguments.  However, the
   Fortran frontend can produce a different situation, where a
   function pointer is declared with no arguments, but the actual
   function and calls to it take more arguments.  In that case, we
   want to ensure the call matches the definition of the function.  */

static bool
gcn_strict_argument_naming (cumulative_args_t cum_v)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  return cum->fntype == NULL_TREE || stdarg_p (cum->fntype);
}

/* Implement TARGET_PRETEND_OUTGOING_VARARGS_NAMED.

   See comment on gcn_strict_argument_naming.  */

static bool
gcn_pretend_outgoing_varargs_named (cumulative_args_t cum_v)
{
  return !gcn_strict_argument_naming (cum_v);
}

/* Implement TARGET_FUNCTION_ARG.

   Return an RTX indicating whether a function argument is passed in a register
   and if so, which register.  */

static rtx
gcn_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  if (cum->normal_function)
    {
      if (!arg.named || arg.end_marker_p ())
	return 0;

      if (targetm.calls.must_pass_in_stack (arg))
	return 0;

      int first_reg = (VECTOR_MODE_P (arg.mode)
		       ? FIRST_VPARM_REG : FIRST_PARM_REG);
      int cum_num = (VECTOR_MODE_P (arg.mode)
		     ? cum->vnum : cum->num);
      int reg_num = first_reg + cum_num;
      int num_regs = num_arg_regs (arg);
      if (num_regs > 0)
	while (reg_num % num_regs != 0)
	  reg_num++;
      if (reg_num + num_regs <= first_reg + NUM_PARM_REGS)
	return gen_rtx_REG (arg.mode, reg_num);
    }
  else
    {
      if (cum->num >= cum->args.nargs)
	{
	  cum->offset = (cum->offset + TYPE_ALIGN (arg.type) / 8 - 1)
	    & -(TYPE_ALIGN (arg.type) / 8);
	  cfun->machine->kernarg_segment_alignment
	    = MAX ((unsigned) cfun->machine->kernarg_segment_alignment,
		   TYPE_ALIGN (arg.type) / 8);
	  rtx addr = gen_rtx_REG (DImode,
				  cum->args.reg[KERNARG_SEGMENT_PTR_ARG]);
	  if (cum->offset)
	    addr = gen_rtx_PLUS (DImode, addr,
				 gen_int_mode (cum->offset, DImode));
	  rtx mem = gen_rtx_MEM (arg.mode, addr);
	  set_mem_attributes (mem, arg.type, 1);
	  set_mem_addr_space (mem, ADDR_SPACE_SCALAR_FLAT);
	  MEM_READONLY_P (mem) = 1;
	  return mem;
	}

      int a = cum->args.order[cum->num];
      if (arg.mode != gcn_kernel_arg_types[a].mode)
	{
	  error ("wrong type of argument %s", gcn_kernel_arg_types[a].name);
	  return 0;
	}
      return gen_rtx_REG ((machine_mode) gcn_kernel_arg_types[a].mode,
			  cum->args.reg[a]);
    }
  return 0;
}

/* Implement TARGET_FUNCTION_ARG_ADVANCE.

   Updates the summarizer variable pointed to by CUM_V to advance past an
   argument in the argument list.  */

static void
gcn_function_arg_advance (cumulative_args_t cum_v,
			  const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (cum->normal_function)
    {
      if (!arg.named)
	return;

      int first_reg = (VECTOR_MODE_P (arg.mode)
		       ? FIRST_VPARM_REG : FIRST_PARM_REG);
      int *cum_num = (VECTOR_MODE_P (arg.mode)
		      ? &cum->vnum : &cum->num);
      int num_regs = num_arg_regs (arg);
      if (num_regs > 0)
	while ((first_reg + *cum_num) % num_regs != 0)
	  (*cum_num)++;
      *cum_num += num_regs;
    }
  else
    {
      if (cum->num < cum->args.nargs)
	cum->num++;
      else
	{
	  cum->offset += tree_to_uhwi (TYPE_SIZE_UNIT (arg.type));
	  cfun->machine->kernarg_segment_byte_size = cum->offset;
	}
    }
}

/* Implement TARGET_ARG_PARTIAL_BYTES.

   Returns the number of bytes at the beginning of an argument that must be put
   in registers.  The value must be zero for arguments that are passed entirely
   in registers or that are entirely pushed on the stack.  */

static int
gcn_arg_partial_bytes (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (!arg.named)
    return 0;

  if (targetm.calls.must_pass_in_stack (arg))
    return 0;

  int cum_num = (VECTOR_MODE_P (arg.mode) ? cum->vnum : cum->num);
  int regsize = UNITS_PER_WORD * (VECTOR_MODE_P (arg.mode)
				  ? GET_MODE_NUNITS (arg.mode) : 1);

  if (cum_num >= NUM_PARM_REGS)
    return 0;

  /* If the argument fits entirely in registers, return 0.  */
  if (cum_num + num_arg_regs (arg) <= NUM_PARM_REGS)
    return 0;

  return (NUM_PARM_REGS - cum_num) * regsize;
}

/* A normal function which takes a pointer argument may be passed a pointer to
   LDS space (via a high-bits-set aperture), and that only works with FLAT
   addressing, not GLOBAL.  Force FLAT addressing if the function has an
   incoming pointer parameter.  NOTE: This is a heuristic that works in the
   offloading case, but in general, a function might read global pointer
   variables, etc. that may refer to LDS space or other special memory areas
   not supported by GLOBAL instructions, and then this argument check would not
   suffice.  */

static void
gcn_detect_incoming_pointer_arg (tree fndecl)
{
  gcc_assert (cfun && cfun->machine);

  for (tree arg = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
       arg;
       arg = TREE_CHAIN (arg))
    if (POINTER_TYPE_P (TREE_VALUE (arg)))
      cfun->machine->use_flat_addressing = true;
}

/* Implement INIT_CUMULATIVE_ARGS, via gcn.h.

   Initialize a variable CUM of type CUMULATIVE_ARGS for a call to a function
   whose data type is FNTYPE.  For a library call, FNTYPE is 0.  */

void
gcn_init_cumulative_args (CUMULATIVE_ARGS *cum /* Argument info to init */ ,
			  tree fntype /* tree ptr for function decl */ ,
			  rtx libname /* SYMBOL_REF of library name or 0 */ ,
			  tree fndecl, int caller)
{
  memset (cum, 0, sizeof (*cum));
  cum->fntype = fntype;
  if (libname)
    {
      gcc_assert (cfun && cfun->machine);
      cum->normal_function = true;
      if (!caller)
	{
	  cfun->machine->normal_function = true;
	  gcn_detect_incoming_pointer_arg (fndecl);
	}
      return;
    }
  tree attr = NULL;
  if (fndecl)
    attr = lookup_attribute ("amdgpu_hsa_kernel", DECL_ATTRIBUTES (fndecl));
  if (fndecl && !attr)
    attr = lookup_attribute ("amdgpu_hsa_kernel",
			     TYPE_ATTRIBUTES (TREE_TYPE (fndecl)));
  if (!attr && fntype)
    attr = lookup_attribute ("amdgpu_hsa_kernel", TYPE_ATTRIBUTES (fntype));
  /* Handle main () as kernel, so we can run testsuite.
     Handle OpenACC kernels similarly to main.  */
  if (!attr && !caller && fndecl
      && (MAIN_NAME_P (DECL_NAME (fndecl))
	  || lookup_attribute ("omp target entrypoint",
			       DECL_ATTRIBUTES (fndecl)) != NULL_TREE))
    gcn_parse_amdgpu_hsa_kernel_attribute (&cum->args, NULL_TREE);
  else
    {
      if (!attr || caller)
	{
	  gcc_assert (cfun && cfun->machine);
	  cum->normal_function = true;
	  if (!caller)
	    cfun->machine->normal_function = true;
	}
      gcn_parse_amdgpu_hsa_kernel_attribute
	(&cum->args, attr ? TREE_VALUE (attr) : NULL_TREE);
    }
  cfun->machine->args = cum->args;
  if (!caller && cfun->machine->normal_function)
    gcn_detect_incoming_pointer_arg (fndecl);

  reinit_regs ();
}

static bool
gcn_return_in_memory (const_tree type, const_tree ARG_UNUSED (fntype))
{
  machine_mode mode = TYPE_MODE (type);
  HOST_WIDE_INT size = int_size_in_bytes (type);

  if (AGGREGATE_TYPE_P (type))
    return true;

  if (mode == BLKmode)
    return true;

  if ((!VECTOR_TYPE_P (type) && size > 2 * UNITS_PER_WORD)
      || size > 2 * UNITS_PER_WORD * 64)
    return true;

  return false;
}

/* Implement TARGET_PROMOTE_FUNCTION_MODE.

   Return the mode to use for outgoing function arguments.  */

machine_mode
gcn_promote_function_mode (const_tree ARG_UNUSED (type), machine_mode mode,
			   int *ARG_UNUSED (punsignedp),
			   const_tree ARG_UNUSED (funtype),
			   int ARG_UNUSED (for_return))
{
  if (GET_MODE_CLASS (mode) == MODE_INT && GET_MODE_SIZE (mode) < 4)
    return SImode;

  return mode;
}

/* Implement TARGET_GIMPLIFY_VA_ARG_EXPR.

   Derived from hppa_gimplify_va_arg_expr.  The generic routine doesn't handle
   ARGS_GROW_DOWNWARDS.  */

static tree
gcn_gimplify_va_arg_expr (tree valist, tree type,
			  gimple_seq *ARG_UNUSED (pre_p),
			  gimple_seq *ARG_UNUSED (post_p))
{
  tree ptr = build_pointer_type (type);
  tree valist_type;
  tree t, u;
  bool indirect;

  indirect = pass_va_arg_by_reference (type);
  if (indirect)
    {
      type = ptr;
      ptr = build_pointer_type (type);
    }
  valist_type = TREE_TYPE (valist);

  /* Args grow down.  Not handled by generic routines.  */

  u = fold_convert (sizetype, size_in_bytes (type));
  u = fold_build1 (NEGATE_EXPR, sizetype, u);
  t = fold_build_pointer_plus (valist, u);

  /* Align to 8 byte boundary.  */

  u = build_int_cst (TREE_TYPE (t), -8);
  t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t, u);
  t = fold_convert (valist_type, t);

  t = build2 (MODIFY_EXPR, valist_type, valist, t);

  t = fold_convert (ptr, t);
  t = build_va_arg_indirect_ref (t);

  if (indirect)
    t = build_va_arg_indirect_ref (t);

  return t;
}

/* Return 1 if TRAIT NAME is present in the OpenMP context's
   device trait set, return 0 if not present in any OpenMP context in the
   whole translation unit, or -1 if not present in the current OpenMP context
   but might be present in another OpenMP context in the same TU.  */

int
gcn_omp_device_kind_arch_isa (enum omp_device_kind_arch_isa trait,
			      const char *name)
{
  switch (trait)
    {
    case omp_device_kind:
      return strcmp (name, "gpu") == 0;
    case omp_device_arch:
      return strcmp (name, "amdgcn") == 0 || strcmp (name, "gcn") == 0;
    case omp_device_isa:
      return strcmp (name, gcn_devices[gcn_arch].name) == 0;
    default:
      gcc_unreachable ();
    }
}

/* Calculate stack offsets needed to create prologues and epilogues.  */

static struct machine_function *
gcn_compute_frame_offsets (void)
{
  machine_function *offsets = cfun->machine;

  if (reload_completed)
    return offsets;

  offsets->need_frame_pointer = frame_pointer_needed;

  offsets->outgoing_args_size = crtl->outgoing_args_size;
  offsets->pretend_size = crtl->args.pretend_args_size;

  offsets->local_vars = get_frame_size ();

  offsets->lr_needs_saving = (!leaf_function_p ()
			      || df_regs_ever_live_p (LR_REGNUM)
			      || df_regs_ever_live_p (LR_REGNUM + 1));

  offsets->callee_saves = offsets->lr_needs_saving ? 8 : 0;

  for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((df_regs_ever_live_p (regno) && !call_used_or_fixed_reg_p (regno))
	|| ((regno & ~1) == HARD_FRAME_POINTER_REGNUM
	    && frame_pointer_needed))
      offsets->callee_saves += (VGPR_REGNO_P (regno)
			       	|| AVGPR_REGNO_P (regno) ? 256 : 4);

  /* Round up to 64-bit boundary to maintain stack alignment.  */
  offsets->callee_saves = (offsets->callee_saves + 7) & ~7;

  return offsets;
}

/* Insert code into the prologue or epilogue to store or load any
   callee-save register to/from the stack.

   Helper function for gcn_expand_prologue and gcn_expand_epilogue.  */

static void
move_callee_saved_registers (rtx sp, machine_function *offsets,
			     bool prologue)
{
  int regno, offset, saved_scalars;
  rtx exec = gen_rtx_REG (DImode, EXEC_REG);
  rtx vcc = gen_rtx_REG (DImode, VCC_LO_REG);
  rtx offreg = gen_rtx_REG (SImode, SGPR_REGNO (22));
  rtx as = gen_rtx_CONST_INT (VOIDmode, STACK_ADDR_SPACE);
  HOST_WIDE_INT exec_set = 0;
  int offreg_set = 0;
  auto_vec<int> saved_sgprs;

  start_sequence ();

  /* Move scalars into two vector registers.  */
  for (regno = 0, saved_scalars = 0; regno < FIRST_VGPR_REG; regno++)
    if ((df_regs_ever_live_p (regno) && !call_used_or_fixed_reg_p (regno))
	|| ((regno & ~1) == LINK_REGNUM && offsets->lr_needs_saving)
	|| ((regno & ~1) == HARD_FRAME_POINTER_REGNUM
	    && offsets->need_frame_pointer))
      {
	rtx reg = gen_rtx_REG (SImode, regno);
	rtx vreg = gen_rtx_REG (V64SImode,
				VGPR_REGNO (6 + (saved_scalars / 64)));
	int lane = saved_scalars % 64;

	if (prologue)
	  {
	    emit_insn (gen_vec_setv64si (vreg, reg, GEN_INT (lane)));
	    saved_sgprs.safe_push (regno);
	  }
	else
	  emit_insn (gen_vec_extractv64sisi (reg, vreg, GEN_INT (lane)));

	saved_scalars++;
      }

  rtx move_scalars = get_insns ();
  end_sequence ();
  start_sequence ();

  /* Ensure that all vector lanes are moved.  */
  exec_set = -1;
  emit_move_insn (exec, GEN_INT (exec_set));

  /* Set up a vector stack pointer.  */
  rtx _0_1_2_3 = gen_rtx_REG (V64SImode, VGPR_REGNO (1));
  rtx _0_4_8_12 = gen_rtx_REG (V64SImode, VGPR_REGNO (3));
  emit_insn (gen_ashlv64si3_exec (_0_4_8_12, _0_1_2_3, GEN_INT (2),
				  gcn_gen_undef (V64SImode), exec));
  rtx vsp = gen_rtx_REG (V64DImode, VGPR_REGNO (4));
  emit_insn (gen_vec_duplicatev64di_exec (vsp, sp, gcn_gen_undef (V64DImode),
					  exec));
  emit_insn (gen_addv64si3_vcc_exec (gcn_operand_part (V64SImode, vsp, 0),
				     gcn_operand_part (V64SImode, vsp, 0),
				     _0_4_8_12, vcc, gcn_gen_undef (V64SImode),
				     exec));
  emit_insn (gen_addcv64si3_exec (gcn_operand_part (V64SImode, vsp, 1),
				  gcn_operand_part (V64SImode, vsp, 1),
				  const0_rtx, vcc, vcc,
				  gcn_gen_undef (V64SImode), exec));

  /* Move vectors.  */
  for (regno = FIRST_VGPR_REG, offset = 0;
       regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((df_regs_ever_live_p (regno) && !call_used_or_fixed_reg_p (regno))
	|| (regno == VGPR_REGNO (6) && saved_scalars > 0)
	|| (regno == VGPR_REGNO (7) && saved_scalars > 63))
      {
	rtx reg = gen_rtx_REG (V64SImode, regno);
	int size = 256;

	if (regno == VGPR_REGNO (6) && saved_scalars < 64)
	  size = saved_scalars * 4;
	else if (regno == VGPR_REGNO (7) && saved_scalars < 128)
	  size = (saved_scalars - 64) * 4;

	if (size != 256 || exec_set != -1)
	  {
	    exec_set = ((unsigned HOST_WIDE_INT) 1 << (size / 4)) - 1;
	    emit_move_insn (exec, gen_int_mode (exec_set, DImode));
	  }

	if (prologue)
	  {
	    rtx insn = emit_insn (gen_scatterv64si_insn_1offset_exec
				  (vsp, const0_rtx, reg, as, const0_rtx,
				   exec));

	    /* Add CFI metadata.  */
	    rtx note;
	    if (regno == VGPR_REGNO (6) || regno == VGPR_REGNO (7))
	      {
		int start = (regno == VGPR_REGNO (7) ? 64 : 0);
		int count = MIN (saved_scalars - start, 64);
		int add_lr = (regno == VGPR_REGNO (6)
			      && offsets->lr_needs_saving);
		int lrdest = -1;
		rtvec seq = rtvec_alloc (count + add_lr);

		/* Add an REG_FRAME_RELATED_EXPR entry for each scalar
		   register that was saved in this batch.  */
		for (int idx = 0; idx < count; idx++)
		  {
		    int stackaddr = offset + idx * 4;
		    rtx dest = gen_rtx_MEM (SImode,
					    gen_rtx_PLUS
					    (DImode, sp,
					     GEN_INT (stackaddr)));
		    rtx src = gen_rtx_REG (SImode, saved_sgprs[start + idx]);
		    rtx set = gen_rtx_SET (dest, src);
		    RTX_FRAME_RELATED_P (set) = 1;
		    RTVEC_ELT (seq, idx) = set;

		    if (saved_sgprs[start + idx] == LINK_REGNUM)
		      lrdest = stackaddr;
		  }

		/* Add an additional expression for DWARF_LINK_REGISTER if
		   LINK_REGNUM was saved.  */
		if (lrdest != -1)
		  {
		    rtx dest = gen_rtx_MEM (DImode,
					    gen_rtx_PLUS
					    (DImode, sp,
					     GEN_INT (lrdest)));
		    rtx src = gen_rtx_REG (DImode, DWARF_LINK_REGISTER);
		    rtx set = gen_rtx_SET (dest, src);
		    RTX_FRAME_RELATED_P (set) = 1;
		    RTVEC_ELT (seq, count) = set;
		  }

		note = gen_rtx_SEQUENCE (VOIDmode, seq);
	      }
	    else
	      {
		rtx dest = gen_rtx_MEM (V64SImode,
					gen_rtx_PLUS (DImode, sp,
						      GEN_INT (offset)));
		rtx src = gen_rtx_REG (V64SImode, regno);
		note = gen_rtx_SET (dest, src);
	      }
	    RTX_FRAME_RELATED_P (insn) = 1;
	    add_reg_note (insn, REG_FRAME_RELATED_EXPR, note);
	  }
	else
	  emit_insn (gen_gatherv64si_insn_1offset_exec
		     (reg, vsp, const0_rtx, as, const0_rtx,
		      gcn_gen_undef (V64SImode), exec));

	/* Move our VSP to the next stack entry.  */
	if (offreg_set != size)
	  {
	    offreg_set = size;
	    emit_move_insn (offreg, GEN_INT (size));
	  }
	if (exec_set != -1)
	  {
	    exec_set = -1;
	    emit_move_insn (exec, GEN_INT (exec_set));
	  }
	emit_insn (gen_addv64si3_vcc_dup_exec
		   (gcn_operand_part (V64SImode, vsp, 0),
		    offreg, gcn_operand_part (V64SImode, vsp, 0),
		    vcc, gcn_gen_undef (V64SImode), exec));
	emit_insn (gen_addcv64si3_exec
		   (gcn_operand_part (V64SImode, vsp, 1),
		    gcn_operand_part (V64SImode, vsp, 1),
		    const0_rtx, vcc, vcc, gcn_gen_undef (V64SImode), exec));

	offset += size;
      }

  rtx move_vectors = get_insns ();
  end_sequence ();

  if (prologue)
    {
      emit_insn (move_scalars);
      emit_insn (move_vectors);
    }
  else
    {
      emit_insn (move_vectors);
      emit_insn (move_scalars);
    }

  /* This happens when a new register becomes "live" after reload.
     Check your splitters!  */
  gcc_assert (offset <= offsets->callee_saves);
}

/* Generate prologue.  Called from gen_prologue during pro_and_epilogue pass.

   For a non-kernel function, the stack layout looks like this (interim),
   growing *upwards*:

 hi | + ...
    |__________________| <-- current SP
    | outgoing args    |
    |__________________|
    | (alloca space)   |
    |__________________|
    | local vars       |
    |__________________| <-- FP/hard FP
    | callee-save regs |
    |__________________| <-- soft arg pointer
    | pretend args     |
    |__________________| <-- incoming SP
    | incoming args    |
 lo |..................|

   This implies arguments (beyond the first N in registers) must grow
   downwards (as, apparently, PA has them do).

   For a kernel function we have the simpler:

 hi | + ...
    |__________________| <-- current SP
    | outgoing args    |
    |__________________|
    | (alloca space)   |
    |__________________|
    | local vars       |
 lo |__________________| <-- FP/hard FP

*/

void
gcn_expand_prologue ()
{
  machine_function *offsets = gcn_compute_frame_offsets ();

  if (!cfun || !cfun->machine || cfun->machine->normal_function)
    {
      rtx sp = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
      rtx sp_hi = gcn_operand_part (Pmode, sp, 1);
      rtx sp_lo = gcn_operand_part (Pmode, sp, 0);
      rtx fp = gen_rtx_REG (Pmode, HARD_FRAME_POINTER_REGNUM);
      rtx fp_hi = gcn_operand_part (Pmode, fp, 1);
      rtx fp_lo = gcn_operand_part (Pmode, fp, 0);

      start_sequence ();

      if (offsets->pretend_size > 0)
	{
	  /* FIXME: Do the actual saving of register pretend args to the stack.
	     Register order needs consideration.  */
	}

      /* Save callee-save regs.  */
      move_callee_saved_registers (sp, offsets, true);

      HOST_WIDE_INT sp_adjust = offsets->pretend_size
	+ offsets->callee_saves
	+ offsets->local_vars + offsets->outgoing_args_size;
      if (sp_adjust > 0)
	{
	  /* Adding RTX_FRAME_RELATED_P effectively disables spliting, so
	     we use split add explictly, and specify the DImode add in
	     the note.  */
	  rtx scc = gen_rtx_REG (BImode, SCC_REG);
	  rtx adjustment = gen_int_mode (sp_adjust, SImode);
	  rtx insn = emit_insn (gen_addsi3_scalar_carry (sp_lo, sp_lo,
							 adjustment, scc));
	  if (!offsets->need_frame_pointer)
	    {
	      RTX_FRAME_RELATED_P (insn) = 1;
	      add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			    gen_rtx_SET (sp,
					 gen_rtx_PLUS (DImode, sp,
						       adjustment)));
	    }
	  emit_insn (gen_addcsi3_scalar_zero (sp_hi, sp_hi, scc));
	}

      if (offsets->need_frame_pointer)
	{
	  /* Adding RTX_FRAME_RELATED_P effectively disables spliting, so
	     we use split add explictly, and specify the DImode add in
	     the note.  */
	  rtx scc = gen_rtx_REG (BImode, SCC_REG);
	  int fp_adjust = -(offsets->local_vars + offsets->outgoing_args_size);
	  rtx adjustment = gen_int_mode (fp_adjust, SImode);
	  rtx insn = emit_insn (gen_addsi3_scalar_carry(fp_lo, sp_lo,
							adjustment, scc));
	  emit_insn (gen_addcsi3_scalar (fp_hi, sp_hi,
					 (fp_adjust < 0 ? GEN_INT (-1)
					  : const0_rtx),
					 scc, scc));

	  /* Set the CFA to the entry stack address, as an offset from the
	     frame pointer.  This is preferred because the frame pointer is
	     saved in each frame, whereas the stack pointer is not.  */
	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			gen_rtx_PLUS (DImode, fp,
				      GEN_INT (-(offsets->pretend_size
						 + offsets->callee_saves))));
	}

      rtx_insn *seq = get_insns ();
      end_sequence ();

      emit_insn (seq);
    }
  else
    {
      if (TARGET_PACKED_WORK_ITEMS)
	{
	  /* v0 conatins the X, Y and Z dimensions all in one.
	     Expand them out for ABI compatibility.  */
	  /* TODO: implement and use zero_extract.  */
	  rtx v1 = gen_rtx_REG (V64SImode, VGPR_REGNO (1));
	  emit_insn (gen_andv64si3 (v1, gen_rtx_REG (V64SImode, VGPR_REGNO (0)),
				    gen_rtx_CONST_INT (VOIDmode, 0x3FF << 10)));
	  emit_insn (gen_lshrv64si3 (v1, v1, gen_rtx_CONST_INT (VOIDmode, 10)));
	  emit_insn (gen_prologue_use (v1));

	  rtx v2 = gen_rtx_REG (V64SImode, VGPR_REGNO (2));
	  emit_insn (gen_andv64si3 (v2, gen_rtx_REG (V64SImode, VGPR_REGNO (0)),
				    gen_rtx_CONST_INT (VOIDmode, 0x3FF << 20)));
	  emit_insn (gen_lshrv64si3 (v2, v2, gen_rtx_CONST_INT (VOIDmode, 20)));
	  emit_insn (gen_prologue_use (v2));
	}

      /* We no longer use the private segment for the stack (it's not
	 accessible to reverse offload), so we must calculate a wave offset
	 from the grid dimensions and stack size, which is calculated on the
	 host, and passed in the kernargs region.
	 See libgomp-gcn.h for details.  */
      rtx wave_offset = gen_rtx_REG (SImode, FIRST_PARM_REG);

      rtx num_waves_mem = gcn_oacc_dim_size (1);
      rtx num_waves = gen_rtx_REG (SImode, FIRST_PARM_REG+1);
      set_mem_addr_space (num_waves_mem, ADDR_SPACE_SCALAR_FLAT);
      emit_move_insn (num_waves, num_waves_mem);

      rtx workgroup_num = gcn_oacc_dim_pos (0);
      rtx wave_num = gen_rtx_REG (SImode, FIRST_PARM_REG+2);
      emit_move_insn(wave_num, gcn_oacc_dim_pos (1));

      rtx thread_id = gen_rtx_REG (SImode, FIRST_PARM_REG+3);
      emit_insn (gen_mulsi3 (thread_id, num_waves, workgroup_num));
      emit_insn (gen_addsi3_scc (thread_id, thread_id, wave_num));

      rtx kernarg_reg = gen_rtx_REG (DImode, cfun->machine->args.reg
				     [KERNARG_SEGMENT_PTR_ARG]);
      rtx stack_size_mem = gen_rtx_MEM (SImode,
					gen_rtx_PLUS (DImode, kernarg_reg,
						      GEN_INT (52)));
      set_mem_addr_space (stack_size_mem, ADDR_SPACE_SCALAR_FLAT);
      emit_move_insn (wave_offset, stack_size_mem);

      emit_insn (gen_mulsi3 (wave_offset, wave_offset, thread_id));

      /* The FLAT_SCRATCH_INIT is not usually needed, but can be enabled
	 via the function attributes.  */
      if (cfun->machine->args.requested & (1 << FLAT_SCRATCH_INIT_ARG))
	{
	  rtx fs_init_lo =
	    gen_rtx_REG (SImode,
			 cfun->machine->args.reg[FLAT_SCRATCH_INIT_ARG]);
	  rtx fs_init_hi =
	    gen_rtx_REG (SImode,
			 cfun->machine->args.reg[FLAT_SCRATCH_INIT_ARG] + 1);
	  rtx fs_reg_lo = gen_rtx_REG (SImode, FLAT_SCRATCH_REG);
	  rtx fs_reg_hi = gen_rtx_REG (SImode, FLAT_SCRATCH_REG + 1);

	  /*rtx queue = gen_rtx_REG(DImode,
				  cfun->machine->args.reg[QUEUE_PTR_ARG]);
	  rtx aperture = gen_rtx_MEM (SImode,
				      gen_rtx_PLUS (DImode, queue,
						    gen_int_mode (68, SImode)));
	  set_mem_addr_space (aperture, ADDR_SPACE_SCALAR_FLAT);*/

	  /* Set up flat_scratch.  */
	  emit_insn (gen_addsi3_scc (fs_reg_hi, fs_init_lo, wave_offset));
	  emit_insn (gen_lshrsi3_scc (fs_reg_hi, fs_reg_hi,
				      gen_int_mode (8, SImode)));
	  emit_move_insn (fs_reg_lo, fs_init_hi);
	}

      /* Set up frame pointer and stack pointer.  */
      rtx sp = gen_rtx_REG (DImode, STACK_POINTER_REGNUM);
      rtx sp_hi = simplify_gen_subreg (SImode, sp, DImode, 4);
      rtx sp_lo = simplify_gen_subreg (SImode, sp, DImode, 0);
      rtx fp = gen_rtx_REG (DImode, HARD_FRAME_POINTER_REGNUM);
      rtx fp_hi = simplify_gen_subreg (SImode, fp, DImode, 4);
      rtx fp_lo = simplify_gen_subreg (SImode, fp, DImode, 0);

      HOST_WIDE_INT sp_adjust = (offsets->local_vars
				 + offsets->outgoing_args_size);

      /* Initialize FP and SP from space allocated on the host.  */
      rtx stack_addr_mem = gen_rtx_MEM (DImode,
					gen_rtx_PLUS (DImode, kernarg_reg,
						      GEN_INT (40)));
      set_mem_addr_space (stack_addr_mem, ADDR_SPACE_SCALAR_FLAT);
      emit_move_insn (fp, stack_addr_mem);
      rtx scc = gen_rtx_REG (BImode, SCC_REG);
      emit_insn (gen_addsi3_scalar_carry (fp_lo, fp_lo, wave_offset, scc));
      emit_insn (gen_addcsi3_scalar_zero (fp_hi, fp_hi, scc));

      /* Adding RTX_FRAME_RELATED_P effectively disables spliting, so we use
	 split add explictly, and specify the DImode add in the note.
         The DWARF info expects that the callee-save data is in the frame,
         even though it isn't (because this is the entry point), so we
         make a notional adjustment to the DWARF frame offset here.  */
      rtx dbg_adjustment = gen_int_mode (sp_adjust + offsets->callee_saves,
					 DImode);
      rtx insn;
      if (sp_adjust > 0)
	{
	  rtx scc = gen_rtx_REG (BImode, SCC_REG);
	  rtx adjustment = gen_int_mode (sp_adjust, DImode);
	  insn = emit_insn (gen_addsi3_scalar_carry(sp_lo, fp_lo, adjustment,
						    scc));
	  emit_insn (gen_addcsi3_scalar_zero (sp_hi, fp_hi, scc));
	}
      else
	insn = emit_move_insn (sp, fp);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		    gen_rtx_SET (sp, gen_rtx_PLUS (DImode, sp,
						   dbg_adjustment)));

      if (offsets->need_frame_pointer)
	{
	  /* Set the CFA to the entry stack address, as an offset from the
	     frame pointer.  This is necessary when alloca is used, and
	     harmless otherwise.  */
	  rtx neg_adjust = gen_int_mode (-offsets->callee_saves, DImode);
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			gen_rtx_PLUS (DImode, fp, neg_adjust));
	}

      /* Make sure the flat scratch reg doesn't get optimised away.  */
      emit_insn (gen_prologue_use (gen_rtx_REG (DImode, FLAT_SCRATCH_REG)));
    }

  /* Ensure that the scheduler doesn't do anything unexpected.  */
  emit_insn (gen_blockage ());

  if (cfun && cfun->machine && !cfun->machine->normal_function && flag_openmp)
    {
      /* OpenMP kernels have an implicit call to gomp_gcn_enter_kernel.  */
      rtx fn_reg = gen_rtx_REG (Pmode, FIRST_PARM_REG);
      emit_move_insn (fn_reg, gen_rtx_SYMBOL_REF (Pmode,
						  "gomp_gcn_enter_kernel"));
      emit_call_insn (gen_gcn_indirect_call (fn_reg, const0_rtx));
    }
}

/* Generate epilogue.  Called from gen_epilogue during pro_and_epilogue pass.

   See gcn_expand_prologue for stack details.  */

void
gcn_expand_epilogue (void)
{
  /* Ensure that the scheduler doesn't do anything unexpected.  */
  emit_insn (gen_blockage ());

  if (!cfun || !cfun->machine || cfun->machine->normal_function)
    {
      machine_function *offsets = gcn_compute_frame_offsets ();
      rtx sp = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
      rtx fp = gen_rtx_REG (Pmode, HARD_FRAME_POINTER_REGNUM);

      HOST_WIDE_INT sp_adjust = offsets->callee_saves + offsets->pretend_size;

      if (offsets->need_frame_pointer)
	{
	  /* Restore old SP from the frame pointer.  */
	  if (sp_adjust > 0)
	    emit_insn (gen_subdi3 (sp, fp, gen_int_mode (sp_adjust, DImode)));
	  else
	    emit_move_insn (sp, fp);
	}
      else
	{
	  /* Restore old SP from current SP.  */
	  sp_adjust += offsets->outgoing_args_size + offsets->local_vars;

	  if (sp_adjust > 0)
	    emit_insn (gen_subdi3 (sp, sp, gen_int_mode (sp_adjust, DImode)));
	}

      move_callee_saved_registers (sp, offsets, false);

      /* There's no explicit use of the link register on the return insn.  Emit
         one here instead.  */
      if (offsets->lr_needs_saving)
	emit_use (gen_rtx_REG (DImode, LINK_REGNUM));

      /* Similar for frame pointer.  */
      if (offsets->need_frame_pointer)
	emit_use (gen_rtx_REG (DImode, HARD_FRAME_POINTER_REGNUM));
    }
  else if (flag_openmp)
    {
      /* OpenMP kernels have an implicit call to gomp_gcn_exit_kernel.  */
      rtx fn_reg = gen_rtx_REG (Pmode, FIRST_PARM_REG);
      emit_move_insn (fn_reg,
		      gen_rtx_SYMBOL_REF (Pmode, "gomp_gcn_exit_kernel"));
      emit_call_insn (gen_gcn_indirect_call (fn_reg, const0_rtx));
    }
  else if (TREE_CODE (TREE_TYPE (DECL_RESULT (cfun->decl))) != VOID_TYPE)
    {
      /* Assume that an exit value compatible with gcn-run is expected.
         That is, the third input parameter is an int*.

         We can't allocate any new registers, but the dispatch_ptr and
	 kernarg_reg are dead after this, so we'll use those.  */
      rtx dispatch_ptr_reg = gen_rtx_REG (DImode, cfun->machine->args.reg
					  [DISPATCH_PTR_ARG]);
      rtx kernarg_reg = gen_rtx_REG (DImode, cfun->machine->args.reg
				     [KERNARG_SEGMENT_PTR_ARG]);
      rtx retptr_mem = gen_rtx_MEM (DImode,
				    gen_rtx_PLUS (DImode, kernarg_reg,
						  GEN_INT (16)));
      set_mem_addr_space (retptr_mem, ADDR_SPACE_SCALAR_FLAT);
      emit_move_insn (dispatch_ptr_reg, retptr_mem);

      rtx retval_addr = gen_rtx_REG (DImode, FIRST_VPARM_REG + 2);
      emit_move_insn (retval_addr, dispatch_ptr_reg);
      rtx retval_mem = gen_rtx_MEM (SImode, retval_addr);
      set_mem_addr_space (retval_mem, ADDR_SPACE_FLAT);
      emit_move_insn (retval_mem, gen_rtx_REG (SImode, RETURN_VALUE_REG));
    }

  emit_jump_insn (gen_gcn_return ());
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.

   Return true if the frame pointer should not be eliminated.  */

bool
gcn_frame_pointer_rqd (void)
{
  /* GDB needs the frame pointer in order to unwind properly,
     but that's not important for the entry point, unless alloca is used.
     It's not important for code execution, so we should repect the
     -fomit-frame-pointer flag.  */
  return (!flag_omit_frame_pointer
	  && cfun
	  && (cfun->calls_alloca
	      || (cfun->machine && cfun->machine->normal_function)));
}

/* Implement TARGET_CAN_ELIMINATE.

   Return true if the compiler is allowed to try to replace register number
   FROM_REG with register number TO_REG.

   FIXME: is the default "true" not enough? Should this be a negative set?  */

bool
gcn_can_eliminate_p (int /*from_reg */ , int to_reg)
{
  return (to_reg == HARD_FRAME_POINTER_REGNUM
	  || to_reg == STACK_POINTER_REGNUM);
}

/* Implement INITIAL_ELIMINATION_OFFSET.

   Returns the initial difference between the specified pair of registers, in
   terms of stack position.  */

HOST_WIDE_INT
gcn_initial_elimination_offset (int from, int to)
{
  machine_function *offsets = gcn_compute_frame_offsets ();

  switch (from)
    {
    case ARG_POINTER_REGNUM:
      if (to == STACK_POINTER_REGNUM)
	return -(offsets->callee_saves + offsets->local_vars
		 + offsets->outgoing_args_size);
      else if (to == FRAME_POINTER_REGNUM || to == HARD_FRAME_POINTER_REGNUM)
	return -offsets->callee_saves;
      else
	gcc_unreachable ();
      break;

    case FRAME_POINTER_REGNUM:
      if (to == STACK_POINTER_REGNUM)
	return -(offsets->local_vars + offsets->outgoing_args_size);
      else if (to == HARD_FRAME_POINTER_REGNUM)
	return 0;
      else
	gcc_unreachable ();
      break;

    default:
      gcc_unreachable ();
    }
}

/* Implement HARD_REGNO_RENAME_OK.

   Return true if it is permissible to rename a hard register from
   FROM_REG to TO_REG.  */

bool
gcn_hard_regno_rename_ok (unsigned int from_reg, unsigned int to_reg)
{
  if (from_reg == SCC_REG
      || from_reg == VCC_LO_REG || from_reg == VCC_HI_REG
      || from_reg == EXEC_LO_REG || from_reg == EXEC_HI_REG
      || to_reg == SCC_REG
      || to_reg == VCC_LO_REG || to_reg == VCC_HI_REG
      || to_reg == EXEC_LO_REG || to_reg == EXEC_HI_REG)
    return false;

  /* Allow the link register to be used if it was saved.  */
  if ((to_reg & ~1) == LINK_REGNUM)
    return !cfun || cfun->machine->lr_needs_saving;

  /* Allow the registers used for the static chain to be used if the chain is
     not in active use.  */
  if ((to_reg & ~1) == STATIC_CHAIN_REGNUM)
    return !cfun
	|| !(cfun->static_chain_decl
	     && df_regs_ever_live_p (STATIC_CHAIN_REGNUM)
	     && df_regs_ever_live_p (STATIC_CHAIN_REGNUM + 1));

  return true;
}

/* Implement HARD_REGNO_CALLER_SAVE_MODE.

   Which mode is required for saving NREGS of a pseudo-register in
   call-clobbered hard register REGNO.  */

machine_mode
gcn_hard_regno_caller_save_mode (unsigned int regno, unsigned int nregs,
				 machine_mode regmode)
{
  machine_mode result = choose_hard_reg_mode (regno, nregs, NULL);

  if (VECTOR_MODE_P (result) && !VECTOR_MODE_P (regmode))
    result = (nregs == 1 ? SImode : DImode);

  return result;
}

/* Implement TARGET_ASM_TRAMPOLINE_TEMPLATE.

   Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

static void
gcn_asm_trampoline_template (FILE *f)
{
  /* The source operand of the move instructions must be a 32-bit
     constant following the opcode.  */
  asm_fprintf (f, "\ts_mov_b32\ts%i, 0xffff\n", STATIC_CHAIN_REGNUM);
  asm_fprintf (f, "\ts_mov_b32\ts%i, 0xffff\n", STATIC_CHAIN_REGNUM + 1);
  asm_fprintf (f, "\ts_mov_b32\ts%i, 0xffff\n", CC_SAVE_REG);
  asm_fprintf (f, "\ts_mov_b32\ts%i, 0xffff\n", CC_SAVE_REG + 1);
  asm_fprintf (f, "\ts_setpc_b64\ts[%i:%i]\n", CC_SAVE_REG, CC_SAVE_REG + 1);
  asm_fprintf (f, "\t.align 8\n");
}

/* Implement TARGET_TRAMPOLINE_INIT.

   Emit RTL insns to initialize the variable parts of a trampoline.
   FNDECL is the decl of the target address, M_TRAMP is a MEM for
   the trampoline, and CHAIN_VALUE is an RTX for the static chain
   to be passed to the target function.  */

static void
gcn_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx chain_value_reg = copy_to_reg (chain_value);
  rtx fnaddr_reg = copy_to_reg (fnaddr);

  for (int i = 0; i < 4; i++)
    {
      rtx mem = adjust_address (m_tramp, SImode, i * 8 + 4);
      rtx reg = i < 2 ? chain_value_reg : fnaddr_reg;
      emit_move_insn (mem, gen_rtx_SUBREG (SImode, reg, (i % 2) * 4));
    }

  rtx tramp_addr = XEXP (m_tramp, 0);
  emit_insn (gen_clear_icache (tramp_addr,
			       plus_constant (ptr_mode, tramp_addr,
					      TRAMPOLINE_SIZE)));
}

/* Implement TARGET_EXPAND_DIVMOD_LIBFUNC.

   There are divmod libfuncs for all modes except TImode.  They return the
   two values packed into a larger integer/vector.  */

void
gcn_expand_divmod_libfunc (rtx libfunc, machine_mode mode, rtx op0, rtx op1,
			   rtx *quot, rtx *rem)
{
  machine_mode innermode = (VECTOR_MODE_P (mode)
			    ? GET_MODE_INNER (mode) : mode);
  machine_mode wideinnermode = VOIDmode;
  machine_mode widemode = VOIDmode;

  switch (innermode)
    {
    case E_QImode:
    case E_HImode:
    case E_SImode:
      wideinnermode = DImode;
      break;
    case E_DImode:
      wideinnermode = TImode;
      break;
    default:
      gcc_unreachable ();
    }

  if (VECTOR_MODE_P (mode))
    widemode = VnMODE (GET_MODE_NUNITS (mode), wideinnermode);
  else
    widemode = wideinnermode;

  emit_library_call_value (libfunc, gen_rtx_REG (widemode, RETURN_VALUE_REG),
			   LCT_NORMAL, widemode, op0, mode, op1, mode);

  *quot = gen_rtx_REG (mode, RETURN_VALUE_REG);
  *rem = gen_rtx_REG (mode,
		      RETURN_VALUE_REG + (wideinnermode == TImode ? 2 : 1));
}

/* }}}  */
/* {{{ Miscellaneous.  */

/* Implement TARGET_CANNOT_COPY_INSN_P.

   Return true if INSN must not be duplicated.  */

static bool
gcn_cannot_copy_insn_p (rtx_insn *insn)
{
  if (recog_memoized (insn) == CODE_FOR_gcn_wavefront_barrier)
    return true;

  return false;
}

/* Implement TARGET_DEBUG_UNWIND_INFO.

   Defines the mechanism that will be used for describing frame unwind
   information to the debugger.  */

static enum unwind_info_type
gcn_debug_unwind_info ()
{
  return UI_DWARF2;
}

/* Determine if there is a suitable hardware conversion instruction.
   Used primarily by the machine description.  */

bool
gcn_valid_cvt_p (machine_mode from, machine_mode to, enum gcn_cvt_t op)
{
  if (VECTOR_MODE_P (from) != VECTOR_MODE_P (to))
    return false;

  if (VECTOR_MODE_P (from))
    {
      if (GET_MODE_NUNITS (from) != GET_MODE_NUNITS (to))
	return false;

      from = GET_MODE_INNER (from);
      to = GET_MODE_INNER (to);
    }

  switch (op)
    {
    case fix_trunc_cvt:
    case fixuns_trunc_cvt:
      if (GET_MODE_CLASS (from) != MODE_FLOAT
	  || GET_MODE_CLASS (to) != MODE_INT)
	return false;
      break;
    case float_cvt:
    case floatuns_cvt:
      if (GET_MODE_CLASS (from) != MODE_INT
	  || GET_MODE_CLASS (to) != MODE_FLOAT)
	return false;
      break;
    case extend_cvt:
      if (GET_MODE_CLASS (from) != MODE_FLOAT
	  || GET_MODE_CLASS (to) != MODE_FLOAT
	  || GET_MODE_SIZE (from) >= GET_MODE_SIZE (to))
	return false;
      break;
    case trunc_cvt:
      if (GET_MODE_CLASS (from) != MODE_FLOAT
	  || GET_MODE_CLASS (to) != MODE_FLOAT
	  || GET_MODE_SIZE (from) <= GET_MODE_SIZE (to))
	return false;
      break;
    }

  return ((to == HImode && from == HFmode)
	  || (to == SImode && (from == SFmode || from == DFmode))
	  || (to == HFmode && (from == HImode || from == SFmode))
	  || (to == SFmode && (from == SImode || from == HFmode
			       || from == DFmode))
	  || (to == DFmode && (from == SImode || from == SFmode)));
}

/* Implement TARGET_EMUTLS_VAR_INIT.

   Disable emutls (gthr-gcn.h does not support it, yet).  */

tree
gcn_emutls_var_init (tree, tree decl, tree)
{
  sorry_at (DECL_SOURCE_LOCATION (decl), "TLS is not implemented for GCN.");
  return NULL_TREE;
}

/* }}}  */
/* {{{ Costs.  */

/* Implement TARGET_RTX_COSTS.

   Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
gcn_rtx_costs (rtx x, machine_mode, int, int, int *total, bool)
{
  enum rtx_code code = GET_CODE (x);
  switch (code)
    {
    case CONST:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CONST_INT:
      if (gcn_inline_constant_p (x))
	*total = 0;
      else if (code == CONST_INT
	  && ((unsigned HOST_WIDE_INT) INTVAL (x) + 0x8000) < 0x10000)
	*total = 1;
      else if (gcn_constant_p (x))
	*total = 2;
      else
	*total = vgpr_vector_mode_p (GET_MODE (x)) ? 64 : 4;
      return true;

    case DIV:
      *total = 100;
      return false;

    default:
      *total = 3;
      return false;
    }
}

/* Implement TARGET_MEMORY_MOVE_COST.

   Return the cost of moving data of mode M between a
   register and memory.  A value of 2 is the default; this cost is
   relative to those in `REGISTER_MOVE_COST'.

   This function is used extensively by register_move_cost that is used to
   build tables at startup.  Make it inline in this case.
   When IN is 2, return maximum of in and out move cost.

   If moving between registers and memory is more expensive than
   between two registers, you should define this macro to express the
   relative cost.

   Model also increased moving costs of QImode registers in non
   Q_REGS classes.  */

#define LOAD_COST  32
#define STORE_COST 32
static int
gcn_memory_move_cost (machine_mode mode, reg_class_t regclass, bool in)
{
  int nregs = CEIL (GET_MODE_SIZE (mode), 4);
  switch (regclass)
    {
    case SCC_CONDITIONAL_REG:
    case VCCZ_CONDITIONAL_REG:
    case VCC_CONDITIONAL_REG:
    case EXECZ_CONDITIONAL_REG:
    case ALL_CONDITIONAL_REGS:
    case SGPR_REGS:
    case SGPR_EXEC_REGS:
    case EXEC_MASK_REG:
    case SGPR_VOP_SRC_REGS:
    case SGPR_MEM_SRC_REGS:
    case SGPR_SRC_REGS:
    case SGPR_DST_REGS:
    case GENERAL_REGS:
    case AFP_REGS:
      if (!in)
	return (STORE_COST + 2) * nregs;
      return LOAD_COST * nregs;
    case VGPR_REGS:
      if (in)
	return (LOAD_COST + 2) * nregs;
      return STORE_COST * nregs;
    case AVGPR_REGS:
    case ALL_VGPR_REGS:
      if (in)
	return (LOAD_COST + (TARGET_CDNA2_MEM_COSTS ? 2 : 4)) * nregs;
      return (STORE_COST + (TARGET_CDNA2_MEM_COSTS ? 0 : 2)) * nregs;
    case ALL_REGS:
    case ALL_GPR_REGS:
    case SRCDST_REGS:
      if (in)
	return (LOAD_COST + 2) * nregs;
      return (STORE_COST + 2) * nregs;
    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_REGISTER_MOVE_COST.

   Return the cost of moving data from a register in class CLASS1 to
   one in class CLASS2.  Base value is 2.  */

static int
gcn_register_move_cost (machine_mode, reg_class_t dst, reg_class_t src)
{
  if (src == AVGPR_REGS)
    {
      if (dst == AVGPR_REGS)
	return !TARGET_AVGPR_MEMOPS ? 6 : 2;
      if (dst != VGPR_REGS)
	return 6;
    }
  if (dst == AVGPR_REGS && src != VGPR_REGS)
    return 6;
  /* Increase cost of moving from and to vector registers.  While this is
     fast in hardware (I think), it has hidden cost of setting up the exec
     flags.  */
  if ((src < VGPR_REGS) != (dst < VGPR_REGS))
    return 4;
  return 2;
}

/* }}}  */
/* {{{ Builtins.  */

/* Type codes used by GCN built-in definitions.  */

enum gcn_builtin_type_index
{
  GCN_BTI_END_OF_PARAMS,

  GCN_BTI_VOID,
  GCN_BTI_BOOL,
  GCN_BTI_INT,
  GCN_BTI_UINT,
  GCN_BTI_SIZE_T,
  GCN_BTI_LLINT,
  GCN_BTI_LLUINT,
  GCN_BTI_EXEC,

  GCN_BTI_SF,
  GCN_BTI_V64SI,
  GCN_BTI_V64SF,
  GCN_BTI_V64DF,
  GCN_BTI_V64PTR,
  GCN_BTI_SIPTR,
  GCN_BTI_SFPTR,
  GCN_BTI_VOIDPTR,

  GCN_BTI_LDS_VOIDPTR,

  GCN_BTI_MAX
};

static GTY(()) tree gcn_builtin_types[GCN_BTI_MAX];

#define exec_type_node (gcn_builtin_types[GCN_BTI_EXEC])
#define sf_type_node (gcn_builtin_types[GCN_BTI_SF])
#define v64si_type_node (gcn_builtin_types[GCN_BTI_V64SI])
#define v64sf_type_node (gcn_builtin_types[GCN_BTI_V64SF])
#define v64df_type_node (gcn_builtin_types[GCN_BTI_V64DF])
#define v64ptr_type_node (gcn_builtin_types[GCN_BTI_V64PTR])
#define siptr_type_node (gcn_builtin_types[GCN_BTI_SIPTR])
#define sfptr_type_node (gcn_builtin_types[GCN_BTI_SFPTR])
#define voidptr_type_node (gcn_builtin_types[GCN_BTI_VOIDPTR])
#define size_t_type_node (gcn_builtin_types[GCN_BTI_SIZE_T])

static rtx gcn_expand_builtin_1 (tree, rtx, rtx, machine_mode, int,
				 struct gcn_builtin_description *);
static rtx gcn_expand_builtin_binop (tree, rtx, rtx, machine_mode, int,
				     struct gcn_builtin_description *);

struct gcn_builtin_description;
typedef rtx (*gcn_builtin_expander) (tree, rtx, rtx, machine_mode, int,
				     struct gcn_builtin_description *);

enum gcn_builtin_type
{
  B_UNIMPLEMENTED,		/* Sorry out */
  B_INSN,			/* Emit a pattern */
  B_OVERLOAD			/* Placeholder for an overloaded function */
};

struct gcn_builtin_description
{
  int fcode;
  int icode;
  const char *name;
  enum gcn_builtin_type type;
  /* The first element of parm is always the return type.  The rest
     are a zero terminated list of parameters.  */
  int parm[6];
  gcn_builtin_expander expander;
};

/* Read in the GCN builtins from gcn-builtins.def.  */

extern GTY(()) struct gcn_builtin_description gcn_builtins[GCN_BUILTIN_MAX];

struct gcn_builtin_description gcn_builtins[] = {
#define DEF_BUILTIN(fcode, icode, name, type, params, expander)	\
  {GCN_BUILTIN_ ## fcode, icode, name, type, params, expander},

#define DEF_BUILTIN_BINOP_INT_FP(fcode, ic, name)			\
  {GCN_BUILTIN_ ## fcode ## _V64SI,					\
   CODE_FOR_ ## ic ##v64si3_exec, name "_v64int", B_INSN,		\
   {GCN_BTI_V64SI, GCN_BTI_EXEC, GCN_BTI_V64SI, GCN_BTI_V64SI,		\
    GCN_BTI_V64SI, GCN_BTI_END_OF_PARAMS}, gcn_expand_builtin_binop},	\
  {GCN_BUILTIN_ ## fcode ## _V64SI_unspec,				\
   CODE_FOR_ ## ic ##v64si3_exec, name "_v64int_unspec", B_INSN, 	\
   {GCN_BTI_V64SI, GCN_BTI_EXEC, GCN_BTI_V64SI, GCN_BTI_V64SI,		\
    GCN_BTI_END_OF_PARAMS}, gcn_expand_builtin_binop},

#include "gcn-builtins.def"
#undef DEF_BUILTIN_BINOP_INT_FP
#undef DEF_BUILTIN
};

static GTY(()) tree gcn_builtin_decls[GCN_BUILTIN_MAX];

/* Implement TARGET_BUILTIN_DECL.

   Return the GCN builtin for CODE.  */

tree
gcn_builtin_decl (unsigned code, bool ARG_UNUSED (initialize_p))
{
  if (code >= GCN_BUILTIN_MAX)
    return error_mark_node;

  return gcn_builtin_decls[code];
}

/* Helper function for gcn_init_builtins.  */

static void
gcn_init_builtin_types (void)
{
  gcn_builtin_types[GCN_BTI_VOID] = void_type_node;
  gcn_builtin_types[GCN_BTI_BOOL] = boolean_type_node;
  gcn_builtin_types[GCN_BTI_INT] = intSI_type_node;
  gcn_builtin_types[GCN_BTI_UINT] = unsigned_type_for (intSI_type_node);
  gcn_builtin_types[GCN_BTI_SIZE_T] = size_type_node;
  gcn_builtin_types[GCN_BTI_LLINT] = intDI_type_node;
  gcn_builtin_types[GCN_BTI_LLUINT] = unsigned_type_for (intDI_type_node);

  exec_type_node = unsigned_intDI_type_node;
  sf_type_node = float32_type_node;
  v64si_type_node = build_vector_type (intSI_type_node, 64);
  v64sf_type_node = build_vector_type (float_type_node, 64);
  v64df_type_node = build_vector_type (double_type_node, 64);
  v64ptr_type_node = build_vector_type (unsigned_intDI_type_node
					/*build_pointer_type
					  (integer_type_node) */
					, 64);
  tree tmp = build_distinct_type_copy (intSI_type_node);
  TYPE_ADDR_SPACE (tmp) = ADDR_SPACE_DEFAULT;
  siptr_type_node = build_pointer_type (tmp);

  tmp = build_distinct_type_copy (float_type_node);
  TYPE_ADDR_SPACE (tmp) = ADDR_SPACE_DEFAULT;
  sfptr_type_node = build_pointer_type (tmp);

  tmp = build_distinct_type_copy (void_type_node);
  TYPE_ADDR_SPACE (tmp) = ADDR_SPACE_DEFAULT;
  voidptr_type_node = build_pointer_type (tmp);

  tmp = build_distinct_type_copy (void_type_node);
  TYPE_ADDR_SPACE (tmp) = ADDR_SPACE_LDS;
  gcn_builtin_types[GCN_BTI_LDS_VOIDPTR] = build_pointer_type (tmp);
}

/* Implement TARGET_INIT_BUILTINS.

   Set up all builtin functions for this target.  */

static void
gcn_init_builtins (void)
{
  gcn_init_builtin_types ();

  struct gcn_builtin_description *d;
  unsigned int i;
  for (i = 0, d = gcn_builtins; i < GCN_BUILTIN_MAX; i++, d++)
    {
      tree p;
      char name[64];		/* build_function will make a copy.  */
      int parm;

      /* FIXME: Is this necessary/useful? */
      if (d->name == 0)
	continue;

      /* Find last parm.  */
      for (parm = 1; d->parm[parm] != GCN_BTI_END_OF_PARAMS; parm++)
	;

      p = void_list_node;
      while (parm > 1)
	p = tree_cons (NULL_TREE, gcn_builtin_types[d->parm[--parm]], p);

      p = build_function_type (gcn_builtin_types[d->parm[0]], p);

      sprintf (name, "__builtin_gcn_%s", d->name);
      gcn_builtin_decls[i]
	= add_builtin_function (name, p, i, BUILT_IN_MD, NULL, NULL_TREE);

      /* These builtins don't throw.  */
      TREE_NOTHROW (gcn_builtin_decls[i]) = 1;
    }

  /* These builtins need to take/return an LDS pointer: override the generic
     versions here.  */

  set_builtin_decl (BUILT_IN_GOACC_SINGLE_START,
		    gcn_builtin_decls[GCN_BUILTIN_ACC_SINGLE_START], false);

  set_builtin_decl (BUILT_IN_GOACC_SINGLE_COPY_START,
		    gcn_builtin_decls[GCN_BUILTIN_ACC_SINGLE_COPY_START],
		    false);

  set_builtin_decl (BUILT_IN_GOACC_SINGLE_COPY_END,
		    gcn_builtin_decls[GCN_BUILTIN_ACC_SINGLE_COPY_END],
		    false);

  set_builtin_decl (BUILT_IN_GOACC_BARRIER,
		    gcn_builtin_decls[GCN_BUILTIN_ACC_BARRIER], false);
}

/* Implement TARGET_INIT_LIBFUNCS.  */

static void
gcn_init_libfuncs (void)
{
  /* BITS_PER_UNIT * 2 is 64 bits, which causes
     optabs-libfuncs.cc:gen_int_libfunc to omit TImode (i.e 128 bits)
     libcalls that we need to support operations for that type.  Initialise
     them here instead.  */
  set_optab_libfunc (udiv_optab, TImode, "__udivti3");
  set_optab_libfunc (umod_optab, TImode, "__umodti3");
  set_optab_libfunc (sdiv_optab, TImode, "__divti3");
  set_optab_libfunc (smod_optab, TImode, "__modti3");
  set_optab_libfunc (smul_optab, TImode, "__multi3");
  set_optab_libfunc (addv_optab, TImode, "__addvti3");
  set_optab_libfunc (subv_optab, TImode, "__subvti3");
  set_optab_libfunc (negv_optab, TImode, "__negvti2");
  set_optab_libfunc (absv_optab, TImode, "__absvti2");
  set_optab_libfunc (smulv_optab, TImode, "__mulvti3");
  set_optab_libfunc (ffs_optab, TImode, "__ffsti2");
  set_optab_libfunc (clz_optab, TImode, "__clzti2");
  set_optab_libfunc (ctz_optab, TImode, "__ctzti2");
  set_optab_libfunc (clrsb_optab, TImode, "__clrsbti2");
  set_optab_libfunc (popcount_optab, TImode, "__popcountti2");
  set_optab_libfunc (parity_optab, TImode, "__parityti2");
  set_optab_libfunc (bswap_optab, TImode, "__bswapti2");

  set_optab_libfunc (sdivmod_optab, SImode, "__divmodsi4");
  set_optab_libfunc (udivmod_optab, SImode, "__udivmodsi4");
  set_optab_libfunc (sdivmod_optab, DImode, "__divmoddi4");
  set_optab_libfunc (udivmod_optab, DImode, "__udivmoddi4");

  set_optab_libfunc (sdiv_optab, V2QImode, "__divv2qi3");
  set_optab_libfunc (udiv_optab, V2QImode, "__udivv2qi3");
  set_optab_libfunc (smod_optab, V2QImode, "__modv2qi3");
  set_optab_libfunc (umod_optab, V2QImode, "__umodv2qi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V2QImode, "__divmodv2qi4");
  set_optab_libfunc (udivmod_optab, V2QImode, "__udivmodv2qi4");
#endif
  set_optab_libfunc (sdiv_optab, V4QImode, "__divv4qi3");
  set_optab_libfunc (udiv_optab, V4QImode, "__udivv4qi3");
  set_optab_libfunc (smod_optab, V4QImode, "__modv4qi3");
  set_optab_libfunc (umod_optab, V4QImode, "__umodv4qi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V4QImode, "__divmodv4qi4");
  set_optab_libfunc (udivmod_optab, V4QImode, "__udivmodv4qi4");
#endif
  set_optab_libfunc (sdiv_optab, V8QImode, "__divv8qi3");
  set_optab_libfunc (udiv_optab, V8QImode, "__udivv8qi3");
  set_optab_libfunc (smod_optab, V8QImode, "__modv8qi3");
  set_optab_libfunc (umod_optab, V8QImode, "__umodv8qi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V8QImode, "__divmodv8qi4");
  set_optab_libfunc (udivmod_optab, V8QImode, "__udivmodv8qi4");
#endif
  set_optab_libfunc (sdiv_optab, V16QImode, "__divv16qi3");
  set_optab_libfunc (udiv_optab, V16QImode, "__udivv16qi3");
  set_optab_libfunc (smod_optab, V16QImode, "__modv16qi3");
  set_optab_libfunc (umod_optab, V16QImode, "__umodv16qi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V16QImode, "__divmodv16qi4");
  set_optab_libfunc (udivmod_optab, V16QImode, "__udivmodv16qi4");
#endif
  set_optab_libfunc (sdiv_optab, V32QImode, "__divv32qi3");
  set_optab_libfunc (udiv_optab, V32QImode, "__udivv32qi3");
  set_optab_libfunc (smod_optab, V32QImode, "__modv32qi3");
  set_optab_libfunc (umod_optab, V32QImode, "__umodv32qi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V32QImode, "__divmodv32qi4");
  set_optab_libfunc (udivmod_optab, V32QImode, "__udivmodv32qi4");
#endif
  set_optab_libfunc (sdiv_optab, V64QImode, "__divv64qi3");
  set_optab_libfunc (udiv_optab, V64QImode, "__udivv64qi3");
  set_optab_libfunc (smod_optab, V64QImode, "__modv64qi3");
  set_optab_libfunc (umod_optab, V64QImode, "__umodv64qi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V64QImode, "__divmodv64qi4");
  set_optab_libfunc (udivmod_optab, V64QImode, "__udivmodv64qi4");
#endif

  set_optab_libfunc (sdiv_optab, V2HImode, "__divv2hi3");
  set_optab_libfunc (udiv_optab, V2HImode, "__udivv2hi3");
  set_optab_libfunc (smod_optab, V2HImode, "__modv2hi3");
  set_optab_libfunc (umod_optab, V2HImode, "__umodv2hi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V2HImode, "__divmodv2hi4");
  set_optab_libfunc (udivmod_optab, V2HImode, "__udivmodv2hi4");
#endif
  set_optab_libfunc (sdiv_optab, V4HImode, "__divv4hi3");
  set_optab_libfunc (udiv_optab, V4HImode, "__udivv4hi3");
  set_optab_libfunc (smod_optab, V4HImode, "__modv4hi3");
  set_optab_libfunc (umod_optab, V4HImode, "__umodv4hi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V4HImode, "__divmodv4hi4");
  set_optab_libfunc (udivmod_optab, V4HImode, "__udivmodv4hi4");
#endif
  set_optab_libfunc (sdiv_optab, V8HImode, "__divv8hi3");
  set_optab_libfunc (udiv_optab, V8HImode, "__udivv8hi3");
  set_optab_libfunc (smod_optab, V8HImode, "__modv8hi3");
  set_optab_libfunc (umod_optab, V8HImode, "__umodv8hi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V8HImode, "__divmodv8hi4");
  set_optab_libfunc (udivmod_optab, V8HImode, "__udivmodv8hi4");
#endif
  set_optab_libfunc (sdiv_optab, V16HImode, "__divv16hi3");
  set_optab_libfunc (udiv_optab, V16HImode, "__udivv16hi3");
  set_optab_libfunc (smod_optab, V16HImode, "__modv16hi3");
  set_optab_libfunc (umod_optab, V16HImode, "__umodv16hi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V16HImode, "__divmodv16hi4");
  set_optab_libfunc (udivmod_optab, V16HImode, "__udivmodv16hi4");
#endif
  set_optab_libfunc (sdiv_optab, V32HImode, "__divv32hi3");
  set_optab_libfunc (udiv_optab, V32HImode, "__udivv32hi3");
  set_optab_libfunc (smod_optab, V32HImode, "__modv32hi3");
  set_optab_libfunc (umod_optab, V32HImode, "__umodv32hi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V32HImode, "__divmodv32hi4");
  set_optab_libfunc (udivmod_optab, V32HImode, "__udivmodv32hi4");
#endif
  set_optab_libfunc (sdiv_optab, V64HImode, "__divv64hi3");
  set_optab_libfunc (udiv_optab, V64HImode, "__udivv64hi3");
  set_optab_libfunc (smod_optab, V64HImode, "__modv64hi3");
  set_optab_libfunc (umod_optab, V64HImode, "__umodv64hi3");
#if 0
  set_optab_libfunc (sdivmod_optab, V64HImode, "__divmodv64hi4");
  set_optab_libfunc (udivmod_optab, V64HImode, "__udivmodv64hi4");
#endif

  set_optab_libfunc (sdiv_optab, V2SImode, "__divv2si3");
  set_optab_libfunc (udiv_optab, V2SImode, "__udivv2si3");
  set_optab_libfunc (smod_optab, V2SImode, "__modv2si3");
  set_optab_libfunc (umod_optab, V2SImode, "__umodv2si3");
#if 0
  set_optab_libfunc (sdivmod_optab, V2SImode, "__divmodv2si4");
  set_optab_libfunc (udivmod_optab, V2SImode, "__udivmodv2si4");
#endif
  set_optab_libfunc (sdiv_optab, V4SImode, "__divv4si3");
  set_optab_libfunc (udiv_optab, V4SImode, "__udivv4si3");
  set_optab_libfunc (smod_optab, V4SImode, "__modv4si3");
  set_optab_libfunc (umod_optab, V4SImode, "__umodv4si3");
#if 0
  set_optab_libfunc (sdivmod_optab, V4SImode, "__divmodv4si4");
  set_optab_libfunc (udivmod_optab, V4SImode, "__udivmodv4si4");
#endif
  set_optab_libfunc (sdiv_optab, V8SImode, "__divv8si3");
  set_optab_libfunc (udiv_optab, V8SImode, "__udivv8si3");
  set_optab_libfunc (smod_optab, V8SImode, "__modv8si3");
  set_optab_libfunc (umod_optab, V8SImode, "__umodv8si3");
#if 0
  set_optab_libfunc (sdivmod_optab, V8SImode, "__divmodv8si4");
  set_optab_libfunc (udivmod_optab, V8SImode, "__udivmodv8si4");
#endif
  set_optab_libfunc (sdiv_optab, V16SImode, "__divv16si3");
  set_optab_libfunc (udiv_optab, V16SImode, "__udivv16si3");
  set_optab_libfunc (smod_optab, V16SImode, "__modv16si3");
  set_optab_libfunc (umod_optab, V16SImode, "__umodv16si3");
#if 0
  set_optab_libfunc (sdivmod_optab, V16SImode, "__divmodv16si4");
  set_optab_libfunc (udivmod_optab, V16SImode, "__udivmodv16si4");
#endif
  set_optab_libfunc (sdiv_optab, V32SImode, "__divv32si3");
  set_optab_libfunc (udiv_optab, V32SImode, "__udivv32si3");
  set_optab_libfunc (smod_optab, V32SImode, "__modv32si3");
  set_optab_libfunc (umod_optab, V32SImode, "__umodv32si3");
#if 0
  set_optab_libfunc (sdivmod_optab, V32SImode, "__divmodv32si4");
  set_optab_libfunc (udivmod_optab, V32SImode, "__udivmodv32si4");
#endif
  set_optab_libfunc (sdiv_optab, V64SImode, "__divv64si3");
  set_optab_libfunc (udiv_optab, V64SImode, "__udivv64si3");
  set_optab_libfunc (smod_optab, V64SImode, "__modv64si3");
  set_optab_libfunc (umod_optab, V64SImode, "__umodv64si3");
#if 0
  set_optab_libfunc (sdivmod_optab, V64SImode, "__divmodv64si4");
  set_optab_libfunc (udivmod_optab, V64SImode, "__udivmodv64si4");
#endif

  set_optab_libfunc (sdiv_optab, V2DImode, "__divv2di3");
  set_optab_libfunc (udiv_optab, V2DImode, "__udivv2di3");
  set_optab_libfunc (smod_optab, V2DImode, "__modv2di3");
  set_optab_libfunc (umod_optab, V2DImode, "__umodv2di3");
#if 0
  set_optab_libfunc (sdivmod_optab, V2DImode, "__divmodv2di4");
  set_optab_libfunc (udivmod_optab, V2DImode, "__udivmodv2di4");
#endif
  set_optab_libfunc (sdiv_optab, V4DImode, "__divv4di3");
  set_optab_libfunc (udiv_optab, V4DImode, "__udivv4di3");
  set_optab_libfunc (smod_optab, V4DImode, "__modv4di3");
  set_optab_libfunc (umod_optab, V4DImode, "__umodv4di3");
#if 0
  set_optab_libfunc (sdivmod_optab, V4DImode, "__divmodv4di4");
  set_optab_libfunc (udivmod_optab, V4DImode, "__udivmodv4di4");
#endif
  set_optab_libfunc (sdiv_optab, V8DImode, "__divv8di3");
  set_optab_libfunc (udiv_optab, V8DImode, "__udivv8di3");
  set_optab_libfunc (smod_optab, V8DImode, "__modv8di3");
  set_optab_libfunc (umod_optab, V8DImode, "__umodv8di3");
#if 0
  set_optab_libfunc (sdivmod_optab, V8DImode, "__divmodv8di4");
  set_optab_libfunc (udivmod_optab, V8DImode, "__udivmodv8di4");
#endif
  set_optab_libfunc (sdiv_optab, V16DImode, "__divv16di3");
  set_optab_libfunc (udiv_optab, V16DImode, "__udivv16di3");
  set_optab_libfunc (smod_optab, V16DImode, "__modv16di3");
  set_optab_libfunc (umod_optab, V16DImode, "__umodv16di3");
#if 0
  set_optab_libfunc (sdivmod_optab, V16DImode, "__divmodv16di4");
  set_optab_libfunc (udivmod_optab, V16DImode, "__udivmodv16di4");
#endif
  set_optab_libfunc (sdiv_optab, V32DImode, "__divv32di3");
  set_optab_libfunc (udiv_optab, V32DImode, "__udivv32di3");
  set_optab_libfunc (smod_optab, V32DImode, "__modv32di3");
  set_optab_libfunc (umod_optab, V32DImode, "__umodv32di3");
#if 0
  set_optab_libfunc (sdivmod_optab, V32DImode, "__divmodv32di4");
  set_optab_libfunc (udivmod_optab, V32DImode, "__udivmodv32di4");
#endif
  set_optab_libfunc (sdiv_optab, V64DImode, "__divv64di3");
  set_optab_libfunc (udiv_optab, V64DImode, "__udivv64di3");
  set_optab_libfunc (smod_optab, V64DImode, "__modv64di3");
  set_optab_libfunc (umod_optab, V64DImode, "__umodv64di3");
#if 0
  set_optab_libfunc (sdivmod_optab, V64DImode, "__divmodv64di4");
  set_optab_libfunc (udivmod_optab, V64DImode, "__udivmodv64di4");
#endif
}

/* Expand the CMP_SWAP GCN builtins.  We have our own versions that do
   not require taking the address of any object, other than the memory
   cell being operated on.

   Helper function for gcn_expand_builtin_1.  */

static rtx
gcn_expand_cmp_swap (tree exp, rtx target)
{
  machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
  addr_space_t as
    = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (CALL_EXPR_ARG (exp, 0))));
  machine_mode as_mode = gcn_addr_space_address_mode (as);

  if (!target)
    target = gen_reg_rtx (mode);

  rtx addr = expand_expr (CALL_EXPR_ARG (exp, 0),
			  NULL_RTX, as_mode, EXPAND_NORMAL);
  rtx cmp = expand_expr (CALL_EXPR_ARG (exp, 1),
			 NULL_RTX, mode, EXPAND_NORMAL);
  rtx src = expand_expr (CALL_EXPR_ARG (exp, 2),
			 NULL_RTX, mode, EXPAND_NORMAL);
  rtx pat;

  rtx mem = gen_rtx_MEM (mode, force_reg (as_mode, addr));
  set_mem_addr_space (mem, as);

  if (!REG_P (cmp))
    cmp = copy_to_mode_reg (mode, cmp);
  if (!REG_P (src))
    src = copy_to_mode_reg (mode, src);

  if (mode == SImode)
    pat = gen_sync_compare_and_swapsi (target, mem, cmp, src);
  else
    pat = gen_sync_compare_and_swapdi (target, mem, cmp, src);

  emit_insn (pat);

  return target;
}

/* Expand many different builtins.

   Intended for use in gcn-builtins.def.  */

static rtx
gcn_expand_builtin_1 (tree exp, rtx target, rtx /*subtarget */ ,
		      machine_mode /*mode */ , int ignore,
		      struct gcn_builtin_description *)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  switch (DECL_MD_FUNCTION_CODE (fndecl))
    {
    case GCN_BUILTIN_FLAT_LOAD_INT32:
      {
	if (ignore)
	  return target;
	/*rtx exec = */
	force_reg (DImode,
		   expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX, DImode,
				EXPAND_NORMAL));
	/*rtx ptr = */
	force_reg (V64DImode,
		   expand_expr (CALL_EXPR_ARG (exp, 1), NULL_RTX, V64DImode,
				EXPAND_NORMAL));
	/*emit_insn (gen_vector_flat_loadv64si
		     (target, gcn_gen_undef (V64SImode), ptr, exec)); */
	return target;
      }
    case GCN_BUILTIN_FLAT_LOAD_PTR_INT32:
    case GCN_BUILTIN_FLAT_LOAD_PTR_FLOAT:
      {
	if (ignore)
	  return target;
	rtx exec = force_reg (DImode,
			      expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					   DImode,
					   EXPAND_NORMAL));
	rtx ptr = force_reg (DImode,
			     expand_expr (CALL_EXPR_ARG (exp, 1), NULL_RTX,
					  V64DImode,
					  EXPAND_NORMAL));
	rtx offsets = force_reg (V64SImode,
				 expand_expr (CALL_EXPR_ARG (exp, 2),
					      NULL_RTX, V64DImode,
					      EXPAND_NORMAL));
	rtx addrs = gen_reg_rtx (V64DImode);
	rtx tmp = gen_reg_rtx (V64SImode);
	emit_insn (gen_ashlv64si3_exec (tmp, offsets,
					  GEN_INT (2),
					  gcn_gen_undef (V64SImode), exec));
	emit_insn (gen_addv64di3_zext_dup2_exec (addrs, tmp, ptr,
						 gcn_gen_undef (V64DImode),
						 exec));
	rtx mem = gen_rtx_MEM (GET_MODE (target), addrs);
	/*set_mem_addr_space (mem, ADDR_SPACE_FLAT); */
	/* FIXME: set attributes.  */
	emit_insn (gen_movvNm (target, mem, NULL, exec));
	return target;
      }
    case GCN_BUILTIN_FLAT_STORE_PTR_INT32:
    case GCN_BUILTIN_FLAT_STORE_PTR_FLOAT:
      {
	rtx exec = force_reg (DImode,
			      expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					   DImode,
					   EXPAND_NORMAL));
	rtx ptr = force_reg (DImode,
			     expand_expr (CALL_EXPR_ARG (exp, 1), NULL_RTX,
					  V64DImode,
					  EXPAND_NORMAL));
	rtx offsets = force_reg (V64SImode,
				 expand_expr (CALL_EXPR_ARG (exp, 2),
					      NULL_RTX, V64DImode,
					      EXPAND_NORMAL));
	machine_mode vmode = TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp,
								       3)));
	rtx val = force_reg (vmode,
			     expand_expr (CALL_EXPR_ARG (exp, 3), NULL_RTX,
					  vmode,
					  EXPAND_NORMAL));
	rtx addrs = gen_reg_rtx (V64DImode);
	rtx tmp = gen_reg_rtx (V64SImode);
	emit_insn (gen_ashlv64si3_exec (tmp, offsets,
					  GEN_INT (2),
					  gcn_gen_undef (V64SImode), exec));
	emit_insn (gen_addv64di3_zext_dup2_exec (addrs, tmp, ptr,
						 gcn_gen_undef (V64DImode),
						 exec));
	rtx mem = gen_rtx_MEM (vmode, addrs);
	/*set_mem_addr_space (mem, ADDR_SPACE_FLAT); */
	/* FIXME: set attributes.  */
	emit_insn (gen_movvNm (mem, val, NULL, exec));
	return target;
      }
    case GCN_BUILTIN_SQRTVF:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (V64SFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  V64SFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_sqrtv64sf2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_SQRTF:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (SFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  SFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_sqrtsf2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_FABSVF:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (V64SFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  V64SFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_absv64sf2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_FABSV:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (V64DFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  V64DFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_absv64df2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_FLOORVF:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (V64SFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  V64SFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_floorv64sf2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_FLOORV:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (V64DFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  V64DFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_floorv64df2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_LDEXPVF:
      {
	if (ignore)
	  return target;
	rtx arg1 = force_reg (V64SFmode,
			      expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					   V64SFmode,
					   EXPAND_NORMAL));
	rtx arg2 = force_reg (V64SImode,
			      expand_expr (CALL_EXPR_ARG (exp, 1), NULL_RTX,
					   V64SImode,
					   EXPAND_NORMAL));
	emit_insn (gen_ldexpv64sf3 (target, arg1, arg2));
	return target;
      }
    case GCN_BUILTIN_LDEXPV:
      {
	if (ignore)
	  return target;
	rtx arg1 = force_reg (V64DFmode,
			      expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					   V64DFmode,
					   EXPAND_NORMAL));
	rtx arg2 = force_reg (V64SImode,
			      expand_expr (CALL_EXPR_ARG (exp, 1), NULL_RTX,
					   V64SImode,
					   EXPAND_NORMAL));
	emit_insn (gen_ldexpv64df3 (target, arg1, arg2));
	return target;
      }
    case GCN_BUILTIN_FREXPVF_EXP:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (V64SFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  V64SFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_frexpv64sf_exp2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_FREXPVF_MANT:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (V64SFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  V64SFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_frexpv64sf_mant2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_FREXPV_EXP:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (V64DFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  V64DFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_frexpv64df_exp2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_FREXPV_MANT:
      {
	if (ignore)
	  return target;
	rtx arg = force_reg (V64DFmode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  V64DFmode,
					  EXPAND_NORMAL));
	emit_insn (gen_frexpv64df_mant2 (target, arg));
	return target;
      }
    case GCN_BUILTIN_OMP_DIM_SIZE:
      {
	if (ignore)
	  return target;
	emit_insn (gen_oacc_dim_size (target,
				      expand_expr (CALL_EXPR_ARG (exp, 0),
						   NULL_RTX, SImode,
						   EXPAND_NORMAL)));
	return target;
      }
    case GCN_BUILTIN_OMP_DIM_POS:
      {
	if (ignore)
	  return target;
	emit_insn (gen_oacc_dim_pos (target,
				     expand_expr (CALL_EXPR_ARG (exp, 0),
						  NULL_RTX, SImode,
						  EXPAND_NORMAL)));
	return target;
      }
    case GCN_BUILTIN_CMP_SWAP:
    case GCN_BUILTIN_CMP_SWAPLL:
      return gcn_expand_cmp_swap (exp, target);

    case GCN_BUILTIN_ACC_SINGLE_START:
      {
	if (ignore)
	  return target;

	rtx wavefront = gcn_oacc_dim_pos (1);
	rtx cond = gen_rtx_EQ (VOIDmode, wavefront, const0_rtx);
	rtx cc = (target && REG_P (target)) ? target : gen_reg_rtx (BImode);
	emit_insn (gen_cstoresi4 (cc, cond, wavefront, const0_rtx));
	return cc;
      }

    case GCN_BUILTIN_ACC_SINGLE_COPY_START:
      {
	rtx blk = force_reg (SImode,
			     expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
					  SImode, EXPAND_NORMAL));
	rtx wavefront = gcn_oacc_dim_pos (1);
	rtx cond = gen_rtx_NE (VOIDmode, wavefront, const0_rtx);
	rtx not_zero = gen_label_rtx ();
	emit_insn (gen_cbranchsi4 (cond, wavefront, const0_rtx, not_zero));
	emit_move_insn (blk, const0_rtx);
	emit_label (not_zero);
	return blk;
      }

    case GCN_BUILTIN_ACC_SINGLE_COPY_END:
      return target;

    case GCN_BUILTIN_ACC_BARRIER:
      emit_insn (gen_gcn_wavefront_barrier ());
      return target;

    case GCN_BUILTIN_GET_STACK_LIMIT:
      {
	/* stackbase = (stack_segment_decr & 0x0000ffffffffffff)
			+ stack_wave_offset);
	   seg_size = dispatch_ptr->private_segment_size;
	   stacklimit = stackbase + seg_size*64;
	   with segsize = *(uint32_t *) ((char *) dispatch_ptr
				       + 6*sizeof(int16_t) + 3*sizeof(int32_t));
	   cf. struct hsa_kernel_dispatch_packet_s in the HSA doc.  */
	rtx ptr;
	if (cfun->machine->args.reg[DISPATCH_PTR_ARG] >= 0
	    && cfun->machine->args.reg[KERNARG_SEGMENT_PTR_ARG] >= 0)
	  {
	    rtx num_waves_mem = gcn_oacc_dim_size (1);
	    rtx num_waves = gen_reg_rtx (SImode);
	    set_mem_addr_space (num_waves_mem, ADDR_SPACE_SCALAR_FLAT);
	    emit_move_insn (num_waves, num_waves_mem);

	    rtx workgroup_num = gcn_oacc_dim_pos (0);
	    rtx wave_num = gen_reg_rtx (SImode);
	    emit_move_insn(wave_num, gcn_oacc_dim_pos (1));

	    rtx thread_id = gen_reg_rtx (SImode);
	    emit_insn (gen_mulsi3 (thread_id, num_waves, workgroup_num));
	    emit_insn (gen_addsi3_scc (thread_id, thread_id, wave_num));

	    rtx kernarg_reg = gen_rtx_REG (DImode, cfun->machine->args.reg
					   [KERNARG_SEGMENT_PTR_ARG]);
	    rtx stack_size_mem = gen_rtx_MEM (SImode,
					      gen_rtx_PLUS (DImode,
							    kernarg_reg,
							    GEN_INT (52)));
	    set_mem_addr_space (stack_size_mem, ADDR_SPACE_SCALAR_FLAT);
	    rtx stack_size = gen_reg_rtx (SImode);
	    emit_move_insn (stack_size, stack_size_mem);

	    rtx wave_offset = gen_reg_rtx (SImode);
	    emit_insn (gen_mulsi3 (wave_offset, stack_size, thread_id));

	    rtx stack_limit_offset = gen_reg_rtx (SImode);
	    emit_insn (gen_addsi3 (stack_limit_offset, wave_offset,
				   stack_size));

	    rtx stack_limit_offset_di = gen_reg_rtx (DImode);
	    emit_move_insn (gen_rtx_SUBREG (SImode, stack_limit_offset_di, 4),
			    const0_rtx);
	    emit_move_insn (gen_rtx_SUBREG (SImode, stack_limit_offset_di, 0),
			    stack_limit_offset);

	    rtx stack_addr_mem = gen_rtx_MEM (DImode,
					      gen_rtx_PLUS (DImode,
							    kernarg_reg,
							    GEN_INT (40)));
	    set_mem_addr_space (stack_addr_mem, ADDR_SPACE_SCALAR_FLAT);
	    rtx stack_addr = gen_reg_rtx (DImode);
	    emit_move_insn (stack_addr, stack_addr_mem);

	    ptr = gen_rtx_PLUS (DImode, stack_addr, stack_limit_offset_di);
	  }
	else
	  {
	    ptr = gen_reg_rtx (DImode);
	    emit_move_insn (ptr, const0_rtx);
	  }
	return ptr;
      }
    case GCN_BUILTIN_KERNARG_PTR:
      {
	rtx ptr;
	if (cfun->machine->args.reg[KERNARG_SEGMENT_PTR_ARG] >= 0)
	   ptr = gen_rtx_REG (DImode,
			      cfun->machine->args.reg[KERNARG_SEGMENT_PTR_ARG]);
	else
	  {
	    ptr = gen_reg_rtx (DImode);
	    emit_move_insn (ptr, const0_rtx);
	  }
	return ptr;
      }
    case GCN_BUILTIN_DISPATCH_PTR:
      {
	rtx ptr;
	if (cfun->machine->args.reg[DISPATCH_PTR_ARG] >= 0)
	   ptr = gen_rtx_REG (DImode,
			      cfun->machine->args.reg[DISPATCH_PTR_ARG]);
	else
	  {
	    ptr = gen_reg_rtx (DImode);
	    emit_move_insn (ptr, const0_rtx);
	  }
	return ptr;
      }
    case GCN_BUILTIN_FIRST_CALL_THIS_THREAD_P:
      {
	/* Stash a marker in the unused upper 16 bits of QUEUE_PTR_ARG to
	   indicate whether it was the first call.  */
	rtx result = gen_reg_rtx (BImode);
	emit_move_insn (result, const0_rtx);
	if (cfun->machine->args.reg[QUEUE_PTR_ARG] >= 0)
	  {
	    rtx not_first = gen_label_rtx ();
	    rtx reg = gen_rtx_REG (DImode,
			cfun->machine->args.reg[QUEUE_PTR_ARG]);
	    reg = gcn_operand_part (DImode, reg, 1);
	    rtx cmp = force_reg (SImode,
				 gen_rtx_LSHIFTRT (SImode, reg, GEN_INT (16)));
	    emit_insn (gen_cstoresi4 (result, gen_rtx_NE (BImode, cmp,
							  GEN_INT(12345)),
				      cmp, GEN_INT(12345)));
	    emit_jump_insn (gen_cjump (not_first, gen_rtx_EQ (BImode, result,
							      const0_rtx),
				       result));
	    emit_move_insn (reg,
	      force_reg (SImode,
		gen_rtx_IOR (SImode,
			     gen_rtx_AND (SImode, reg, GEN_INT (0x0000ffff)),
			     GEN_INT (12345L << 16))));
	    emit_insn (gen_rtx_USE (VOIDmode, reg));
	    emit_label (not_first);
	  }
	return result;
      }
    default:
      gcc_unreachable ();
    }
}

/* Expansion of simple arithmetic and bit binary operation builtins.

   Intended for use with gcn_builtins table.  */

static rtx
gcn_expand_builtin_binop (tree exp, rtx target, rtx /*subtarget */ ,
			  machine_mode /*mode */ , int ignore,
			  struct gcn_builtin_description *d)
{
  int icode = d->icode;
  if (ignore)
    return target;

  rtx exec = force_reg (DImode,
			expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX, DImode,
				     EXPAND_NORMAL));

  machine_mode m1 = insn_data[icode].operand[1].mode;
  rtx arg1 = expand_expr (CALL_EXPR_ARG (exp, 1), NULL_RTX, m1,
			  EXPAND_NORMAL);
  if (!insn_data[icode].operand[1].predicate (arg1, m1))
    arg1 = force_reg (m1, arg1);

  machine_mode m2 = insn_data[icode].operand[2].mode;
  rtx arg2 = expand_expr (CALL_EXPR_ARG (exp, 2), NULL_RTX, m2,
			  EXPAND_NORMAL);
  if (!insn_data[icode].operand[2].predicate (arg2, m2))
    arg2 = force_reg (m2, arg2);

  rtx arg_prev;
  if (call_expr_nargs (exp) == 4)
    {
      machine_mode m_prev = insn_data[icode].operand[4].mode;
      arg_prev = force_reg (m_prev,
			    expand_expr (CALL_EXPR_ARG (exp, 3), NULL_RTX,
					 m_prev, EXPAND_NORMAL));
    }
  else
    arg_prev = gcn_gen_undef (GET_MODE (target));

  rtx pat = GEN_FCN (icode) (target, arg1, arg2, exec, arg_prev);
  emit_insn (pat);
  return target;
}

/* Implement TARGET_EXPAND_BUILTIN.

   Expand an expression EXP that calls a built-in function, with result going
   to TARGET if that's convenient (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
gcn_expand_builtin (tree exp, rtx target, rtx subtarget, machine_mode mode,
		    int ignore)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_MD_FUNCTION_CODE (fndecl);
  struct gcn_builtin_description *d;

  gcc_assert (fcode < GCN_BUILTIN_MAX);
  d = &gcn_builtins[fcode];

  if (d->type == B_UNIMPLEMENTED)
    sorry ("Builtin not implemented");

  return d->expander (exp, target, subtarget, mode, ignore, d);
}

/* }}}  */
/* {{{ Vectorization.  */

/* Implement TARGET_VECTORIZE_GET_MASK_MODE.

   A vector mask is a value that holds one boolean result for every element in
   a vector.  */

opt_machine_mode
gcn_vectorize_get_mask_mode (machine_mode)
{
  /* GCN uses a DImode bit-mask.  */
  return DImode;
}

/* Return an RTX that references a vector with the i-th lane containing
   PERM[i]*4.

   Helper function for gcn_vectorize_vec_perm_const.  */

static rtx
gcn_make_vec_perm_address (unsigned int *perm, int nelt)
{
  machine_mode mode = VnMODE (nelt, SImode);
  rtx x = gen_reg_rtx (mode);
  emit_move_insn (x, gcn_vec_constant (mode, 0));

  /* Permutation addresses use byte addressing.  With each vector lane being
     4 bytes wide, and with 64 lanes in total, only bits 2..7 are significant,
     so only set those.

     The permutation given to the vec_perm* patterns range from 0 to 2N-1 to
     select between lanes in two vectors, but as the DS_BPERMUTE* instructions
     only take one source vector, the most-significant bit can be ignored
     here.  Instead, we can use EXEC masking to select the relevant part of
     each source vector after they are permuted separately.  */
  uint64_t bit_mask = 1 << 2;
  for (int i = 2; i < 8; i++, bit_mask <<= 1)
    {
      uint64_t exec_mask = 0;
      uint64_t lane_mask = 1;
      for (int j = 0; j < nelt; j++, lane_mask <<= 1)
	if (((perm[j] % nelt) * 4) & bit_mask)
	  exec_mask |= lane_mask;

      if (exec_mask)
	emit_insn (gen_addvNsi3 (x, x, gcn_vec_constant (mode, bit_mask),
				 x, get_exec (exec_mask)));
    }

  return x;
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST.

   Return true if permutation with SEL is possible.

   If DST/SRC0/SRC1 are non-null, emit the instructions to perform the
   permutations.  */

static bool
gcn_vectorize_vec_perm_const (machine_mode vmode, machine_mode op_mode,
			      rtx dst, rtx src0, rtx src1,
			      const vec_perm_indices & sel)
{
  if (vmode != op_mode
      || !VECTOR_MODE_P (vmode)
      || GET_MODE_INNER (vmode) == TImode)
    return false;

  unsigned int nelt = GET_MODE_NUNITS (vmode);

  gcc_assert (VECTOR_MODE_P (vmode));
  gcc_assert (nelt <= 64);
  gcc_assert (sel.length () == nelt);

  unsigned int perm[64];
  for (unsigned int i = 0; i < nelt; ++i)
    perm[i] = sel[i] & (2 * nelt - 1);
  for (unsigned int i = nelt; i < 64; ++i)
    perm[i] = 0;

  /* RDNA devices can only do permutations within each group of 32-lanes.
     Reject permutations that cross the boundary.  */
  if (TARGET_WAVE64_COMPAT)
    for (unsigned int i = 0; i < nelt; i++)
      if (i < 32 ? (perm[i] % nelt) > 31 : (perm[i] % nelt) < 32)
	return false;

  /* All vector permutations are possible on other architectures,
     with varying degrees of efficiency depending on the permutation. */
  if (!dst)
    return true;

  src0 = force_reg (vmode, src0);
  src1 = force_reg (vmode, src1);

  /* Make life a bit easier by swapping operands if necessary so that
     the first element always comes from src0.  */
  if (perm[0] >= nelt)
    {
      std::swap (src0, src1);

      for (unsigned int i = 0; i < nelt; ++i)
	if (perm[i] < nelt)
	  perm[i] += nelt;
	else
	  perm[i] -= nelt;
    }

  /* TODO: There are more efficient ways to implement certain permutations
     using ds_swizzle_b32 and/or DPP.  Test for and expand them here, before
     this more inefficient generic approach is used.  */

  int64_t src1_lanes = 0;
  int64_t lane_bit = 1;

  for (unsigned int i = 0; i < nelt; ++i, lane_bit <<= 1)
    {
      /* Set the bits for lanes from src1.  */
      if (perm[i] >= nelt)
	src1_lanes |= lane_bit;
    }

  rtx addr = gcn_make_vec_perm_address (perm, nelt);

  /* Load elements from src0 to dst.  */
  gcc_assert ((~src1_lanes) & (0xffffffffffffffffUL > (64-nelt)));
  emit_insn (gen_ds_bpermutevNm (dst, addr, src0, get_exec (vmode)));

  /* Load elements from src1 to dst.  */
  if (src1_lanes)
    {
      /* Masking a lane masks both the destination and source lanes for
         DS_BPERMUTE, so we need to have all lanes enabled for the permute,
         then add an extra masked move to merge the results of permuting
         the two source vectors together.
       */
      rtx tmp = gen_reg_rtx (vmode);
      emit_insn (gen_ds_bpermutevNm (tmp, addr, src1, get_exec (vmode)));
      emit_insn (gen_movvNm (dst, tmp, dst, get_exec (src1_lanes)));
    }

  return true;
}

/* Implements TARGET_VECTOR_MODE_SUPPORTED_P.

   Return nonzero if vector MODE is supported with at least move
   instructions.  */

static bool
gcn_vector_mode_supported_p (machine_mode mode)
{
  return (mode == V64QImode || mode == V64HImode
	  || mode == V64SImode || mode == V64DImode
	  || mode == V64SFmode || mode == V64DFmode
	  || mode == V32QImode || mode == V32HImode
	  || mode == V32SImode || mode == V32DImode
	  || mode == V32SFmode || mode == V32DFmode
	  || mode == V16QImode || mode == V16HImode
	  || mode == V16SImode || mode == V16DImode
	  || mode == V16SFmode || mode == V16DFmode
	  || mode == V8QImode || mode == V8HImode
	  || mode == V8SImode || mode == V8DImode
	  || mode == V8SFmode || mode == V8DFmode
	  || mode == V4QImode || mode == V4HImode
	  || mode == V4SImode || mode == V4DImode
	  || mode == V4SFmode || mode == V4DFmode
	  || mode == V2QImode || mode == V2HImode
	  || mode == V2SImode || mode == V2DImode
	  || mode == V2SFmode || mode == V2DFmode
	  /* TImode vectors are allowed to exist for divmod, but there
	     are almost no instructions defined for them, and the
	     autovectorizer does not use them.  */
	  || mode == V64TImode || mode == V32TImode
	  || mode == V16TImode || mode == V8TImode
	  || mode == V4TImode || mode == V2TImode);
}

/* Implement TARGET_VECTORIZE_PREFERRED_SIMD_MODE.

   Enables autovectorization for all supported modes.  */

static machine_mode
gcn_vectorize_preferred_simd_mode (scalar_mode mode)
{
  bool v32;
  if (gcn_preferred_vectorization_factor == 32)
    v32 = true;
  else if (gcn_preferred_vectorization_factor == 64)
    v32 = false;
  else if (gcn_preferred_vectorization_factor != -1)
    gcc_unreachable ();
  else if (TARGET_WAVE64_COMPAT)
  /* RDNA devices have 32-lane vectors with limited support for 64-bit vectors
     (in particular, permute operations are only available for cases that don't
     span the 32-lane boundary).

     From the RDNA3 manual: "Hardware may choose to skip either half if the
     EXEC mask for that half is all zeros...". This means that preferring
     32-lanes is a good stop-gap until we have proper wave32 support.  */
    v32 = true;
  else
    v32 = false;

  if (v32)
    switch (mode)
      {
      case E_QImode:
	return V32QImode;
      case E_HImode:
	return V32HImode;
      case E_SImode:
	return V32SImode;
      case E_DImode:
	return V32DImode;
      case E_SFmode:
	return V32SFmode;
      case E_DFmode:
	return V32DFmode;
      default:
	return word_mode;
      }

  switch (mode)
    {
    case E_QImode:
      return V64QImode;
    case E_HImode:
      return V64HImode;
    case E_SImode:
      return V64SImode;
    case E_DImode:
      return V64DImode;
    case E_SFmode:
      return V64SFmode;
    case E_DFmode:
      return V64DFmode;
    default:
      return word_mode;
    }
}

/* Implement TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES.

   Try all the vector modes.  */

unsigned int gcn_autovectorize_vector_modes (vector_modes *modes,
					     bool ARG_UNUSED (all))
{
  modes->safe_push (V64QImode);
  modes->safe_push (V64HImode);
  modes->safe_push (V64SImode);
  modes->safe_push (V64SFmode);
  modes->safe_push (V64DImode);
  modes->safe_push (V64DFmode);

  modes->safe_push (V32QImode);
  modes->safe_push (V32HImode);
  modes->safe_push (V32SImode);
  modes->safe_push (V32SFmode);
  modes->safe_push (V32DImode);
  modes->safe_push (V32DFmode);

  modes->safe_push (V16QImode);
  modes->safe_push (V16HImode);
  modes->safe_push (V16SImode);
  modes->safe_push (V16SFmode);
  modes->safe_push (V16DImode);
  modes->safe_push (V16DFmode);

  modes->safe_push (V8QImode);
  modes->safe_push (V8HImode);
  modes->safe_push (V8SImode);
  modes->safe_push (V8SFmode);
  modes->safe_push (V8DImode);
  modes->safe_push (V8DFmode);

  modes->safe_push (V4QImode);
  modes->safe_push (V4HImode);
  modes->safe_push (V4SImode);
  modes->safe_push (V4SFmode);
  modes->safe_push (V4DImode);
  modes->safe_push (V4DFmode);

  modes->safe_push (V2QImode);
  modes->safe_push (V2HImode);
  modes->safe_push (V2SImode);
  modes->safe_push (V2SFmode);
  modes->safe_push (V2DImode);
  modes->safe_push (V2DFmode);

  /* We shouldn't need VECT_COMPARE_COSTS as they should all cost the same.  */
  return 0;
}

/* Implement TARGET_VECTORIZE_RELATED_MODE.

   All GCN vectors are 64-lane, so this is simpler than other architectures.
   In particular, we do *not* want to match vector bit-size.  */

static opt_machine_mode
gcn_related_vector_mode (machine_mode vector_mode,
			 scalar_mode element_mode, poly_uint64 nunits)
{
  int n = nunits.to_constant ();

  if (n == 0)
    n = GET_MODE_NUNITS (vector_mode);

  return VnMODE (n, element_mode);
}

/* Implement TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT.

   Returns the preferred alignment in bits for accesses to vectors of type type
   in vectorized code. This might be less than or greater than the ABI-defined
   value returned by TARGET_VECTOR_ALIGNMENT. It can be equal to the alignment
   of a single element, in which case the vectorizer will not try to optimize
   for alignment.  */

static poly_uint64
gcn_preferred_vector_alignment (const_tree type)
{
  return TYPE_ALIGN (TREE_TYPE (type));
}

/* Implement TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT.

   Return true if the target supports misaligned vector store/load of a
   specific factor denoted in the misalignment parameter.  */

static bool
gcn_vectorize_support_vector_misalignment (machine_mode ARG_UNUSED (mode),
					   const_tree type, int misalignment,
					   bool is_packed)
{
  if (is_packed)
    return false;

  /* If the misalignment is unknown, we should be able to handle the access
     so long as it is not to a member of a packed data structure.  */
  if (misalignment == -1)
    return true;

  /* Return true if the misalignment is a multiple of the natural alignment
     of the vector's element type.  This is probably always going to be
     true in practice, since we've already established that this isn't a
     packed access.  */
  return misalignment % TYPE_ALIGN_UNIT (type) == 0;
}

/* Implement TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE.

   Return true if vector alignment is reachable (by peeling N iterations) for
   the given scalar type TYPE.  */

static bool
gcn_vector_alignment_reachable (const_tree ARG_UNUSED (type), bool is_packed)
{
  /* Vectors which aren't in packed structures will not be less aligned than
     the natural alignment of their element type, so this is safe.  */
  return !is_packed;
}

/* Generate DPP pairwise swap instruction.
   This instruction swaps the values in each even lane with the value in the
   next one:
     a, b, c, d -> b, a, d, c.
   The opcode is given by INSN.  */

char *
gcn_expand_dpp_swap_pairs_insn (machine_mode mode, const char *insn,
				int ARG_UNUSED (unspec))
{
  static char buf[128];
  const char *dpp;

  /* Add the DPP modifiers.  */
  dpp = "quad_perm:[1,0,3,2]";

  if (vgpr_2reg_mode_p (mode))
    sprintf (buf, "%s\t%%L0, %%L1 %s\n\t%s\t%%H0, %%H1 %s",
	     insn, dpp, insn, dpp);
  else
    sprintf (buf, "%s\t%%0, %%1 %s", insn, dpp);

  return buf;
}

/* Generate DPP distribute even instruction.
   This instruction copies the value in each even lane to the next one:
     a, b, c, d -> a, a, c, c.
   The opcode is given by INSN.  */

char *
gcn_expand_dpp_distribute_even_insn (machine_mode mode, const char *insn,
				     int ARG_UNUSED (unspec))
{
  static char buf[128];
  const char *dpp;

  /* Add the DPP modifiers.  */
  dpp = "quad_perm:[0,0,2,2]";

  if (vgpr_2reg_mode_p (mode))
    sprintf (buf, "%s\t%%L0, %%L1 %s\n\t%s\t%%H0, %%H1 %s",
	     insn, dpp, insn, dpp);
  else
    sprintf (buf, "%s\t%%0, %%1 %s", insn, dpp);

  return buf;
}

/* Generate DPP distribute odd instruction.
   This isntruction copies the value in each odd lane to the previous one:
     a, b, c, d -> b, b, d, d.
   The opcode is given by INSN.  */

char *
gcn_expand_dpp_distribute_odd_insn (machine_mode mode, const char *insn,
				    int ARG_UNUSED (unspec))
{
  static char buf[128];
  const char *dpp;

  /* Add the DPP modifiers.  */
  dpp = "quad_perm:[1,1,3,3]";

  if (vgpr_2reg_mode_p (mode))
    sprintf (buf, "%s\t%%L0, %%L1 %s\n\t%s\t%%H0, %%H1 %s",
	     insn, dpp, insn, dpp);
  else
    sprintf (buf, "%s\t%%0, %%1 %s", insn, dpp);

  return buf;
}

/* Generate DPP instructions used for vector reductions.

   The opcode is given by INSN.
   The first operand of the operation is shifted right by SHIFT vector lanes.
   SHIFT must be a power of 2.  If SHIFT is 16, the 15th lane of each row is
   broadcast the next row (thereby acting like a shift of 16 for the end of
   each row).  If SHIFT is 32, lane 31 is broadcast to all the
   following lanes (thereby acting like a shift of 32 for lane 63).  */

char *
gcn_expand_dpp_shr_insn (machine_mode mode, const char *insn,
			 int unspec, int shift)
{
  gcc_checking_assert (TARGET_DPP_FULL);

  static char buf[128];
  const char *dpp;
  const char *vcc_in = "";
  const char *vcc_out = "";

  /* Add the vcc operand if needed.  */
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
    {
      if (unspec == UNSPEC_PLUS_CARRY_IN_DPP_SHR)
	vcc_in = ", vcc";

      if (unspec == UNSPEC_PLUS_CARRY_DPP_SHR
	  || unspec == UNSPEC_PLUS_CARRY_IN_DPP_SHR)
	vcc_out = ", vcc";
    }

  /* Add the DPP modifiers.  */
  switch (shift)
    {
    case 1:
      dpp = "row_shr:1 bound_ctrl:0";
      break;
    case 2:
      dpp = "row_shr:2 bound_ctrl:0";
      break;
    case 4:
      dpp = "row_shr:4 bank_mask:0xe";
      break;
    case 8:
      dpp = "row_shr:8 bank_mask:0xc";
      break;
    case 16:
      dpp = "row_bcast:15 row_mask:0xa";
      break;
    case 32:
      dpp = "row_bcast:31 row_mask:0xc";
      break;
    default:
      gcc_unreachable ();
    }

  if (unspec == UNSPEC_MOV_DPP_SHR && vgpr_2reg_mode_p (mode))
    sprintf (buf, "%s\t%%L0, %%L1 %s\n\t%s\t%%H0, %%H1 %s",
	     insn, dpp, insn, dpp);
  else if (unspec == UNSPEC_MOV_DPP_SHR)
    sprintf (buf, "%s\t%%0, %%1 %s", insn, dpp);
  else
    sprintf (buf, "%s\t%%0%s, %%1, %%2%s %s", insn, vcc_out, vcc_in, dpp);

  return buf;
}

/* Generate vector reductions in terms of DPP instructions.

   The vector register SRC of mode MODE is reduced using the operation given
   by UNSPEC, and the scalar result is returned in lane 63 of a vector
   register (or lane 31, 15, 7, 3, 1 for partial vectors).  */

rtx
gcn_expand_reduc_scalar (machine_mode mode, rtx src, int unspec)
{
  gcc_checking_assert (TARGET_DPP_FULL);

  machine_mode orig_mode = mode;
  machine_mode scalar_mode = GET_MODE_INNER (mode);
  int vf = GET_MODE_NUNITS (mode);
  bool use_moves = (((unspec == UNSPEC_SMIN_DPP_SHR
		      || unspec == UNSPEC_SMAX_DPP_SHR
		      || unspec == UNSPEC_UMIN_DPP_SHR
		      || unspec == UNSPEC_UMAX_DPP_SHR)
		     && (scalar_mode == DImode
			 || scalar_mode == DFmode))
		    || (unspec == UNSPEC_PLUS_DPP_SHR
			&& scalar_mode == DFmode));
  rtx_code code = (unspec == UNSPEC_SMIN_DPP_SHR ? SMIN
		   : unspec == UNSPEC_SMAX_DPP_SHR ? SMAX
		   : unspec == UNSPEC_UMIN_DPP_SHR ? UMIN
		   : unspec == UNSPEC_UMAX_DPP_SHR ? UMAX
		   : unspec == UNSPEC_PLUS_DPP_SHR ? PLUS
		   : UNKNOWN);
  bool use_extends = ((unspec == UNSPEC_SMIN_DPP_SHR
		       || unspec == UNSPEC_SMAX_DPP_SHR
		       || unspec == UNSPEC_UMIN_DPP_SHR
		       || unspec == UNSPEC_UMAX_DPP_SHR)
		      && (scalar_mode == QImode
			  || scalar_mode == HImode));
  bool unsignedp = (unspec == UNSPEC_UMIN_DPP_SHR
		    || unspec == UNSPEC_UMAX_DPP_SHR);
  bool use_plus_carry = unspec == UNSPEC_PLUS_DPP_SHR
			&& GET_MODE_CLASS (mode) == MODE_VECTOR_INT
			&& scalar_mode == DImode;

  if (use_plus_carry)
    unspec = UNSPEC_PLUS_CARRY_DPP_SHR;

  if (use_extends)
    {
      mode = VnMODE (vf, SImode);
      rtx tmp = gen_reg_rtx (mode);
      convert_move (tmp, src, unsignedp);
      src = tmp;
    }

  /* Perform reduction by first performing the reduction operation on every
     pair of lanes, then on every pair of results from the previous
     iteration (thereby effectively reducing every 4 lanes) and so on until
     all lanes are reduced.  */
  rtx in, out = force_reg (mode, src);
  int iterations = exact_log2 (vf);
  for (int i = 0, shift = 1; i < iterations; i++, shift <<= 1)
    {
      rtx shift_val = gen_rtx_CONST_INT (VOIDmode, shift);
      in = out;
      out = gen_reg_rtx (mode);

      if (use_moves)
	{
	  rtx tmp = gen_reg_rtx (mode);
	  emit_insn (gen_dpp_move (mode, tmp, in, shift_val));
	  rtx insn = gen_rtx_SET (out, gen_rtx_fmt_ee (code, mode, tmp, in));
	  if (scalar_mode == DImode)
	    {
	      rtx clobber = gen_rtx_CLOBBER (VOIDmode,
					     gen_rtx_REG (DImode, VCC_REG));
	      insn = gen_rtx_PARALLEL (VOIDmode,
				       gen_rtvec (2, insn, clobber));
	    }
	  emit_insn (insn);
	}
      else
	{
	  rtx insn = gen_rtx_SET (out,
				  gen_rtx_UNSPEC (mode,
						  gen_rtvec (3, in, in,
							     shift_val),
						  unspec));

	  /* Add clobber for instructions that set the carry flags.  */
	  if (use_plus_carry)
	    {
	      rtx clobber = gen_rtx_CLOBBER (VOIDmode,
					     gen_rtx_REG (DImode, VCC_REG));
	      insn = gen_rtx_PARALLEL (VOIDmode,
				       gen_rtvec (2, insn, clobber));
	    }

	  emit_insn (insn);
	}
    }

  if (use_extends)
    {
      rtx tmp = gen_reg_rtx (orig_mode);
      convert_move (tmp, out, unsignedp);
      out = tmp;
    }

  return out;
}

/* Implement TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST.  */

int
gcn_vectorization_cost (enum vect_cost_for_stmt ARG_UNUSED (type_of_cost),
			tree ARG_UNUSED (vectype), int ARG_UNUSED (misalign))
{
  /* Always vectorize.  */
  return 1;
}

/* Implement TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN.  */

static int
gcn_simd_clone_compute_vecsize_and_simdlen (struct cgraph_node *ARG_UNUSED (node),
					    struct cgraph_simd_clone *clonei,
					    tree ARG_UNUSED (base_type),
					    int ARG_UNUSED (num),
					    bool explicit_p)
{
  if (known_eq (clonei->simdlen, 0U))
    clonei->simdlen = 64;
  else if (maybe_ne (clonei->simdlen, 64U))
    {
      /* Note that x86 has a similar message that is likely to trigger on
	 sizes that are OK for gcn; the user can't win.  */
      if (explicit_p)
	warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		    "unsupported simdlen %wd (amdgcn)",
		    clonei->simdlen.to_constant ());
      return 0;
    }

  clonei->vecsize_mangle = 'n';
  clonei->vecsize_int = 0;
  clonei->vecsize_float = 0;

  /* DImode ought to be more natural here, but VOIDmode produces better code,
     at present, due to the shift-and-test steps not being optimized away
     inside the in-branch clones.  */
  clonei->mask_mode = VOIDmode;

  return 1;
}

/* Implement TARGET_SIMD_CLONE_ADJUST.  */

static void
gcn_simd_clone_adjust (struct cgraph_node *ARG_UNUSED (node))
{
  /* This hook has to be defined when
     TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN is defined, but we don't
     need it to do anything yet.  */
}

/* Implement TARGET_SIMD_CLONE_USABLE.  */

static int
gcn_simd_clone_usable (struct cgraph_node *ARG_UNUSED (node),
		       machine_mode ARG_UNUSED (vector_mode))
{
  /* We don't need to do anything here because
     gcn_simd_clone_compute_vecsize_and_simdlen currently only returns one
     possibility.  */
  return 0;
}

tree mathfn_built_in_explicit (tree, combined_fn);

/* Implement TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION.
   Return the function declaration of the vectorized version of the builtin
   in the math library if available.  */

tree
gcn_vectorize_builtin_vectorized_function (unsigned int fn, tree type_out,
					   tree type_in)
{
  if (TREE_CODE (type_out) != VECTOR_TYPE
      || TREE_CODE (type_in) != VECTOR_TYPE)
    return NULL_TREE;

  machine_mode out_mode = TYPE_MODE (TREE_TYPE (type_out));
  int out_n = TYPE_VECTOR_SUBPARTS (type_out);
  combined_fn cfn = combined_fn (fn);

  /* Keep this consistent with the list of vectorized math routines.  */
  int implicit_p;
  switch (fn)
    {
    CASE_CFN_ACOS:
    CASE_CFN_ACOSH:
    CASE_CFN_ASIN:
    CASE_CFN_ASINH:
    CASE_CFN_ATAN:
    CASE_CFN_ATAN2:
    CASE_CFN_ATANH:
    CASE_CFN_COPYSIGN:
    CASE_CFN_COS:
    CASE_CFN_COSH:
    CASE_CFN_ERF:
    CASE_CFN_EXP:
    CASE_CFN_EXP2:
    CASE_CFN_FINITE:
    CASE_CFN_FMOD:
    CASE_CFN_GAMMA:
    CASE_CFN_HYPOT:
    CASE_CFN_ISNAN:
    CASE_CFN_LGAMMA:
    CASE_CFN_LOG:
    CASE_CFN_LOG10:
    CASE_CFN_LOG2:
    CASE_CFN_POW:
    CASE_CFN_REMAINDER:
    CASE_CFN_RINT:
    CASE_CFN_SIN:
    CASE_CFN_SINH:
    CASE_CFN_SQRT:
    CASE_CFN_TAN:
    CASE_CFN_TANH:
    CASE_CFN_TGAMMA:
      implicit_p = 1;
      break;

    CASE_CFN_SCALB:
    CASE_CFN_SIGNIFICAND:
      implicit_p = 0;
      break;

    default:
      return NULL_TREE;
    }

  tree out_t_node = (out_mode == DFmode) ? double_type_node : float_type_node;
  tree fndecl = implicit_p ? mathfn_built_in (out_t_node, cfn)
			   : mathfn_built_in_explicit (out_t_node, cfn);

  const char *bname = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  char name[20];
  sprintf (name, out_mode == DFmode ? "v%ddf_%s" : "v%dsf_%s",
	   out_n, bname + 10);

  unsigned arity = 0;
  for (tree args = DECL_ARGUMENTS (fndecl); args; args = TREE_CHAIN (args))
    arity++;

  tree fntype = (arity == 1)
		? build_function_type_list (type_out, type_in, NULL)
		: build_function_type_list (type_out, type_in, type_in, NULL);

  /* Build a function declaration for the vectorized function.  */
  tree new_fndecl = build_decl (BUILTINS_LOCATION,
				FUNCTION_DECL, get_identifier (name), fntype);
  TREE_PUBLIC (new_fndecl) = 1;
  DECL_EXTERNAL (new_fndecl) = 1;
  DECL_IS_NOVOPS (new_fndecl) = 1;
  TREE_READONLY (new_fndecl) = 1;

  return new_fndecl;
}

/* Implement TARGET_LIBC_HAS_FUNCTION.  */

bool
gcn_libc_has_function (enum function_class fn_class,
		       tree type)
{
  return bsd_libc_has_function (fn_class, type);
}

/* }}}  */
/* {{{ md_reorg pass.  */

/* Identify VMEM instructions from their "type" attribute.  */

static bool
gcn_vmem_insn_p (attr_type type)
{
  switch (type)
    {
    case TYPE_MUBUF:
    case TYPE_MTBUF:
    case TYPE_FLAT:
    case TYPE_VOP3P_MAI:
      return true;
    case TYPE_UNKNOWN:
    case TYPE_SOP1:
    case TYPE_SOP2:
    case TYPE_SOPK:
    case TYPE_SOPC:
    case TYPE_SOPP:
    case TYPE_SMEM:
    case TYPE_DS:
    case TYPE_VOP2:
    case TYPE_VOP1:
    case TYPE_VOPC:
    case TYPE_VOP3A:
    case TYPE_VOP3B:
    case TYPE_VOP_SDWA:
    case TYPE_VOP_DPP:
    case TYPE_MULT:
    case TYPE_VMULT:
      return false;
    }
  gcc_unreachable ();
  return false;
}

/* If INSN sets the EXEC register to a constant value, return the value,
   otherwise return zero.  */

static int64_t
gcn_insn_exec_value (rtx_insn *insn)
{
  if (!NONDEBUG_INSN_P (insn))
    return 0;

  rtx pattern = PATTERN (insn);

  if (GET_CODE (pattern) == SET)
    {
      rtx dest = XEXP (pattern, 0);
      rtx src = XEXP (pattern, 1);

      if (GET_MODE (dest) == DImode
	  && REG_P (dest) && REGNO (dest) == EXEC_REG
	  && CONST_INT_P (src))
	return INTVAL (src);
    }

  return 0;
}

/* Sets the EXEC register before INSN to the value that it had after
   LAST_EXEC_DEF.  The constant value of the EXEC register is returned if
   known, otherwise it returns zero.  */

static int64_t
gcn_restore_exec (rtx_insn *insn, rtx_insn *last_exec_def, int64_t curr_exec,
		  bool curr_exec_known, bool &last_exec_def_saved)
{
  rtx exec_reg = gen_rtx_REG (DImode, EXEC_REG);
  rtx exec;

  int64_t exec_value = gcn_insn_exec_value (last_exec_def);

  if (exec_value)
    {
      /* If the EXEC value is a constant and it happens to be the same as the
         current EXEC value, the restore can be skipped.  */
      if (curr_exec_known && exec_value == curr_exec)
	return exec_value;

      exec = GEN_INT (exec_value);
    }
  else
    {
      /* If the EXEC value is not a constant, save it in a register after the
	 point of definition.  */
      rtx exec_save_reg = gen_rtx_REG (DImode, EXEC_SAVE_REG);

      if (!last_exec_def_saved)
	{
	  start_sequence ();
	  emit_move_insn (exec_save_reg, exec_reg);
	  rtx_insn *seq = get_insns ();
	  end_sequence ();

	  emit_insn_after (seq, last_exec_def);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Saving EXEC after insn %d.\n",
		     INSN_UID (last_exec_def));

	  last_exec_def_saved = true;
	}

      exec = exec_save_reg;
    }

  /* Restore EXEC register before the usage.  */
  start_sequence ();
  emit_move_insn (exec_reg, exec);
  rtx_insn *seq = get_insns ();
  end_sequence ();
  emit_insn_before (seq, insn);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (exec_value)
	fprintf (dump_file, "Restoring EXEC to %ld before insn %d.\n",
		 exec_value, INSN_UID (insn));
      else
	fprintf (dump_file,
		 "Restoring EXEC from saved value before insn %d.\n",
		 INSN_UID (insn));
    }

  return exec_value;
}

/* Implement TARGET_MACHINE_DEPENDENT_REORG.

   Ensure that pipeline dependencies and lane masking are set correctly.  */

static void
gcn_md_reorg (void)
{
  basic_block bb;
  rtx exec_reg = gen_rtx_REG (DImode, EXEC_REG);
  regset_head live;

  INIT_REG_SET (&live);

  compute_bb_for_insn ();

  if (!optimize)
    {
      split_all_insns ();
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "After split:\n");
	  print_rtl_with_bb (dump_file, get_insns (), dump_flags);
	}

      /* Update data-flow information for split instructions.  */
      df_insn_rescan_all ();
    }

  df_live_add_problem ();
  df_live_set_all_dirty ();
  df_analyze ();

  /* This pass ensures that the EXEC register is set correctly, according
     to the "exec" attribute.  However, care must be taken so that the
     value that reaches explicit uses of the EXEC register remains the
     same as before.
   */

  FOR_EACH_BB_FN (bb, cfun)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "BB %d:\n", bb->index);

      rtx_insn *insn, *curr;
      rtx_insn *last_exec_def = BB_HEAD (bb);
      bool last_exec_def_saved = false;
      bool curr_exec_explicit = true;
      bool curr_exec_known = true;
      int64_t curr_exec = 0;	/* 0 here means 'the value is that of EXEC
				   after last_exec_def is executed'.  */

      bitmap live_in = DF_LR_IN (bb);
      bool exec_live_on_entry = false;
      if (bitmap_bit_p (live_in, EXEC_LO_REG)
	  || bitmap_bit_p (live_in, EXEC_HI_REG))
	{
	  if (dump_file)
	    fprintf (dump_file, "EXEC reg is live on entry to block %d\n",
		     (int) bb->index);
	  exec_live_on_entry = true;
	}

      FOR_BB_INSNS_SAFE (bb, insn, curr)
	{
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  if (GET_CODE (PATTERN (insn)) == USE
	      || GET_CODE (PATTERN (insn)) == CLOBBER)
	    continue;

	  HARD_REG_SET defs, uses;
	  CLEAR_HARD_REG_SET (defs);
	  CLEAR_HARD_REG_SET (uses);
	  note_stores (insn, record_hard_reg_sets, &defs);
	  note_uses (&PATTERN (insn), record_hard_reg_uses, &uses);

	  bool exec_lo_def_p = TEST_HARD_REG_BIT (defs, EXEC_LO_REG);
	  bool exec_hi_def_p = TEST_HARD_REG_BIT (defs, EXEC_HI_REG);
	  bool exec_used = (hard_reg_set_intersect_p
			    (uses, reg_class_contents[(int) EXEC_MASK_REG])
			    || TEST_HARD_REG_BIT (uses, EXECZ_REG));

	  /* Check the instruction for implicit setting of EXEC via an
	     attribute.  */
	  attr_exec exec_attr = get_attr_exec (insn);
	  int64_t new_exec;

	  switch (exec_attr)
	    {
	    case EXEC_NONE:
	      new_exec = 0;
	      break;

	    case EXEC_SINGLE:
	      /* Instructions that do not involve memory accesses only require
		 bit 0 of EXEC to be set.  */
	      if (gcn_vmem_insn_p (get_attr_type (insn))
		  || get_attr_type (insn) == TYPE_DS)
		new_exec = 1;
	      else
		new_exec = curr_exec | 1;
	      break;

	    case EXEC_FULL:
	      new_exec = -1;
	      break;

	    default:  /* Auto-detect what setting is appropriate.  */
	      {
	        new_exec = 0;

		/* If EXEC is referenced explicitly then we don't need to do
		   anything to set it, so we're done.  */
		if (exec_used)
		  break;

		/* Scan the insn for VGPRs defs or uses.  The mode determines
		   what kind of exec is needed.  */
		subrtx_iterator::array_type array;
		FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
		  {
		    const_rtx x = *iter;
		    if (REG_P (x) && (VGPR_REGNO_P (REGNO (x))
				      || AVGPR_REGNO_P (REGNO (x))))
		      {
			if (VECTOR_MODE_P (GET_MODE (x)))
			  {
			    int vf = GET_MODE_NUNITS (GET_MODE (x));
			    new_exec = MAX ((uint64_t)new_exec,
					    0xffffffffffffffffUL >> (64-vf));
			  }
			else if (new_exec == 0)
			  new_exec = 1;
		      }
		  }
	        }
	      break;
	    }

	  if (new_exec && (!curr_exec_known || new_exec != curr_exec))
	    {
	      start_sequence ();
	      emit_move_insn (exec_reg, GEN_INT (new_exec));
	      rtx_insn *seq = get_insns ();
	      end_sequence ();
	      emit_insn_before (seq, insn);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Setting EXEC to %ld before insn %d.\n",
			 new_exec, INSN_UID (insn));

	      curr_exec = new_exec;
	      curr_exec_explicit = false;
	      curr_exec_known = true;
	    }
	  else if (new_exec && dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Exec already is %ld before insn %d.\n",
		       new_exec, INSN_UID (insn));
	    }

	  /* The state of the EXEC register is unknown after a
	     function call.  */
	  if (CALL_P (insn))
	    curr_exec_known = false;

	  /* Handle explicit uses of EXEC.  If the instruction is a partial
	     explicit definition of EXEC, then treat it as an explicit use of
	     EXEC as well.  */
	  if (exec_used || exec_lo_def_p != exec_hi_def_p)
	    {
	      /* An instruction that explicitly uses EXEC should not also
		 implicitly define it.  */
	      gcc_assert (!exec_used || !new_exec);

	      if (!curr_exec_known || !curr_exec_explicit)
		{
		  /* Restore the previous explicitly defined value.  */
		  curr_exec = gcn_restore_exec (insn, last_exec_def,
						curr_exec, curr_exec_known,
						last_exec_def_saved);
		  curr_exec_explicit = true;
		  curr_exec_known = true;
		}
	    }

	  /* Handle explicit definitions of EXEC.  */
	  if (exec_lo_def_p || exec_hi_def_p)
	    {
	      last_exec_def = insn;
	      last_exec_def_saved = false;
	      curr_exec = gcn_insn_exec_value (insn);
	      curr_exec_explicit = true;
	      curr_exec_known = true;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "Found %s definition of EXEC at insn %d.\n",
			 exec_lo_def_p == exec_hi_def_p ? "full" : "partial",
			 INSN_UID (insn));
	    }

	  exec_live_on_entry = false;
	}

      COPY_REG_SET (&live, DF_LR_OUT (bb));
      df_simulate_initialize_backwards (bb, &live);

      /* If EXEC is live after the basic block, restore the value of EXEC
	 at the end of the block.  */
      if ((REGNO_REG_SET_P (&live, EXEC_LO_REG)
	   || REGNO_REG_SET_P (&live, EXEC_HI_REG))
	  && (!curr_exec_known || !curr_exec_explicit || exec_live_on_entry))
	{
	  rtx_insn *end_insn = BB_END (bb);

	  /* If the instruction is not a jump instruction, do the restore
	     after the last instruction in the basic block.  */
	  if (NONJUMP_INSN_P (end_insn))
	    end_insn = NEXT_INSN (end_insn);

	  gcn_restore_exec (end_insn, last_exec_def, curr_exec,
			    curr_exec_known, last_exec_def_saved);
	}
    }

  CLEAR_REG_SET (&live);

  /* "Manually Inserted Wait States (NOPs)."

     GCN hardware detects most kinds of register dependencies, but there
     are some exceptions documented in the ISA manual.  This pass
     detects the missed cases, and inserts the documented number of NOPs
     required for correct execution.  */

  const int max_waits = 5;
  struct ilist
  {
    rtx_insn *insn;
    attr_unit unit;
    attr_delayeduse delayeduse;
    HARD_REG_SET writes;
    HARD_REG_SET reads;
    int age;
  } back[max_waits];
  int oldest = 0;
  for (int i = 0; i < max_waits; i++)
    back[i].insn = NULL;

  rtx_insn *insn, *last_insn = NULL;
  for (insn = get_insns (); insn != 0; insn = NEXT_INSN (insn))
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      if (GET_CODE (PATTERN (insn)) == USE
	  || GET_CODE (PATTERN (insn)) == CLOBBER)
	continue;

      attr_type itype = get_attr_type (insn);
      attr_unit iunit = get_attr_unit (insn);
      attr_delayeduse idelayeduse = get_attr_delayeduse (insn);
      int ivccwait = get_attr_vccwait (insn);
      HARD_REG_SET ireads, iwrites;
      CLEAR_HARD_REG_SET (ireads);
      CLEAR_HARD_REG_SET (iwrites);
      note_stores (insn, record_hard_reg_sets, &iwrites);
      note_uses (&PATTERN (insn), record_hard_reg_uses, &ireads);

      /* Scan recent previous instructions for dependencies not handled in
         hardware.  */
      int nops_rqd = 0;
      for (int i = oldest; i < oldest + max_waits; i++)
	{
	  struct ilist *prev_insn = &back[i % max_waits];

	  if (!prev_insn->insn)
	    continue;

	  HARD_REG_SET depregs = prev_insn->writes & ireads;

	  /* VALU writes SGPR followed by VMEM reading the same SGPR
	     requires 5 wait states.  */
	  if ((prev_insn->age + nops_rqd) < 5
	      && prev_insn->unit == UNIT_VECTOR
	      && gcn_vmem_insn_p (itype)
	      && hard_reg_set_intersect_p
		   (depregs, reg_class_contents[(int) SGPR_REGS]))
	    nops_rqd = 5 - prev_insn->age;

	  /* VALU sets VCC/EXEC followed by VALU uses VCCZ/EXECZ
	     requires 5 wait states.  */
	  if ((prev_insn->age + nops_rqd) < 5
	      && prev_insn->unit == UNIT_VECTOR
	      && iunit == UNIT_VECTOR
	      && ((hard_reg_set_intersect_p
		   (prev_insn->writes,
		    reg_class_contents[(int) EXEC_MASK_REG])
		   && TEST_HARD_REG_BIT (ireads, EXECZ_REG))
		  ||
		  (hard_reg_set_intersect_p
		   (prev_insn->writes,
		    reg_class_contents[(int) VCC_CONDITIONAL_REG])
		   && TEST_HARD_REG_BIT (ireads, VCCZ_REG))))
	    nops_rqd = 5 - prev_insn->age;

	  /* VALU writes SGPR/VCC followed by v_{read,write}lane using
	     SGPR/VCC as lane select requires 4 wait states.  */
	  if ((prev_insn->age + nops_rqd) < 4
	      && prev_insn->unit == UNIT_VECTOR
	      && get_attr_laneselect (insn) == LANESELECT_YES
	      && (hard_reg_set_intersect_p
		    (depregs, reg_class_contents[(int) SGPR_REGS])
		  || hard_reg_set_intersect_p
		       (depregs, reg_class_contents[(int) VCC_CONDITIONAL_REG])))
	    nops_rqd = 4 - prev_insn->age;

	  /* VALU writes VGPR followed by VALU_DPP reading that VGPR
	     requires 2 wait states.  */
	  if ((prev_insn->age + nops_rqd) < 2
	      && prev_insn->unit == UNIT_VECTOR
	      && itype == TYPE_VOP_DPP)
	    {
	      if (hard_reg_set_intersect_p
		  (depregs, reg_class_contents[(int) VGPR_REGS]))
		nops_rqd = 2 - prev_insn->age;
	    }

	  /* Store that requires input registers are not overwritten by
	     following instruction.  */
	  if ((prev_insn->age + nops_rqd) < 1
	      && prev_insn->delayeduse == DELAYEDUSE_YES
	      && ((hard_reg_set_intersect_p
		   (prev_insn->reads, iwrites))))
	    nops_rqd = 1 - prev_insn->age;

	  /* Instruction that requires VCC is not written too close before
	     using it.  */
	  if (prev_insn->age < ivccwait
	      && (hard_reg_set_intersect_p
		  (prev_insn->writes,
		   reg_class_contents[(int)VCC_CONDITIONAL_REG])))
	    nops_rqd = ivccwait - prev_insn->age;

	  /* CDNA1: write VGPR before v_accvgpr_write reads it.  */
	  if (TARGET_AVGPR_CDNA1_NOPS
	      && (prev_insn->age + nops_rqd) < 2
	      && hard_reg_set_intersect_p
		  (depregs, reg_class_contents[(int) VGPR_REGS])
	      && hard_reg_set_intersect_p
		  (iwrites, reg_class_contents[(int) AVGPR_REGS]))
	    nops_rqd = 2 - prev_insn->age;

	  /* CDNA1: v_accvgpr_write writes AVGPR before v_accvgpr_read.  */
	  if (TARGET_AVGPR_CDNA1_NOPS
	      && (prev_insn->age + nops_rqd) < 3
	      && hard_reg_set_intersect_p
		  (depregs, reg_class_contents[(int) AVGPR_REGS])
	      && hard_reg_set_intersect_p
		  (iwrites, reg_class_contents[(int) VGPR_REGS]))
	    nops_rqd = 3 - prev_insn->age;

	  /* CDNA1: Undocumented(?!) read-after-write when restoring values
	     from AVGPRs to VGPRS.  Observed problem was for address register
	     of flat_load instruction, but others may be affected?  */
	  if (TARGET_AVGPR_CDNA1_NOPS
	      && (prev_insn->age + nops_rqd) < 2
	      && hard_reg_set_intersect_p
		   (prev_insn->reads, reg_class_contents[(int) AVGPR_REGS])
	      && hard_reg_set_intersect_p
		   (depregs, reg_class_contents[(int) VGPR_REGS]))
	    nops_rqd = 2 - prev_insn->age;
	}

      /* Insert the required number of NOPs.  */
      for (int i = nops_rqd; i > 0; i--)
	emit_insn_after (gen_nop (), last_insn);

      /* Age the previous instructions.  We can also ignore writes to
         registers subsequently overwritten.  */
      HARD_REG_SET written;
      CLEAR_HARD_REG_SET (written);
      for (int i = oldest + max_waits - 1; i > oldest; i--)
	{
	  struct ilist *prev_insn = &back[i % max_waits];

	  /* Assume all instructions are equivalent to one "wait", the same
	     as s_nop.  This is probably true for SALU, but not VALU (which
	     may take longer), so this is not optimal.  However, AMD do
	     not publish the cycle times for instructions.  */
	  prev_insn->age += 1 + nops_rqd;

	  written |= iwrites;
	  prev_insn->writes &= ~written;
	}

      /* Track the current instruction as a previous instruction.  */
      back[oldest].insn = insn;
      back[oldest].unit = iunit;
      back[oldest].delayeduse = idelayeduse;
      back[oldest].writes = iwrites;
      back[oldest].reads = ireads;
      back[oldest].age = 0;
      oldest = (oldest + 1) % max_waits;

      last_insn = insn;
    }
}

/* }}}  */
/* {{{ OpenACC / OpenMP.  */

#define GCN_DEFAULT_GANGS 0	/* Choose at runtime.  */
#define GCN_DEFAULT_WORKERS 0	/* Choose at runtime.  */
#define GCN_DEFAULT_VECTORS 1	/* Use autovectorization only, for now.  */

/* Implement TARGET_GOACC_VALIDATE_DIMS.

   Check the launch dimensions provided for an OpenACC compute
   region, or routine.  */

static bool
gcn_goacc_validate_dims (tree decl, int dims[], int fn_level,
			 unsigned /*used*/)
{
  bool changed = false;
  const int max_workers = 16;

  /* The vector size must appear to be 64, to the user, unless this is a
     SEQ routine.  The real, internal value is always 1, which means use
     autovectorization, but the user should not see that.  */
  if (fn_level <= GOMP_DIM_VECTOR && fn_level >= -1
      && dims[GOMP_DIM_VECTOR] >= 0)
    {
      if (fn_level < 0 && dims[GOMP_DIM_VECTOR] >= 0
	  && dims[GOMP_DIM_VECTOR] != 64)
	warning_at (decl ? DECL_SOURCE_LOCATION (decl) : UNKNOWN_LOCATION,
		    OPT_Wopenacc_dims,
		    (dims[GOMP_DIM_VECTOR]
		     ? G_("using %<vector_length (64)%>, ignoring %d")
		     : G_("using %<vector_length (64)%>, "
			  "ignoring runtime setting")),
		    dims[GOMP_DIM_VECTOR]);
      dims[GOMP_DIM_VECTOR] = 1;
      changed = true;
    }

  /* Check the num workers is not too large.  */
  if (dims[GOMP_DIM_WORKER] > max_workers)
    {
      warning_at (decl ? DECL_SOURCE_LOCATION (decl) : UNKNOWN_LOCATION,
		  OPT_Wopenacc_dims,
		  "using %<num_workers (%d)%>, ignoring %d",
		  max_workers, dims[GOMP_DIM_WORKER]);
      dims[GOMP_DIM_WORKER] = max_workers;
      changed = true;
    }

  /* Set global defaults.  */
  if (!decl)
    {
      dims[GOMP_DIM_VECTOR] = GCN_DEFAULT_VECTORS;
      if (dims[GOMP_DIM_WORKER] < 0)
	dims[GOMP_DIM_WORKER] = GCN_DEFAULT_WORKERS;
      if (dims[GOMP_DIM_GANG] < 0)
	dims[GOMP_DIM_GANG] = GCN_DEFAULT_GANGS;
      changed = true;
    }

  return changed;
}

/* Helper function for oacc_dim_size instruction.
   Also used for OpenMP, via builtin_gcn_dim_size, and the omp_gcn pass.  */

rtx
gcn_oacc_dim_size (int dim)
{
  if (dim < 0 || dim > 2)
    error ("offload dimension out of range (%d)", dim);

  /* Vectors are a special case.  */
  if (dim == 2)
    return const1_rtx;		/* Think of this as 1 times 64.  */

  static int offset[] = {
    /* Offsets into dispatch packet.  */
    12,				/* X dim = Gang / Team / Work-group.  */
    20,				/* Z dim = Worker / Thread / Wavefront.  */
    16				/* Y dim = Vector / SIMD / Work-item.  */
  };
  rtx addr = gen_rtx_PLUS (DImode,
			   gen_rtx_REG (DImode,
					cfun->machine->args.
					reg[DISPATCH_PTR_ARG]),
			   GEN_INT (offset[dim]));
  rtx mem = gen_rtx_MEM (SImode, addr);
  set_mem_addr_space (mem, ADDR_SPACE_SCALAR_FLAT);
  return mem;
}

/* Helper function for oacc_dim_pos instruction.
   Also used for OpenMP, via builtin_gcn_dim_pos, and the omp_gcn pass.  */

rtx
gcn_oacc_dim_pos (int dim)
{
  if (dim < 0 || dim > 2)
    error ("offload dimension out of range (%d)", dim);

  static const int reg[] = {
    WORKGROUP_ID_X_ARG,		/* Gang / Team / Work-group.  */
    WORK_ITEM_ID_Z_ARG,		/* Worker / Thread / Wavefront.  */
    WORK_ITEM_ID_Y_ARG		/* Vector / SIMD / Work-item.  */
  };

  int reg_num = cfun->machine->args.reg[reg[dim]];

  /* The information must have been requested by the kernel.  */
  gcc_assert (reg_num >= 0);

  return gen_rtx_REG (SImode, reg_num);
}

/* Implement TARGET_GOACC_FORK_JOIN.  */

static bool
gcn_fork_join (gcall *call, const int dims[], bool is_fork)
{
  tree arg = gimple_call_arg (call, 2);
  unsigned axis = TREE_INT_CST_LOW (arg);

  if (!is_fork && axis == GOMP_DIM_WORKER && dims[axis] != 1)
    return true;

  return false;
}

/* Implement ???????
   FIXME make this a real hook.

   Adjust FNDECL such that options inherited from the host compiler
   are made appropriate for the accelerator compiler.  */

void
gcn_fixup_accel_lto_options (tree fndecl)
{
  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);
  if (!func_optimize)
    return;

  tree old_optimize
    = build_optimization_node (&global_options, &global_options_set);
  tree new_optimize;

  /* If the function changed the optimization levels as well as
     setting target options, start with the optimizations
     specified.  */
  if (func_optimize != old_optimize)
    cl_optimization_restore (&global_options, &global_options_set,
			     TREE_OPTIMIZATION (func_optimize));

  gcn_option_override ();

  /* The target attributes may also change some optimization flags,
     so update the optimization options if necessary.  */
  new_optimize = build_optimization_node (&global_options,
					  &global_options_set);

  if (old_optimize != new_optimize)
    {
      DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;
      cl_optimization_restore (&global_options, &global_options_set,
			       TREE_OPTIMIZATION (old_optimize));
    }
}

/* Implement TARGET_GOACC_SHARED_MEM_LAYOUT hook.  */

static void
gcn_shared_mem_layout (unsigned HOST_WIDE_INT *lo,
		       unsigned HOST_WIDE_INT *hi,
		       int ARG_UNUSED (dims[GOMP_DIM_MAX]),
		       unsigned HOST_WIDE_INT
			 ARG_UNUSED (private_size[GOMP_DIM_MAX]),
		       unsigned HOST_WIDE_INT reduction_size[GOMP_DIM_MAX])
{
  *lo = gang_private_size_opt + reduction_size[GOMP_DIM_WORKER];
  /* !!! We can maybe use dims[] to estimate the maximum number of work
     groups/wavefronts/etc. we will launch, and therefore tune the maximum
     amount of LDS we should use.  For now, use a minimal amount to try to
     maximise occupancy.  */
  *hi = acc_lds_size;
  machine_function *machfun = cfun->machine;
  machfun->reduction_base = gang_private_size_opt;
  machfun->reduction_limit
    = gang_private_size_opt + reduction_size[GOMP_DIM_WORKER];
}

/* }}}  */
/* {{{ ASM Output.  */

/*  Implement TARGET_ASM_FILE_START.

    Print assembler file header text.  */

static void
output_file_start (void)
{
  /* In HSACOv4 no attribute setting means the binary supports "any" hardware
     configuration.  */
  const char *xnack = (flag_xnack == HSACO_ATTR_ON ? ":xnack+"
		       : flag_xnack == HSACO_ATTR_OFF ? ":xnack-"
		       : "" /* Unsupported or "any".  */);
  const char *sram_ecc = (flag_sram_ecc == HSACO_ATTR_ON ? ":sramecc+"
			  : flag_sram_ecc == HSACO_ATTR_OFF ? ":sramecc-"
			  : "" /* Unsupported or "any".  */);
  const char *cpu = gcn_devices[gcn_arch].name;

  fprintf(asm_out_file, "\t.amdgcn_target \"amdgcn-unknown-amdhsa--%s%s%s\"\n",
	  cpu, sram_ecc, xnack);
}

/* Implement ASM_DECLARE_FUNCTION_NAME via gcn-hsa.h.

   Print the initial definition of a function name.

   For GCN kernel entry points this includes all the HSA meta-data, special
   alignment constraints that don't apply to regular functions, and magic
   comments that pass information to mkoffload.  */

void
gcn_hsa_declare_function_name (FILE *file, const char *name,
			       tree decl ATTRIBUTE_UNUSED)
{
  int sgpr, vgpr, avgpr;
  bool xnack_enabled = TARGET_XNACK;

  fputs ("\n\n", file);

  if (cfun && cfun->machine && cfun->machine->normal_function)
    {
      fputs ("\t.type\t", file);
      assemble_name (file, name);
      fputs (",@function\n", file);
      assemble_name (file, name);
      fputs (":\n", file);
      return;
    }

  /* Determine count of sgpr/vgpr registers by looking for last
     one used.  */
  for (sgpr = LAST_SGPR_REG - FIRST_SGPR_REG; sgpr >= 0; sgpr--)
    if (df_regs_ever_live_p (FIRST_SGPR_REG + sgpr))
      break;
  sgpr++;
  for (vgpr = LAST_VGPR_REG - FIRST_VGPR_REG; vgpr >= 0; vgpr--)
    if (df_regs_ever_live_p (FIRST_VGPR_REG + vgpr))
      break;
  vgpr++;
  for (avgpr = LAST_AVGPR_REG - FIRST_AVGPR_REG; avgpr >= 0; avgpr--)
    if (df_regs_ever_live_p (FIRST_AVGPR_REG + avgpr))
      break;
  avgpr++;

  /* The main function epilogue uses v8, but df doesn't see that.  */
  if (vgpr < 9)
    vgpr = 9;

  if (!leaf_function_p ())
    {
      /* We can't know how many registers function calls might use.  */
      if (vgpr < MAX_NORMAL_VGPR_COUNT)
	vgpr = MAX_NORMAL_VGPR_COUNT;
      if (sgpr < MAX_NORMAL_SGPR_COUNT)
	sgpr = MAX_NORMAL_SGPR_COUNT;
      if (avgpr < MAX_NORMAL_AVGPR_COUNT)
	avgpr = MAX_NORMAL_AVGPR_COUNT;
    }

  /* SIMD32 devices count double in wavefront64 mode.  */
  if (TARGET_WAVE64_COMPAT)
    vgpr *= 2;

  /* Round up to the allocation block size.  */
  int vgpr_block_size = TARGET_VGPR_GRANULARITY;
  if (vgpr % vgpr_block_size)
    vgpr += vgpr_block_size - (vgpr % vgpr_block_size);
  if (avgpr % vgpr_block_size)
    avgpr += vgpr_block_size - (avgpr % vgpr_block_size);

  fputs ("\t.rodata\n"
	 "\t.p2align\t6\n"
	 "\t.amdhsa_kernel\t", file);
  assemble_name (file, name);
  fputs ("\n", file);
  int reg = FIRST_SGPR_REG;
  for (int a = 0; a < GCN_KERNEL_ARG_TYPES; a++)
    {
      int reg_first = -1;
      int reg_last;
      if ((cfun->machine->args.requested & (1 << a))
	  && (gcn_kernel_arg_types[a].fixed_regno < 0))
	{
	  reg_first = reg;
	  reg_last = (reg_first
		      + (GET_MODE_SIZE (gcn_kernel_arg_types[a].mode)
			 / UNITS_PER_WORD) - 1);
	  reg = reg_last + 1;
	}

      if (gcn_kernel_arg_types[a].header_pseudo)
	{
	  fprintf (file, "\t  %s%s\t%i",
		   (cfun->machine->args.requested & (1 << a)) != 0 ? "" : ";",
		   gcn_kernel_arg_types[a].header_pseudo,
		   (cfun->machine->args.requested & (1 << a)) != 0);
	  if (reg_first != -1)
	    {
	      fprintf (file, " ; (");
	      for (int i = reg_first; i <= reg_last; ++i)
		{
		  if (i != reg_first)
		    fprintf (file, ", ");
		  fprintf (file, "%s", reg_names[i]);
		}
	      fprintf (file, ")");
	    }
	  fprintf (file, "\n");
	}
      else if (gcn_kernel_arg_types[a].fixed_regno >= 0
	       && cfun->machine->args.requested & (1 << a))
	fprintf (file, "\t  ; %s\t%i (%s)\n",
		 gcn_kernel_arg_types[a].name,
		 (cfun->machine->args.requested & (1 << a)) != 0,
		 reg_names[gcn_kernel_arg_types[a].fixed_regno]);
    }
  fprintf (file, "\t  .amdhsa_system_vgpr_workitem_id\t%i\n",
	   (cfun->machine->args.requested & (1 << WORK_ITEM_ID_Z_ARG))
	   ? 2
	   : cfun->machine->args.requested & (1 << WORK_ITEM_ID_Y_ARG)
	   ? 1 : 0);
  int next_free_vgpr = vgpr;
  if (TARGET_AVGPR_COMBINED)
    next_free_vgpr += avgpr;
  else if (TARGET_AVGPRS && avgpr > vgpr)
    next_free_vgpr = avgpr;
  fprintf (file,
	   "\t  .amdhsa_next_free_vgpr\t%i\n"
	   "\t  .amdhsa_next_free_sgpr\t%i\n"
	   "\t  .amdhsa_reserve_vcc\t1\n"
	   "\t  .amdhsa_reserve_xnack_mask\t%i\n"
	   "\t  .amdhsa_private_segment_fixed_size\t0\n"
	   "\t  .amdhsa_group_segment_fixed_size\t%u\n"
	   "\t  .amdhsa_float_denorm_mode_32\t3\n"
	   "\t  .amdhsa_float_denorm_mode_16_64\t3\n",
	   next_free_vgpr,
	   sgpr,
	   xnack_enabled,
	   LDS_SIZE);
  /* Not supported with 'architected flat scratch'.  */
  if (!TARGET_ARCHITECTED_FLAT_SCRATCH)
    fprintf (file,
	   "\t  .amdhsa_reserve_flat_scratch\t0\n");
  if (TARGET_AVGPR_COMBINED)
    fprintf (file,
	     "\t  .amdhsa_accum_offset\t%i\n",
	     vgpr); /* The AGPRs come after the VGPRs.  */
  if (TARGET_TGSPLIT)
    fprintf (file,
	     "\t  .amdhsa_tg_split\t0\n");
  fputs ("\t.end_amdhsa_kernel\n", file);

#if 1
  /* The following is YAML embedded in assembler; tabs are not allowed.  */
  fputs ("        .amdgpu_metadata\n"
	 "        amdhsa.version:\n"
	 "          - 1\n"
	 "          - 0\n"
	 "        amdhsa.kernels:\n"
	 "          - .name: ", file);
  assemble_name (file, name);
  fputs ("\n            .symbol: ", file);
  assemble_name (file, name);
  fprintf (file,
	   ".kd\n"
	   "            .kernarg_segment_size: %i\n"
	   "            .kernarg_segment_align: %i\n"
	   "            .group_segment_fixed_size: %u\n"
	   "            .private_segment_fixed_size: 0\n"
	   "            .wavefront_size: 64\n"
	   "            .sgpr_count: %i\n"
	   "            .vgpr_count: %i%s\n"
	   "            .max_flat_workgroup_size: 1024\n",
	   cfun->machine->kernarg_segment_byte_size,
	   cfun->machine->kernarg_segment_alignment,
	   LDS_SIZE,
	   sgpr, next_free_vgpr,
	   (TARGET_WAVE64_COMPAT
	    ? " ; wavefrontsize64 counts double on SIMD32"
	    : ""));
  if (TARGET_AVGPRS)
    fprintf (file, "            .agpr_count: %i\n", avgpr);
  fputs ("        .end_amdgpu_metadata\n", file);
#endif

  fputs ("\t.text\n", file);
  fputs ("\t.align\t256\n", file);
  fputs ("\t.type\t", file);
  assemble_name (file, name);
  fputs (",@function\n", file);
  ASM_OUTPUT_FUNCTION_LABEL (file, name, decl);

  /* This comment is read by mkoffload.  */
  if (flag_openacc)
    fprintf (file, "\t;; OPENACC-DIMS: %d, %d, %d : %s\n",
	     oacc_get_fn_dim_size (cfun->decl, GOMP_DIM_GANG),
	     oacc_get_fn_dim_size (cfun->decl, GOMP_DIM_WORKER),
	     oacc_get_fn_dim_size (cfun->decl, GOMP_DIM_VECTOR), name);
}

/* Implement TARGET_ASM_SELECT_SECTION.

   Return the section into which EXP should be placed.  */

static section *
gcn_asm_select_section (tree exp, int reloc, unsigned HOST_WIDE_INT align)
{
  if (TREE_TYPE (exp) != error_mark_node
      && TYPE_ADDR_SPACE (TREE_TYPE (exp)) == ADDR_SPACE_LDS)
    {
      if (!DECL_P (exp))
	return get_section (".lds_bss",
			    SECTION_WRITE | SECTION_BSS | SECTION_DEBUG,
			    NULL);

      return get_named_section (exp, ".lds_bss", reloc);
    }

  return default_elf_select_section (exp, reloc, align);
}

/* Implement TARGET_ASM_FUNCTION_PROLOGUE.

   Emits custom text into the assembler file at the head of each function.  */

static void
gcn_target_asm_function_prologue (FILE *file)
{
  machine_function *offsets = gcn_compute_frame_offsets ();

  asm_fprintf (file, "\t; using %s addressing in function\n",
	       offsets->use_flat_addressing ? "flat" : "global");

  if (offsets->normal_function)
    {
      asm_fprintf (file, "\t; frame pointer needed: %s\n",
		   offsets->need_frame_pointer ? "true" : "false");
      asm_fprintf (file, "\t; lr needs saving: %s\n",
		   offsets->lr_needs_saving ? "true" : "false");
      asm_fprintf (file, "\t; outgoing args size: %wd\n",
		   offsets->outgoing_args_size);
      asm_fprintf (file, "\t; pretend size: %wd\n", offsets->pretend_size);
      asm_fprintf (file, "\t; local vars size: %wd\n", offsets->local_vars);
      asm_fprintf (file, "\t; callee save size: %wd\n",
		   offsets->callee_saves);
    }
  else
    {
      asm_fprintf (file, "\t; HSA kernel entry point\n");
      asm_fprintf (file, "\t; local vars size: %wd\n", offsets->local_vars);
      asm_fprintf (file, "\t; outgoing args size: %wd\n",
		   offsets->outgoing_args_size);
    }
}

/* Helper function for print_operand and print_operand_address.

   Print a register as the assembler requires, according to mode and name.  */

static void
print_reg (FILE *file, rtx x)
{
  machine_mode mode = GET_MODE (x);
  if (VECTOR_MODE_P (mode))
    mode = GET_MODE_INNER (mode);
  if (mode == BImode || mode == QImode || mode == HImode || mode == SImode
      || mode == HFmode || mode == SFmode)
    fprintf (file, "%s", reg_names[REGNO (x)]);
  else if (mode == DImode || mode == DFmode)
    {
      if (SGPR_REGNO_P (REGNO (x)))
	fprintf (file, "s[%i:%i]", REGNO (x) - FIRST_SGPR_REG,
		 REGNO (x) - FIRST_SGPR_REG + 1);
      else if (VGPR_REGNO_P (REGNO (x)))
	fprintf (file, "v[%i:%i]", REGNO (x) - FIRST_VGPR_REG,
		 REGNO (x) - FIRST_VGPR_REG + 1);
      else if (AVGPR_REGNO_P (REGNO (x)))
	fprintf (file, "a[%i:%i]", REGNO (x) - FIRST_AVGPR_REG,
		 REGNO (x) - FIRST_AVGPR_REG + 1);
      else if (REGNO (x) == FLAT_SCRATCH_REG)
	fprintf (file, "flat_scratch");
      else if (REGNO (x) == EXEC_REG)
	fprintf (file, "exec");
      else if (REGNO (x) == VCC_LO_REG)
	fprintf (file, "vcc");
      else
	fprintf (file, "[%s:%s]",
		 reg_names[REGNO (x)], reg_names[REGNO (x) + 1]);
    }
  else if (mode == TImode)
    {
      if (SGPR_REGNO_P (REGNO (x)))
	fprintf (file, "s[%i:%i]", REGNO (x) - FIRST_SGPR_REG,
		 REGNO (x) - FIRST_SGPR_REG + 3);
      else if (VGPR_REGNO_P (REGNO (x)))
	fprintf (file, "v[%i:%i]", REGNO (x) - FIRST_VGPR_REG,
		 REGNO (x) - FIRST_VGPR_REG + 3);
      else if (AVGPR_REGNO_P (REGNO (x)))
	fprintf (file, "a[%i:%i]", REGNO (x) - FIRST_AVGPR_REG,
		 REGNO (x) - FIRST_AVGPR_REG + 3);
      else
	gcc_unreachable ();
    }
  else
    gcc_unreachable ();
}

/* Implement TARGET_SECTION_TYPE_FLAGS.

   Return a set of section attributes for use by TARGET_ASM_NAMED_SECTION.  */

static unsigned int
gcn_section_type_flags (tree decl, const char *name, int reloc)
{
  if (strcmp (name, ".lds_bss") == 0)
    return SECTION_WRITE | SECTION_BSS | SECTION_DEBUG;

  return default_section_type_flags (decl, name, reloc);
}

/* Helper function for gcn_asm_output_symbol_ref.

   FIXME: This function is used to lay out gang-private variables in LDS
   on a per-CU basis.
   There may be cases in which gang-private variables in different compilation
   units could clobber each other.  In that case we should be relying on the
   linker to lay out gang-private LDS space, but that doesn't appear to be
   possible at present.  */

static void
gcn_print_lds_decl (FILE *f, tree var)
{
  int *offset;
  if ((offset = lds_allocs.get (var)))
    fprintf (f, "%u", (unsigned) *offset);
  else
    {
      unsigned HOST_WIDE_INT align = DECL_ALIGN_UNIT (var);
      tree type = TREE_TYPE (var);
      unsigned HOST_WIDE_INT size = tree_to_uhwi (TYPE_SIZE_UNIT (type));
      if (size > align && size > 4 && align < 8)
	align = 8;

      gang_private_hwm = ((gang_private_hwm + align - 1) & ~(align - 1));

      lds_allocs.put (var, gang_private_hwm);
      fprintf (f, "%u", gang_private_hwm);
      gang_private_hwm += size;
      if (gang_private_hwm > gang_private_size_opt)
	error ("%d bytes of gang-private data-share memory exhausted"
	       " (increase with %<-mgang-private-size=%d%>, for example)",
	       gang_private_size_opt, gang_private_hwm);
    }
}

/* Implement ASM_OUTPUT_SYMBOL_REF via gcn-hsa.h.  */

void
gcn_asm_output_symbol_ref (FILE *file, rtx x)
{
  tree decl;
  if (cfun
      && (decl = SYMBOL_REF_DECL (x)) != 0
      && VAR_P (decl)
      && AS_LDS_P (TYPE_ADDR_SPACE (TREE_TYPE (decl))))
    {
      /* LDS symbols (emitted using this hook) are only used at present
         to propagate worker values from an active thread to neutered
         threads.  Use the same offset for each such block, but don't
         use zero because null pointers are used to identify the active
         thread in GOACC_single_copy_start calls.  */
      gcn_print_lds_decl (file, decl);
    }
  else
    {
      assemble_name (file, XSTR (x, 0));
      /* FIXME: See above -- this condition is unreachable.  */
      if (cfun
	  && (decl = SYMBOL_REF_DECL (x)) != 0
	  && VAR_P (decl)
	  && AS_LDS_P (TYPE_ADDR_SPACE (TREE_TYPE (decl))))
	fputs ("@abs32", file);
    }
}

/* Implement TARGET_CONSTANT_ALIGNMENT.

   Returns the alignment in bits of a constant that is being placed in memory.
   CONSTANT is the constant and BASIC_ALIGN is the alignment that the object
   would ordinarily have.  */

static HOST_WIDE_INT
gcn_constant_alignment (const_tree ARG_UNUSED (constant),
			HOST_WIDE_INT basic_align)
{
  return basic_align > 128 ? basic_align : 128;
}

/* Implement PRINT_OPERAND_ADDRESS via gcn.h.  */

void
print_operand_address (FILE *file, rtx mem)
{
  gcc_assert (MEM_P (mem));

  rtx reg;
  rtx offset;
  addr_space_t as = MEM_ADDR_SPACE (mem);
  rtx addr = XEXP (mem, 0);
  gcc_assert (REG_P (addr) || GET_CODE (addr) == PLUS);

  if (AS_SCRATCH_P (as))
    switch (GET_CODE (addr))
      {
      case REG:
	print_reg (file, addr);
	break;

      case PLUS:
	reg = XEXP (addr, 0);
	offset = XEXP (addr, 1);
	print_reg (file, reg);
	if (GET_CODE (offset) == CONST_INT)
	  fprintf (file, " offset:" HOST_WIDE_INT_PRINT_DEC, INTVAL (offset));
	else
	  abort ();
	break;

      default:
	debug_rtx (addr);
	abort ();
      }
  else if (AS_ANY_FLAT_P (as))
    {
      if (GET_CODE (addr) == REG)
	print_reg (file, addr);
      else
	print_reg (file, XEXP (addr, 0));
    }
  else if (AS_GLOBAL_P (as))
    {
      rtx base = addr;
      rtx vgpr_offset = NULL_RTX;

      if (GET_CODE (addr) == PLUS)
	{
	  base = XEXP (addr, 0);

	  if (GET_CODE (base) == PLUS)
	    {
	      /* (SGPR + VGPR) + CONST  */
	      vgpr_offset = XEXP (base, 1);
	      base = XEXP (base, 0);
	    }
	  else
	    {
	      rtx offset = XEXP (addr, 1);

	      if (REG_P (offset))
		/* SGPR + VGPR  */
		vgpr_offset = offset;
	      else if (CONST_INT_P (offset))
		/* VGPR + CONST or SGPR + CONST  */
		;
	      else
		output_operand_lossage ("bad ADDR_SPACE_GLOBAL address");
	    }
	}

      if (REG_P (base))
	{
	  if (VGPR_REGNO_P (REGNO (base)))
	    print_reg (file, base);
	  else if (SGPR_REGNO_P (REGNO (base)))
	    {
	      /* The assembler requires a 64-bit VGPR pair here, even though
	         the offset should be only 32-bit.  */
	      if (vgpr_offset == NULL_RTX)
		/* In this case, the vector offset is zero, so we use the first
		   lane of v1, which is initialized to zero.  */
		fprintf (file, "v1");
	      else if (REG_P (vgpr_offset)
		       && VGPR_REGNO_P (REGNO (vgpr_offset)))
		fprintf (file, "v%d", REGNO (vgpr_offset) - FIRST_VGPR_REG);
	      else
		output_operand_lossage ("bad ADDR_SPACE_GLOBAL address");
	    }
	}
      else
	output_operand_lossage ("bad ADDR_SPACE_GLOBAL address");
    }
  else if (AS_ANY_DS_P (as))
    switch (GET_CODE (addr))
      {
      case REG:
	print_reg (file, addr);
	break;

      case PLUS:
	reg = XEXP (addr, 0);
	print_reg (file, reg);
	break;

      default:
	debug_rtx (addr);
	abort ();
      }
  else
    switch (GET_CODE (addr))
      {
      case REG:
	print_reg (file, addr);
	fprintf (file, ", 0");
	break;

      case PLUS:
	reg = XEXP (addr, 0);
	offset = XEXP (addr, 1);
	print_reg (file, reg);
	fprintf (file, ", ");
	if (GET_CODE (offset) == REG)
	  print_reg (file, reg);
	else if (GET_CODE (offset) == CONST_INT)
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (offset));
	else
	  abort ();
	break;

      default:
	debug_rtx (addr);
	abort ();
      }
}

/* Implement PRINT_OPERAND via gcn.h.

   b - print operand size as untyped operand (b8/b16/b32/b64)
   B - print operand size as SI/DI untyped operand (b32/b32/b32/b64)
   i - print operand size as untyped operand (i16/b32/i64)
   I - print operand size as SI/DI untyped operand(i32/b32/i64)
   u - print operand size as untyped operand (u16/u32/u64)
   U - print operand size as SI/DI untyped operand(u32/u64)
   o - print operand size as memory access size for loads
       (ubyte/ushort/dword/dwordx2/wordx3/dwordx4)
   s - print operand size as memory access size for stores
       (byte/short/dword/dwordx2/wordx3/dwordx4)
   C - print conditional code for s_cbranch (_sccz/_sccnz/_vccz/_vccnz...)
   c - print inverse conditional code for s_cbranch
   D - print conditional code for s_cmp (eq_u64/lg_u64...)
   E - print conditional code for v_cmp (eq_u64/ne_u64...)
   A - print address in formatting suitable for given address space.
   O - print offset:n for data share operations.
   g - print "glc", if appropriate for given MEM
   L - print low-part of a multi-reg value
   H - print second part of a multi-reg value (high-part of 2-reg value)
   J - print third part of a multi-reg value
   K - print fourth part of a multi-reg value
 */

void
print_operand (FILE *file, rtx x, int code)
{
  rtx_code xcode = x ? GET_CODE (x) : UNKNOWN;
  bool invert = false;
  switch (code)
    {
      /* Instructions have the following suffixes.
         If there are two suffixes, the first is the destination type,
	 and the second is the source type.

         B32 Bitfield (untyped data) 32-bit
         B64 Bitfield (untyped data) 64-bit
         F16 floating-point 16-bit
         F32 floating-point 32-bit (IEEE 754 single-precision float)
         F64 floating-point 64-bit (IEEE 754 double-precision float)
         I16 signed 32-bit integer
         I32 signed 32-bit integer
         I64 signed 64-bit integer
         U16 unsigned 32-bit integer
         U32 unsigned 32-bit integer
         U64 unsigned 64-bit integer  */

      /* Print operand size as untyped suffix.  */
    case 'b':
      {
	const char *s = "";
	machine_mode mode = GET_MODE (x);
	if (VECTOR_MODE_P (mode))
	  mode = GET_MODE_INNER (mode);
	switch (GET_MODE_SIZE (mode))
	  {
	  case 1:
	    s = "_b8";
	    break;
	  case 2:
	    s = "_b16";
	    break;
	  case 4:
	    s = "_b32";
	    break;
	  case 8:
	    s = "_b64";
	    break;
	  default:
	    output_operand_lossage ("invalid operand %%xn code");
	    return;
	  }
	fputs (s, file);
      }
      return;
    case 'B':
      {
	const char *s = "";
	machine_mode mode = GET_MODE (x);
	if (VECTOR_MODE_P (mode))
	  mode = GET_MODE_INNER (mode);
	switch (GET_MODE_SIZE (mode))
	  {
	  case 1:
	  case 2:
	  case 4:
	    s = "_b32";
	    break;
	  case 8:
	    s = "_b64";
	    break;
	  default:
	    output_operand_lossage ("invalid operand %%xn code");
	    return;
	  }
	fputs (s, file);
      }
      return;
    case 'e':
      fputs ("sext(", file);
      print_operand (file, x, 0);
      fputs (")", file);
      return;
    case 'i':
    case 'I':
    case 'u':
    case 'U':
      {
	bool signed_p = code == 'i';
	bool min32_p = code == 'I' || code == 'U';
	const char *s = "";
	machine_mode mode = GET_MODE (x);
	if (VECTOR_MODE_P (mode))
	  mode = GET_MODE_INNER (mode);
	if (mode == VOIDmode)
	  switch (GET_CODE (x))
	    {
	    case CONST_INT:
	      s = signed_p ? "_i32" : "_u32";
	      break;
	    case CONST_DOUBLE:
	      s = "_f64";
	      break;
	    default:
	      output_operand_lossage ("invalid operand %%xn code");
	      return;
	    }
	else if (FLOAT_MODE_P (mode))
	  switch (GET_MODE_SIZE (mode))
	    {
	    case 2:
	      s = "_f16";
	      break;
	    case 4:
	      s = "_f32";
	      break;
	    case 8:
	      s = "_f64";
	      break;
	    default:
	      output_operand_lossage ("invalid operand %%xn code");
	      return;
	    }
	else if (min32_p)
	  switch (GET_MODE_SIZE (mode))
	    {
	    case 1:
	    case 2:
	    case 4:
	      s = signed_p ? "_i32" : "_u32";
	      break;
	    case 8:
	      s = signed_p ? "_i64" : "_u64";
	      break;
	    default:
	      output_operand_lossage ("invalid operand %%xn code");
	      return;
	    }
	else
	  switch (GET_MODE_SIZE (mode))
	    {
	    case 1:
	      s = signed_p ? "_i8" : "_u8";
	      break;
	    case 2:
	      s = signed_p ? "_i16" : "_u16";
	      break;
	    case 4:
	      s = signed_p ? "_i32" : "_u32";
	      break;
	    case 8:
	      s = signed_p ? "_i64" : "_u64";
	      break;
	    default:
	      output_operand_lossage ("invalid operand %%xn code");
	      return;
	    }
	fputs (s, file);
      }
      return;
      /* Print operand size as untyped suffix.  */
    case 'o':
      {
	const char *s = 0;
	machine_mode mode = GET_MODE (x);
	if (VECTOR_MODE_P (mode))
	  mode = GET_MODE_INNER (mode);

	switch (mode)
	  {
	  case E_QImode:
	    s = "_ubyte";
	    break;
	  case E_HImode:
	  case E_HFmode:
	    s = "_ushort";
	    break;
	  default:
	    break;
	  }

	if (s)
	  {
	    fputs (s, file);
	    return;
	  }

	/* Fall-through - the other cases for 'o' are the same as for 's'.  */
	gcc_fallthrough();
      }
    case 's':
      {
	const char *s;
	machine_mode mode = GET_MODE (x);
	if (VECTOR_MODE_P (mode))
	  mode = GET_MODE_INNER (mode);

	switch (mode)
	  {
	  case E_QImode:
	    s = "_byte";
	    break;
	  case E_HImode:
	  case E_HFmode:
	    s = "_short";
	    break;
	  case E_SImode:
	  case E_SFmode:
	    s = "_dword";
	    break;
	  case E_DImode:
	  case E_DFmode:
	    s = "_dwordx2";
	    break;
	  case E_TImode:
	    s = "_dwordx4";
	    break;
	  default:
	    output_operand_lossage ("invalid operand %%xn code");
	    return;
	  }
	fputs (s, file);
      }
      return;
    case 'A':
      if (xcode != MEM)
	{
	  output_operand_lossage ("invalid %%xn code");
	  return;
	}
      print_operand_address (file, x);
      return;
    case 'O':
      {
	if (xcode != MEM)
	  {
	    output_operand_lossage ("invalid %%xn code");
	    return;
	  }
	if (AS_GDS_P (MEM_ADDR_SPACE (x)))
	  fprintf (file, " gds");

	rtx x0 = XEXP (x, 0);
	if (AS_GLOBAL_P (MEM_ADDR_SPACE (x)))
	  {
	    fprintf (file, ", ");

	    rtx base = x0;
	    rtx const_offset = NULL_RTX;

	    if (GET_CODE (base) == PLUS)
	      {
		rtx offset = XEXP (x0, 1);
		base = XEXP (x0, 0);

		if (GET_CODE (base) == PLUS)
		  /* (SGPR + VGPR) + CONST  */
		  /* Ignore the VGPR offset for this operand.  */
		  base = XEXP (base, 0);

		if (CONST_INT_P (offset))
		  const_offset = XEXP (x0, 1);
		else if (REG_P (offset))
		  /* SGPR + VGPR  */
		  /* Ignore the VGPR offset for this operand.  */
		  ;
		else
		  output_operand_lossage ("bad ADDR_SPACE_GLOBAL address");
	      }

	    if (REG_P (base))
	      {
		if (VGPR_REGNO_P (REGNO (base)))
		  /* The VGPR address is specified in the %A operand.  */
		  fprintf (file, "off");
		else if (SGPR_REGNO_P (REGNO (base)))
		  print_reg (file, base);
		else
		  output_operand_lossage ("bad ADDR_SPACE_GLOBAL address");
	      }
	    else
	      output_operand_lossage ("bad ADDR_SPACE_GLOBAL address");

	    if (const_offset != NULL_RTX)
	      fprintf (file, " offset:" HOST_WIDE_INT_PRINT_DEC,
		       INTVAL (const_offset));

	    return;
	  }

	if (GET_CODE (x0) == REG)
	  return;
	if (GET_CODE (x0) != PLUS)
	  {
	    output_operand_lossage ("invalid %%xn code");
	    return;
	  }
	rtx val = XEXP (x0, 1);
	if (GET_CODE (val) == CONST_VECTOR)
	  val = CONST_VECTOR_ELT (val, 0);
	if (GET_CODE (val) != CONST_INT)
	  {
	    output_operand_lossage ("invalid %%xn code");
	    return;
	  }
	fprintf (file, " offset:" HOST_WIDE_INT_PRINT_DEC, INTVAL (val));

      }
      return;
    case 'c':
      invert = true;
      /* Fall through.  */
    case 'C':
      {
	const char *s;
	bool num = false;
	if ((xcode != EQ && xcode != NE) || !REG_P (XEXP (x, 0)))
	  {
	    output_operand_lossage ("invalid %%xn code");
	    return;
	  }
	switch (REGNO (XEXP (x, 0)))
	  {
	  case VCC_REG:
	  case VCCZ_REG:
	    s = "_vcc";
	    break;
	  case SCC_REG:
	    /* For some reason llvm-mc insists on scc0 instead of sccz.  */
	    num = true;
	    s = "_scc";
	    break;
	  case EXECZ_REG:
	    s = "_exec";
	    break;
	  default:
	    output_operand_lossage ("invalid %%xn code");
	    return;
	  }
	fputs (s, file);
	if (xcode == (invert ? NE : EQ))
	  fputc (num ? '0' : 'z', file);
	else
	  fputs (num ? "1" : "nz", file);
	return;
      }
    case 'D':
      {
	const char *s;
	bool cmp_signed = false;
	switch (xcode)
	  {
	  case EQ:
	    s = "_eq_";
	    break;
	  case NE:
	    s = "_lg_";
	    break;
	  case LT:
	    s = "_lt_";
	    cmp_signed = true;
	    break;
	  case LE:
	    s = "_le_";
	    cmp_signed = true;
	    break;
	  case GT:
	    s = "_gt_";
	    cmp_signed = true;
	    break;
	  case GE:
	    s = "_ge_";
	    cmp_signed = true;
	    break;
	  case LTU:
	    s = "_lt_";
	    break;
	  case LEU:
	    s = "_le_";
	    break;
	  case GTU:
	    s = "_gt_";
	    break;
	  case GEU:
	    s = "_ge_";
	    break;
	  default:
	    output_operand_lossage ("invalid %%xn code");
	    return;
	  }
	fputs (s, file);
	fputc (cmp_signed ? 'i' : 'u', file);

	machine_mode mode = GET_MODE (XEXP (x, 0));

	if (mode == VOIDmode)
	  mode = GET_MODE (XEXP (x, 1));

	/* If both sides are constants, then assume the instruction is in
	   SImode since s_cmp can only do integer compares.  */
	if (mode == VOIDmode)
	  mode = SImode;

	switch (GET_MODE_SIZE (mode))
	  {
	  case 4:
	    s = "32";
	    break;
	  case 8:
	    s = "64";
	    break;
	  default:
	    output_operand_lossage ("invalid operand %%xn code");
	    return;
	  }
	fputs (s, file);
	return;
      }
    case 'E':
      {
	const char *s;
	bool cmp_signed = false;
	machine_mode mode = GET_MODE (XEXP (x, 0));

	if (mode == VOIDmode)
	  mode = GET_MODE (XEXP (x, 1));

	/* If both sides are constants, assume the instruction is in SFmode
	   if either operand is floating point, otherwise assume SImode.  */
	if (mode == VOIDmode)
	  {
	    if (GET_CODE (XEXP (x, 0)) == CONST_DOUBLE
		|| GET_CODE (XEXP (x, 1)) == CONST_DOUBLE)
	      mode = SFmode;
	    else
	      mode = SImode;
	  }

	/* Use the same format code for vector comparisons.  */
	if (GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT
	    || GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
	  mode = GET_MODE_INNER (mode);

	bool float_p = GET_MODE_CLASS (mode) == MODE_FLOAT;

	switch (xcode)
	  {
	  case EQ:
	    s = "_eq_";
	    break;
	  case NE:
	    s = float_p ? "_neq_" : "_ne_";
	    break;
	  case LT:
	    s = "_lt_";
	    cmp_signed = true;
	    break;
	  case LE:
	    s = "_le_";
	    cmp_signed = true;
	    break;
	  case GT:
	    s = "_gt_";
	    cmp_signed = true;
	    break;
	  case GE:
	    s = "_ge_";
	    cmp_signed = true;
	    break;
	  case LTU:
	    s = "_lt_";
	    break;
	  case LEU:
	    s = "_le_";
	    break;
	  case GTU:
	    s = "_gt_";
	    break;
	  case GEU:
	    s = "_ge_";
	    break;
	  case ORDERED:
	    s = "_o_";
	    break;
	  case UNORDERED:
	    s = "_u_";
	    break;
	  case UNEQ:
	    s = "_nlg_";
	    break;
	  case UNGE:
	    s = "_nlt_";
	    break;
	  case UNGT:
	    s = "_nle_";
	    break;
	  case UNLE:
	    s = "_ngt_";
	    break;
	  case UNLT:
	    s = "_nge_";
	    break;
	  case LTGT:
	    s = "_lg_";
	    break;
	  default:
	    output_operand_lossage ("invalid %%xn code");
	    return;
	  }
	fputs (s, file);
	fputc (float_p ? 'f' : cmp_signed ? 'i' : 'u', file);

	switch (GET_MODE_SIZE (mode))
	  {
	  case 1:
	    output_operand_lossage ("operand %%xn code invalid for QImode");
	    return;
	  case 2:
	    s = "16";
	    break;
	  case 4:
	    s = "32";
	    break;
	  case 8:
	    s = "64";
	    break;
	  default:
	    output_operand_lossage ("invalid operand %%xn code");
	    return;
	  }
	fputs (s, file);
	return;
      }
    case 'L':
      print_operand (file, gcn_operand_part (GET_MODE (x), x, 0), 0);
      return;
    case 'H':
      print_operand (file, gcn_operand_part (GET_MODE (x), x, 1), 0);
      return;
    case 'J':
      print_operand (file, gcn_operand_part (GET_MODE (x), x, 2), 0);
      return;
    case 'K':
      print_operand (file, gcn_operand_part (GET_MODE (x), x, 3), 0);
      return;
    case 'R':
      /* Print a scalar register number as an integer.  Temporary hack.  */
      gcc_assert (REG_P (x));
      fprintf (file, "%u", (int) REGNO (x));
      return;
    case 'V':
      /* Print a vector register number as an integer.  Temporary hack.  */
      gcc_assert (REG_P (x));
      fprintf (file, "%u", (int) REGNO (x) - FIRST_VGPR_REG);
      return;
    case 0:
      if (xcode == REG)
	print_reg (file, x);
      else if (xcode == MEM)
	output_address (GET_MODE (x), x);
      else if (xcode == CONST_INT)
	fprintf (file, "%i", (int) INTVAL (x));
      else if (xcode == CONST_VECTOR)
	print_operand (file, CONST_VECTOR_ELT (x, 0), code);
      else if (xcode == CONST_DOUBLE)
	{
	  const char *str;
	  switch (gcn_inline_fp_constant_p (x, false))
	    {
	    case 240:
	      str = "0.5";
	      break;
	    case 241:
	      str = "-0.5";
	      break;
	    case 242:
	      str = "1.0";
	      break;
	    case 243:
	      str = "-1.0";
	      break;
	    case 244:
	      str = "2.0";
	      break;
	    case 245:
	      str = "-2.0";
	      break;
	    case 246:
	      str = "4.0";
	      break;
	    case 247:
	      str = "-4.0";
	      break;
	    case 248:
	      str = "0.15915494";
	      break;
	    default:
	      rtx ix = simplify_gen_subreg (GET_MODE (x) == DFmode
					    ? DImode : SImode,
					    x, GET_MODE (x), 0);
	      if (x)
		print_operand (file, ix, code);
	      else
		output_operand_lossage ("invalid fp constant");
	      return;
	      break;
	    }
	  fprintf (file, str);
	  return;
	}
      else
	output_addr_const (file, x);
      return;
    case 'g':
      gcc_assert (xcode == MEM);
      if (MEM_VOLATILE_P (x))
	fputs (" glc", file);
      return;
    default:
      output_operand_lossage ("invalid %%xn code");
    }
  gcc_unreachable ();
}

/* Implement DEBUGGER_REGNO macro.

   Return the DWARF register number that corresponds to the GCC internal
   REGNO.  */

unsigned int
gcn_dwarf_register_number (unsigned int regno)
{
  /* Registers defined in DWARF.  */
  if (regno == EXEC_LO_REG)
    return 17;
  /* We need to use a more complex DWARF expression for this
  else if (regno == EXEC_HI_REG)
    return 17; */
  else if (regno == VCC_LO_REG)
    return 768;
  /* We need to use a more complex DWARF expression for this
  else if (regno == VCC_HI_REG)
    return 768;  */
  else if (regno == SCC_REG)
    return 128;
  else if (regno == DWARF_LINK_REGISTER)
    return 16;
  else if (SGPR_REGNO_P (regno))
    {
      if (regno - FIRST_SGPR_REG < 64)
	return (regno - FIRST_SGPR_REG + 32);
      else
	return (regno - FIRST_SGPR_REG + 1024);
    }
  else if (VGPR_REGNO_P (regno))
    return (regno - FIRST_VGPR_REG + 2560);
  else if (AVGPR_REGNO_P (regno))
    return (regno - FIRST_AVGPR_REG + 3072);

  /* Otherwise, there's nothing sensible to do.  */
  return regno + 100000;
}

/* Implement TARGET_DWARF_REGISTER_SPAN.

   DImode and Vector DImode require additional registers.  */

static rtx
gcn_dwarf_register_span (rtx rtl)
{
  machine_mode mode = GET_MODE (rtl);

  if (VECTOR_MODE_P (mode))
    mode = GET_MODE_INNER (mode);

  if (GET_MODE_SIZE (mode) != 8)
    return NULL_RTX;

  unsigned regno = REGNO (rtl);

  if (regno == DWARF_LINK_REGISTER)
    return NULL_RTX;

  rtx p = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
  XVECEXP (p, 0, 0) = gen_rtx_REG (SImode, regno);
  XVECEXP (p, 0, 1) = gen_rtx_REG (SImode, regno + 1);

  return p;
}

/* }}}  */
/* {{{ TARGET hook overrides.  */

#undef  TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE gcn_addr_space_address_mode
#undef  TARGET_ADDR_SPACE_DEBUG
#define TARGET_ADDR_SPACE_DEBUG gcn_addr_space_debug
#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P \
  gcn_addr_space_legitimate_address_p
#undef  TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS
#define TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS gcn_addr_space_legitimize_address
#undef  TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE gcn_addr_space_pointer_mode
#undef  TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P gcn_addr_space_subset_p
#undef  TARGET_ADDR_SPACE_CONVERT
#define TARGET_ADDR_SPACE_CONVERT gcn_addr_space_convert
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES gcn_arg_partial_bytes
#undef  TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.8byte\t"
#undef  TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START output_file_start
#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE gcn_target_asm_function_prologue
#undef  TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION gcn_asm_select_section
#undef  TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE gcn_asm_trampoline_template
#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE gcn_attribute_table
#undef  TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES \
  gcn_autovectorize_vector_modes
#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL gcn_builtin_decl
#undef  TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS gcn_can_change_mode_class
#undef  TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE gcn_can_eliminate_p
#undef  TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P gcn_cannot_copy_insn_p
#undef  TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P gcn_class_likely_spilled_p
#undef  TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS gcn_class_max_nregs
#undef  TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE gcn_conditional_register_usage
#undef  TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT gcn_constant_alignment
#undef  TARGET_DEBUG_UNWIND_INFO
#define TARGET_DEBUG_UNWIND_INFO gcn_debug_unwind_info
#undef  TARGET_DWARF_REGISTER_SPAN
#define TARGET_DWARF_REGISTER_SPAN gcn_dwarf_register_span
#undef  TARGET_EMUTLS_VAR_INIT
#define TARGET_EMUTLS_VAR_INIT gcn_emutls_var_init
#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN gcn_expand_builtin
#undef  TARGET_EXPAND_DIVMOD_LIBFUNC
#define TARGET_EXPAND_DIVMOD_LIBFUNC gcn_expand_divmod_libfunc
#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED gcn_frame_pointer_rqd
#undef  TARGET_FUNCTION_ARG
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE gcn_function_arg_advance
#define TARGET_FUNCTION_ARG gcn_function_arg
#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE gcn_function_value
#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P gcn_function_value_regno_p
#undef  TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR gcn_gimplify_va_arg_expr
#undef TARGET_OMP_DEVICE_KIND_ARCH_ISA
#define TARGET_OMP_DEVICE_KIND_ARCH_ISA gcn_omp_device_kind_arch_isa
#undef  TARGET_GOACC_ADJUST_PRIVATE_DECL
#define TARGET_GOACC_ADJUST_PRIVATE_DECL gcn_goacc_adjust_private_decl
#undef  TARGET_GOACC_CREATE_WORKER_BROADCAST_RECORD
#define TARGET_GOACC_CREATE_WORKER_BROADCAST_RECORD \
  gcn_goacc_create_worker_broadcast_record
#undef  TARGET_GOACC_FORK_JOIN
#define TARGET_GOACC_FORK_JOIN gcn_fork_join
#undef  TARGET_GOACC_REDUCTION
#define TARGET_GOACC_REDUCTION gcn_goacc_reduction
#undef  TARGET_GOACC_VALIDATE_DIMS
#define TARGET_GOACC_VALIDATE_DIMS gcn_goacc_validate_dims
#undef  TARGET_GOACC_SHARED_MEM_LAYOUT
#define TARGET_GOACC_SHARED_MEM_LAYOUT gcn_shared_mem_layout
#undef  TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK gcn_hard_regno_mode_ok
#undef  TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS gcn_hard_regno_nregs
#undef  TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed
#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS gcn_init_builtins
#undef  TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS gcn_init_libfuncs
#undef  TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS
#define TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS \
  gcn_ira_change_pseudo_allocno_class
#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P gcn_legitimate_constant_p
#undef  TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION gcn_libc_has_function
#undef  TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_true
#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG gcn_md_reorg
#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST gcn_memory_move_cost
#undef  TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P gcn_modes_tieable_p
#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE gcn_option_override
#undef  TARGET_PRETEND_OUTGOING_VARARGS_NAMED
#define TARGET_PRETEND_OUTGOING_VARARGS_NAMED \
  gcn_pretend_outgoing_varargs_named
#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE gcn_promote_function_mode
#undef  TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST gcn_register_move_cost
#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY gcn_return_in_memory
#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS gcn_rtx_costs
#undef  TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD gcn_secondary_reload
#undef  TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS gcn_section_type_flags
#undef  TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P gcn_scalar_mode_supported_p
#undef  TARGET_SIMD_CLONE_ADJUST
#define TARGET_SIMD_CLONE_ADJUST gcn_simd_clone_adjust
#undef  TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN
#define TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN \
  gcn_simd_clone_compute_vecsize_and_simdlen
#undef  TARGET_SIMD_CLONE_USABLE
#define TARGET_SIMD_CLONE_USABLE gcn_simd_clone_usable
#undef  TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P \
  gcn_small_register_classes_for_mode_p
#undef  TARGET_SPILL_CLASS
#define TARGET_SPILL_CLASS gcn_spill_class
#undef  TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING gcn_strict_argument_naming
#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT gcn_trampoline_init
#undef  TARGET_TRULY_NOOP_TRUNCATION
#define TARGET_TRULY_NOOP_TRUNCATION gcn_truly_noop_truncation
#undef  TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST gcn_vectorization_cost
#undef  TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION
#define TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION \
  gcn_vectorize_builtin_vectorized_function
#undef  TARGET_VECTORIZE_GET_MASK_MODE
#define TARGET_VECTORIZE_GET_MASK_MODE gcn_vectorize_get_mask_mode
#undef  TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE gcn_vectorize_preferred_simd_mode
#undef  TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT
#define TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT \
  gcn_preferred_vector_alignment
#undef  TARGET_VECTORIZE_RELATED_MODE
#define TARGET_VECTORIZE_RELATED_MODE gcn_related_vector_mode
#undef  TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT
#define TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT \
  gcn_vectorize_support_vector_misalignment
#undef  TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST gcn_vectorize_vec_perm_const
#undef  TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE
#define TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE \
  gcn_vector_alignment_reachable
#undef  TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P gcn_vector_mode_supported_p

#undef TARGET_DOCUMENTATION_NAME
#define TARGET_DOCUMENTATION_NAME "AMD GCN"

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-gcn.h"
/* }}}  */
