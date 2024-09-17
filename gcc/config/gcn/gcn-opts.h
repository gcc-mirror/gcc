/* Copyright (C) 2016-2024 Free Software Foundation, Inc.

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

#ifndef GCN_OPTS_H
#define GCN_OPTS_H

/* Create constants for PROCESSOR_GFX???.  */
enum processor_type
{
#define GCN_DEVICE(name, NAME, ...) \
  PROCESSOR_ ## NAME,
#include "gcn-devices.def"
  PROCESSOR_COUNT
};

/* Set in gcn_option_override.  */
extern enum gcn_isa {
  ISA_UNKNOWN,
  ISA_GCN5,
  ISA_RDNA2,
  ISA_RDNA3,
  ISA_CDNA1,
  ISA_CDNA2
} gcn_isa;

#define TARGET_GCN5 (gcn_isa == ISA_GCN5)
#define TARGET_CDNA1 (gcn_isa == ISA_CDNA1)
#define TARGET_CDNA1_PLUS (gcn_isa >= ISA_CDNA1)
#define TARGET_CDNA2 (gcn_isa == ISA_CDNA2)
#define TARGET_CDNA2_PLUS (gcn_isa >= ISA_CDNA2)
#define TARGET_RDNA2 (gcn_isa == ISA_RDNA2)
#define TARGET_RDNA2_PLUS (gcn_isa >= ISA_RDNA2 && gcn_isa < ISA_CDNA1)
#define TARGET_RDNA3 (gcn_isa == ISA_RDNA3)


#define TARGET_PACKED_WORK_ITEMS (TARGET_CDNA2_PLUS || TARGET_RDNA3)

#define TARGET_XNACK (flag_xnack == HSACO_ATTR_ON \
		      || flag_xnack == HSACO_ATTR_ANY)

enum hsaco_attr_type
{
  HSACO_ATTR_UNSUPPORTED,
  HSACO_ATTR_OFF,
  HSACO_ATTR_ON,
  HSACO_ATTR_ANY,
  HSACO_ATTR_DEFAULT
};

/* Device has an AVGPR register file.  */
#define TARGET_AVGPRS TARGET_CDNA1_PLUS
/* There are load/store instructions for AVGPRS.  */
#define TARGET_AVGPR_MEMOPS TARGET_CDNA2_PLUS
/* AVGPRS may have their own register file, or be combined with VGPRS.  */
#define TARGET_AVGPR_COMBINED TARGET_CDNA2_PLUS
/* global_load/store has reduced offset.  */
#define TARGET_11BIT_GLOBAL_OFFSET TARGET_RDNA2_PLUS
/* The work item details are all encoded into v0.  */
//#define TARGET_PACKED_WORK_ITEMS TARGET_PACKED_WORK_ITEMS
/* CDNA2 load/store costs are reduced.
 * TODO: what does this mean?  */
#define TARGET_CDNA2_MEM_COSTS TARGET_CDNA2_PLUS
/* Wave32 devices running in wave64 compatibility mode.  */
#define TARGET_WAVE64_COMPAT TARGET_RDNA2_PLUS
/* RDNA devices have different DPP with reduced capabilities.  */
#define TARGET_DPP_FULL !TARGET_RDNA2_PLUS
#define TARGET_DPP16 TARGET_RDNA2_PLUS
#define TARGET_DPP8 TARGET_RDNA2_PLUS
/* Device requires CDNA1-style manually inserted wait states for AVGPRs.  */
#define TARGET_AVGPR_CDNA1_NOPS TARGET_CDNA1
/* The metadata on different devices need different granularity.  */
#define TARGET_VGPR_GRANULARITY \
  (TARGET_RDNA3 ? 12 \
   : TARGET_RDNA2_PLUS || TARGET_CDNA2_PLUS ? 8 \
   : 4)
/* This mostly affects the metadata.  */
#define TARGET_ARCHITECTED_FLAT_SCRATCH TARGET_RDNA3
/* Device has Sub-DWord Addressing instrucions.  */
#define TARGET_SDWA (!TARGET_RDNA3)
/* Different devices uses different cache control instructions.  */
#define TARGET_WBINVL1_CACHE (!TARGET_RDNA2_PLUS)
#define TARGET_GLn_CACHE TARGET_RDNA2_PLUS
/* Some devices have TGSPLIT, which needs at least metadata.  */
#define TARGET_TGSPLIT TARGET_CDNA2_PLUS

#endif
