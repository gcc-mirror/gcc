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

/* Which processor to generate code or schedule for.  */
enum processor_type
{
  PROCESSOR_FIJI,    // gfx803
  PROCESSOR_VEGA10,  // gfx900
  PROCESSOR_VEGA20,  // gfx906
  PROCESSOR_GFX908,
  PROCESSOR_GFX90a,
  PROCESSOR_GFX90c,
  PROCESSOR_GFX1030,
  PROCESSOR_GFX1036,
  PROCESSOR_GFX1100,
  PROCESSOR_GFX1103
};

#define TARGET_FIJI (gcn_arch == PROCESSOR_FIJI)
#define TARGET_VEGA10 (gcn_arch == PROCESSOR_VEGA10)
#define TARGET_VEGA20 (gcn_arch == PROCESSOR_VEGA20)
#define TARGET_GFX908 (gcn_arch == PROCESSOR_GFX908)
#define TARGET_GFX90a (gcn_arch == PROCESSOR_GFX90a)
#define TARGET_GFX90c (gcn_arch == PROCESSOR_GFX90c)
#define TARGET_GFX1030 (gcn_arch == PROCESSOR_GFX1030)
#define TARGET_GFX1036 (gcn_arch == PROCESSOR_GFX1036)
#define TARGET_GFX1100 (gcn_arch == PROCESSOR_GFX1100)
#define TARGET_GFX1103 (gcn_arch == PROCESSOR_GFX1103)

/* Set in gcn_option_override.  */
extern enum gcn_isa {
  ISA_UNKNOWN,
  ISA_GCN3,
  ISA_GCN5,
  ISA_RDNA2,
  ISA_RDNA3,
  ISA_CDNA1,
  ISA_CDNA2
} gcn_isa;

#define TARGET_GCN3 (gcn_isa == ISA_GCN3)
#define TARGET_GCN3_PLUS (gcn_isa >= ISA_GCN3)
#define TARGET_GCN5 (gcn_isa == ISA_GCN5)
#define TARGET_GCN5_PLUS (gcn_isa >= ISA_GCN5)
#define TARGET_CDNA1 (gcn_isa == ISA_CDNA1)
#define TARGET_CDNA1_PLUS (gcn_isa >= ISA_CDNA1)
#define TARGET_CDNA2 (gcn_isa == ISA_CDNA2)
#define TARGET_CDNA2_PLUS (gcn_isa >= ISA_CDNA2)
#define TARGET_RDNA2 (gcn_isa == ISA_RDNA2)
#define TARGET_RDNA2_PLUS (gcn_isa >= ISA_RDNA2 && gcn_isa < ISA_CDNA1)
#define TARGET_RDNA3 (gcn_isa == ISA_RDNA3)


#define TARGET_M0_LDS_LIMIT (TARGET_GCN3)
#define TARGET_PACKED_WORK_ITEMS (TARGET_CDNA2_PLUS || TARGET_RDNA3)

#define TARGET_XNACK (flag_xnack != HSACO_ATTR_OFF)

enum hsaco_attr_type
{
  HSACO_ATTR_OFF,
  HSACO_ATTR_ON,
  HSACO_ATTR_ANY,
  HSACO_ATTR_DEFAULT
};

/* There are global address instructions.  */
#define TARGET_GLOBAL_ADDRSPACE TARGET_GCN5_PLUS
/* Device has an AVGPR register file.  */
#define TARGET_AVGPRS TARGET_CDNA1_PLUS
/* There are load/store instructions for AVGPRS.  */
#define TARGET_AVGPR_MEMOPS TARGET_CDNA2_PLUS
/* AVGPRS may have their own register file, or be combined with VGPRS.  */
#define TARGET_AVGPR_COMBINED TARGET_CDNA2_PLUS
/* flat_load/store allows offsets.  */
#define TARGET_FLAT_OFFSETS TARGET_GCN5_PLUS
/* global_load/store has reduced offset.  */
#define TARGET_11BIT_GLOBAL_OFFSET TARGET_RDNA2_PLUS
/* The work item details are all encoded into v0.  */
//#define TARGET_PACKED_WORK_ITEMS TARGET_PACKED_WORK_ITEMS
/* m0 must be initialized in order to use LDS.  */
//#define TARGET_M0_LDS_LIMIT TARGET_M0_LDS_LIMIT
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
/* Assembler uses s_add_co not just s_add.  */
#define TARGET_EXPLICIT_CARRY TARGET_GCN5_PLUS
/* mulsi3 permits immediate.  */
#define TARGET_MULTIPLY_IMMEDIATE TARGET_GCN5_PLUS
/* Device has Sub-DWord Addressing instrucions.  */
#define TARGET_SDWA (!TARGET_RDNA3)
/* Different devices uses different cache control instructions.  */
#define TARGET_WBINVL1_CACHE (!TARGET_RDNA2_PLUS)
#define TARGET_GLn_CACHE TARGET_RDNA2_PLUS

#endif
