/* Copyright (C) 2016-2023 Free Software Foundation, Inc.

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
  PROCESSOR_GFX90a
};

#define TARGET_FIJI (gcn_arch == PROCESSOR_FIJI)
#define TARGET_VEGA10 (gcn_arch == PROCESSOR_VEGA10)
#define TARGET_VEGA20 (gcn_arch == PROCESSOR_VEGA20)
#define TARGET_GFX908 (gcn_arch == PROCESSOR_GFX908)
#define TARGET_GFX90a (gcn_arch == PROCESSOR_GFX90a)

/* Set in gcn_option_override.  */
extern enum gcn_isa {
  ISA_UNKNOWN,
  ISA_GCN3,
  ISA_GCN5,
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

#define TARGET_M0_LDS_LIMIT (TARGET_GCN3)
#define TARGET_PACKED_WORK_ITEMS (TARGET_CDNA2_PLUS)

enum sram_ecc_type
{
  SRAM_ECC_OFF,
  SRAM_ECC_ON,
  SRAM_ECC_ANY
};

#endif
