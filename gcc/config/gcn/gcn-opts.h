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

#endif
