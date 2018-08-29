/* Definitions for option handling of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef NDS32_OPTS_H
#define NDS32_OPTS_H

#define NDS32_DEFAULT_CACHE_BLOCK_SIZE 16
#define NDS32_DEFAULT_ISR_VECTOR_SIZE (TARGET_ISA_V3 ? 4 : 16)

/* The various ANDES ISA.  */
enum nds32_arch_type
{
  ARCH_V2,
  ARCH_V3,
  ARCH_V3J,
  ARCH_V3M,
  ARCH_V3F,
  ARCH_V3S
};

/* The various ANDES CPU.  */
enum nds32_cpu_type
{
  CPU_N6,
  CPU_N7,
  CPU_N8,
  CPU_E8,
  CPU_N9,
  CPU_N10,
  CPU_GRAYWOLF,
  CPU_N12,
  CPU_N13,
  CPU_SIMPLE
};

/* The code model defines the address generation strategy.  */
enum nds32_cmodel_type
{
  CMODEL_SMALL,
  CMODEL_MEDIUM,
  CMODEL_LARGE
};

/* The code model defines the address generation strategy.  */
enum nds32_ict_model_type
{
  ICT_MODEL_SMALL,
  ICT_MODEL_LARGE
};

/* Multiply instruction configuration.  */
enum nds32_mul_type
{
  MUL_TYPE_FAST_1,
  MUL_TYPE_FAST_2,
  MUL_TYPE_SLOW
};

/* Register ports configuration.  */
enum nds32_register_ports
{
  REG_PORT_3R2W,
  REG_PORT_2R1W
};

/* Which ABI to use.  */
enum abi_type
{
  NDS32_ABI_V2,
  NDS32_ABI_V2_FP_PLUS
};

/* The various FPU number of registers.  */
enum float_reg_number
{
  NDS32_CONFIG_FPU_0,
  NDS32_CONFIG_FPU_1,
  NDS32_CONFIG_FPU_2,
  NDS32_CONFIG_FPU_3,
  NDS32_CONFIG_FPU_4,
  NDS32_CONFIG_FPU_5,
  NDS32_CONFIG_FPU_6,
  NDS32_CONFIG_FPU_7
};

#endif
