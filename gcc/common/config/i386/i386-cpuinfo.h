/* Get CPU type and Features for x86 processors.
   Copyright (C) 2012-2020 Free Software Foundation, Inc.
   Contributed by Sriraman Tallam (tmsriram@google.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Processor Vendor and Models. */

enum processor_vendor
{
  VENDOR_INTEL = 1,
  VENDOR_AMD,
  VENDOR_OTHER,
  BUILTIN_VENDOR_MAX = VENDOR_OTHER,
  VENDOR_MAX
};

/* Any new types or subtypes have to be inserted at the end. */

enum processor_types
{
  INTEL_BONNELL = 1,
  INTEL_CORE2,
  INTEL_COREI7,
  AMDFAM10H,
  AMDFAM15H,
  INTEL_SILVERMONT,
  INTEL_KNL,
  AMD_BTVER1,
  AMD_BTVER2,
  AMDFAM17H,
  INTEL_KNM,
  INTEL_GOLDMONT,
  INTEL_GOLDMONT_PLUS,
  INTEL_TREMONT,
  AMDFAM19H,
  CPU_TYPE_MAX,
  BUILTIN_CPU_TYPE_MAX = CPU_TYPE_MAX
};

enum processor_subtypes
{
  INTEL_COREI7_NEHALEM = 1,
  INTEL_COREI7_WESTMERE,
  INTEL_COREI7_SANDYBRIDGE,
  AMDFAM10H_BARCELONA,
  AMDFAM10H_SHANGHAI,
  AMDFAM10H_ISTANBUL,
  AMDFAM15H_BDVER1,
  AMDFAM15H_BDVER2,
  AMDFAM15H_BDVER3,
  AMDFAM15H_BDVER4,
  AMDFAM17H_ZNVER1,
  INTEL_COREI7_IVYBRIDGE,
  INTEL_COREI7_HASWELL,
  INTEL_COREI7_BROADWELL,
  INTEL_COREI7_SKYLAKE,
  INTEL_COREI7_SKYLAKE_AVX512,
  INTEL_COREI7_CANNONLAKE,
  INTEL_COREI7_ICELAKE_CLIENT,
  INTEL_COREI7_ICELAKE_SERVER,
  AMDFAM17H_ZNVER2,
  INTEL_COREI7_CASCADELAKE,
  INTEL_COREI7_TIGERLAKE,
  INTEL_COREI7_COOPERLAKE,
  AMDFAM19H_ZNVER3,
  CPU_SUBTYPE_MAX
};

/* Priority of i386 features, greater value is higher priority.   This is
   used to decide the order in which function dispatch must happen.  For
   instance, a version specialized for SSE4.2 should be checked for dispatch
   before a version for SSE3, as SSE4.2 implies SSE3.  */
enum feature_priority
{
  P_NONE = 0,
  P_MMX,
  P_SSE,
  P_SSE2,
  P_SSE3,
  P_SSSE3,
  P_PROC_SSSE3,
  P_SSE4_A,
  P_PROC_SSE4_A,
  P_SSE4_1,
  P_SSE4_2,
  P_PROC_SSE4_2,
  P_POPCNT,
  P_AES,
  P_PCLMUL,
  P_AVX,
  P_PROC_AVX,
  P_BMI,
  P_PROC_BMI,
  P_FMA4,
  P_XOP,
  P_PROC_XOP,
  P_FMA,
  P_PROC_FMA,
  P_BMI2,
  P_AVX2,
  P_PROC_AVX2,
  P_AVX512F,
  P_PROC_AVX512F,
  P_PROC_DYNAMIC
};

/* These are the values for vendor types, cpu types and subtypes.  Cpu
   types and subtypes should be subtracted by the corresponding start
   value.  */

#define M_CPU_TYPE_START (BUILTIN_VENDOR_MAX)
#define M_CPU_SUBTYPE_START \
  (M_CPU_TYPE_START + BUILTIN_CPU_TYPE_MAX)
#define M_VENDOR(a) (a)
#define M_CPU_TYPE(a) (M_CPU_TYPE_START + a)
#define M_CPU_SUBTYPE(a) (M_CPU_SUBTYPE_START + a)
