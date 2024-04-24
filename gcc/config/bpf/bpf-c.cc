/* BPF-specific code for C family languages.
   Copyright (C) 2024 Free Software Foundation, Inc.
   Contributed by Oracle Inc.

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
#include "tm.h"
#include "c-family/c-common.h"
#include "cpplib.h"

/* Define target-specific CPP macros.  This function in used in the
   definition of TARGET_CPU_CPP_BUILTINS in bpf.h */

#define builtin_define(TXT) cpp_define (pfile, TXT)

void
bpf_target_macros (cpp_reader *pfile)
{
  builtin_define ("__BPF__");
  builtin_define ("__bpf__");

  if (TARGET_BIG_ENDIAN)
    builtin_define ("__BPF_BIG_ENDIAN__");
  else
    builtin_define ("__BPF_LITTLE_ENDIAN__");

  switch (bpf_isa)
    {
    case ISA_V1:
      builtin_define_with_int_value ("__BPF_CPU_VERSION__", 1);
      break;
    case ISA_V2:
      builtin_define_with_int_value ("__BPF_CPU_VERSION__", 2);
      break;
    case ISA_V3:
      builtin_define_with_int_value ("__BPF_CPU_VERSION__", 3);
      break;
    case ISA_V4:
      builtin_define_with_int_value ("__BPF_CPU_VERSION__", 4);
      break;
    default:
      gcc_unreachable ();
      break;
    }

  /* Different BPF CPU versions support different features.  Some of
     them can be enabled/disabled explicitly.  */
  if (bpf_has_alu32)
    builtin_define ("__BPF_FEATURE_ALU32");
  if (bpf_has_jmp32)
    builtin_define ("__BPF_FEATURE_JMP32");
  if (bpf_has_jmpext)
    builtin_define ("__BPF_FEATURE_JMP_EXT");
  if (bpf_has_bswap)
    builtin_define ("__BPF_FEATURE_BSWAP");
  if (bpf_has_sdiv)
    builtin_define ("__BPF_FEATURE_SDIV_SMOD");
  if (bpf_has_smov)
    builtin_define ("__BPF_FEATURE_MOVSX");

  /* Other CPU features can only be enabled/disabled generically by
     selecting the corresponding CPU version.  */
  if (bpf_isa >= ISA_V4)
    {
      builtin_define ("__BPF_FEATURE_LDSX");
      builtin_define ("__BPF_FEATURE_GOTOL");
      builtin_define ("__BPF_FEATURE_ST");
    }
}
