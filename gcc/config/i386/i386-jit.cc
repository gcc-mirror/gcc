/* Subroutines for the JIT front end on the x86 architecture.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tm.h"
#include "tm_jit.h"
#include "jit/jit-target.h"
#include "jit/jit-target-def.h"

/* Implement TARGET_JIT_REGISTER_CPU_TARGET_INFO.  */

#ifndef CROSS_DIRECTORY_STRUCTURE
extern const char *host_detect_local_cpu (int argc, const char **argv);
#endif

#if TARGET_64BIT_DEFAULT
const char* x86_bits = "64";
#else
const char* x86_bits = "32";
#endif

void
ix86_jit_register_target_info (void)
{
#ifndef CROSS_DIRECTORY_STRUCTURE
  const char *params[] = {"arch", x86_bits};
  const char* local_cpu = host_detect_local_cpu (2, params);
  if (local_cpu)
  {
    std::string arch = local_cpu;
    free (const_cast <char *> (local_cpu));

    const char* arg = "-march=";
    size_t arg_pos = arch.find (arg) + strlen (arg);
    size_t end_pos = arch.find (" ", arg_pos);

    std::string cpu = arch.substr (arg_pos, end_pos - arg_pos);
    jit_target_set_arch (cpu);
  }
#endif

  if (targetm.scalar_mode_supported_p (TImode))
  {
    jit_target_add_supported_target_dependent_type (GCC_JIT_TYPE_UINT128_T);
    jit_target_add_supported_target_dependent_type (GCC_JIT_TYPE_INT128_T);
  }

  if (float16_type_node != NULL && TYPE_PRECISION (float16_type_node) == 16)
    jit_target_add_supported_target_dependent_type (GCC_JIT_TYPE_FLOAT16);

  if (float32_type_node != NULL && TYPE_PRECISION (float32_type_node) == 32)
    jit_target_add_supported_target_dependent_type (GCC_JIT_TYPE_FLOAT32);

  if (float64_type_node != NULL && TYPE_PRECISION (float64_type_node) == 64)
    jit_target_add_supported_target_dependent_type (GCC_JIT_TYPE_FLOAT64);

  if (float128_type_node != NULL && TYPE_PRECISION (float128_type_node) == 128)
    jit_target_add_supported_target_dependent_type (GCC_JIT_TYPE_FLOAT128);

#define ADD_TARGET_INFO jit_add_target_info
#include "i386-rust-and-jit.inc"
#undef ADD_TARGET_INFO
}
