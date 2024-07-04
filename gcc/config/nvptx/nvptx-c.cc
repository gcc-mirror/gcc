/* Subroutines for the C front end on the NVPTX architecture.
 * Copyright (C) 2021-2024 Free Software Foundation, Inc.
 *
 * This file is part of GCC.
 *
 * GCC is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3, or (at your
 * option) any later version.
 *
 * GCC is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GCC; see the file COPYING3.  If not see
 * <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-family/c-common.h"
#include "memmodel.h"
#include "tm_p.h"
#include "c-family/c-pragma.h"

/* Function to tell the preprocessor about the defines for the target.  */
void
nvptx_cpu_cpp_builtins (void)
{
  cpp_assert (parse_in, "machine=nvptx");
  cpp_assert (parse_in, "cpu=nvptx");
  cpp_define (parse_in, "__nvptx__");
  if (TARGET_SOFT_STACK)
    cpp_define (parse_in, "__nvptx_softstack__");
  if (TARGET_UNIFORM_SIMT)
    cpp_define (parse_in,"__nvptx_unisimt__");

  const char *ptx_sm = NULL;
#define NVPTX_SM(XX, SEP) \
  {						\
    if (TARGET_SM ## XX)			\
      ptx_sm = "__PTX_SM__=" #XX "0"; \
  }
#include "nvptx-sm.def"
#undef NVPTX_SM
  cpp_define (parse_in, ptx_sm);

  {
    unsigned major
      = ptx_version_to_number ((ptx_version)ptx_version_option, true);
    unsigned minor
      = ptx_version_to_number ((ptx_version)ptx_version_option, false);
    cpp_define_formatted (parse_in, "__PTX_ISA_VERSION_MAJOR__=%u", major);
    cpp_define_formatted (parse_in, "__PTX_ISA_VERSION_MINOR__=%u", minor);
  }
}

