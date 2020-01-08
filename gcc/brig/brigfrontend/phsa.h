/* phsa.h -- interfacing between the gcc BRIG FE and the phsa runtime
   Copyright (C) 2016-2020 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef PHSA_H
#define PHSA_H

#include <stdint.h>

/* This struct is used to pass information from the BRIG FE to the
   runtime of the finalizer kernel, its control directives etc.
   The data is passed raw in a special ELF section named
   phsa.kerneldesc.kernel_function_name.  */

typedef struct __attribute__((__packed__))
{
  /* Set to 1 in case the function is a kernel.  */
  uint8_t is_kernel;
  /* The size of the group segment used by the kernel.  */
  uint32_t group_segment_size;
  /* Size of the private segment used by a single work-item.  */
  uint32_t private_segment_size;
  /* Total size of the kernel arguments.  */
  uint32_t kernarg_segment_size;
  /* Maximum alignment of a kernel argument variable.  */
  uint16_t kernarg_max_align;
  /* Maximum size (in bytes) of dynamic group memory.  */
  uint32_t max_dynamic_group_size;
  /* Max number of work-items used to launch the kernel.  */
  uint64_t max_flat_grid_size;
  /* Max number of work-items in a work-group used to launch the kernel.  */
  uint32_t max_flat_workgroup_size;
  /* The grid size required by the kernel.  */
  uint64_t required_grid_size[3];
  /* The work group size required by the kernel.  */
  uint32_t required_workgroup_size[3];
  /* The number of dimensions required by the kernel.  */
  uint8_t required_dim;

} phsa_descriptor;

/* The prefix to use in the ELF section containing descriptor for
   a function.  */

#define PHSA_DESC_SECTION_PREFIX "phsa.desc."
#define PHSA_HOST_DEF_PTR_PREFIX "__phsa.host_def."

/* The frontend error messages are parsed by the host runtime.  Known
   prefix strings are used to separate the different runtime error
   codes.  */

#define PHSA_ERROR_PREFIX_INCOMPATIBLE_MODULE "Incompatible module: "
#define PHSA_ERROR_PREFIX_CORRUPTED_MODULE "Corrupted module: "

/* Offsets of attributes in the PHSA context structs.
   Used by -fphsa-wi-context-opt.  */
#define PHSA_CONTEXT_OFFS_WI_IDS 0
#define PHSA_CONTEXT_OFFS_WG_IDS (PHSA_CONTEXT_OFFS_WI_IDS + 3 * 4)
#define PHSA_CONTEXT_WG_SIZES (PHSA_CONTEXT_OFFS_WG_IDS + 3 * 4)
#define PHSA_CONTEXT_CURRENT_WG_SIZES (PHSA_CONTEXT_WG_SIZES + 3 * 4)

#endif
