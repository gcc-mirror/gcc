/* Definitions for loongarch native cpu property detection routines.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef LOONGARCH_CPU_H
#define LOONGARCH_CPU_H

#include "system.h"
#include "loongarch-def.h"

void cache_cpucfg (void);
void fill_native_cpu_config (struct loongarch_target *tgt);
uint32_t get_native_prid (void);
const char* get_native_prid_str (void);

#endif /* LOONGARCH_CPU_H */
