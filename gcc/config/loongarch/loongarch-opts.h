/* Definitions for option handling for LoongArch.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

#ifndef LOONGARCH_OPTS_H
#define LOONGARCH_OPTS_H

/* No enumeration is defined to index the -march= values (entries in
   loongarch_cpu_info_table), with the type int being used instead, but we
   need to distinguish the special "from-abi" and "native" values.  */
#define LARCH_ARCH_OPTION_NATIVE -1


enum loongarch_code_model {
  LARCH_CMODEL_NORMAL,
  LARCH_CMODEL_TINY,
  LARCH_CMODEL_TINY_STATIC,
  LARCH_CMODEL_LARGE,
  LARCH_CMODEL_EXTREME
};

#endif
