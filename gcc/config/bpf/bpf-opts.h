/* Definitions for option handling for eBPF.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef BPF_OPTS_H
#define BPF_OPTS_H

/* Supported versions of the Linux kernel.  */
enum bpf_kernel_version
{
  /* Linux 4.x */
  LINUX_V4_0,
  LINUX_V4_1,
  LINUX_V4_2,
  LINUX_V4_3,
  LINUX_V4_4,
  LINUX_V4_5,
  LINUX_V4_6,
  LINUX_V4_7,
  LINUX_V4_8,
  LINUX_V4_9,
  LINUX_V4_10,
  LINUX_V4_11,
  LINUX_V4_12,
  LINUX_V4_13,
  LINUX_V4_14,
  LINUX_V4_15,
  LINUX_V4_16,
  LINUX_V4_17,
  LINUX_V4_18,
  LINUX_V4_19,
  LINUX_V4_20,
  /* Linux 5.x  */
  LINUX_V5_0,
  LINUX_V5_1,
  LINUX_V5_2,
  LINUX_LATEST = LINUX_V5_2,
  LINUX_NATIVE,
};

#endif /* ! BPF_OPTS_H */
