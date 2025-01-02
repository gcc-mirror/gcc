/* Types shared between arm and aarch64.

   Copyright (C) 2009-2025 Free Software Foundation, Inc.
   Contributed by Arm Ltd.

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

#ifndef GCC_AARCH_COMMON_H
#define GCC_AARCH_COMMON_H

/* Enum describing the various ways that the
   aarch*_parse_{arch,tune,cpu,extension,fmv_extension} functions can fail.
   This way their callers can choose what kind of error to give.  */

enum aarch_parse_opt_result
{
  AARCH_PARSE_OK,			/* Parsing was successful.  */
  AARCH_PARSE_MISSING_ARG,		/* Missing argument.  */
  AARCH_PARSE_INVALID_FEATURE,		/* Invalid feature modifier.  */
  AARCH_PARSE_INVALID_ARG,		/* Invalid arch, tune, cpu arg.  */
  AARCH_PARSE_DUPLICATE_FEATURE		/* Duplicate feature modifier.  */
};

/* Function types -msign-return-address should sign.  */
enum aarch_function_type {
  /* Don't sign any function.  */
  AARCH_FUNCTION_NONE,
  /* Non-leaf functions.  */
  AARCH_FUNCTION_NON_LEAF,
  /* All functions.  */
  AARCH_FUNCTION_ALL
};

#endif /* GCC_AARCH_COMMON_H */
