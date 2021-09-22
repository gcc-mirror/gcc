/* Subroutine headers for the gcc driver.
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

#ifndef LOONGARCH_DRIVER_H
#define LOONGARCH_DRIVER_H

extern const char*
driver_set_m_flag (int argc, const char **argv);

extern const char*
driver_get_normalized_m_opts (int argc, const char **argv);


#define EXTRA_SPEC_FUNCTIONS \
  { "set_m_flag", driver_set_m_flag  }, \
  { "get_normalized_m_opts", driver_get_normalized_m_opts  },

#define DRIVER_HANDLE_MACHINE_OPTIONS \
  " %{c|S|E: %:set_m_flag(no_link 1)} " \
  " %{mabi=*: %:set_m_flag(abi %*)} %<mabi=*" \
  " %{march=*: %:set_m_flag(arch %*)} %<march=*" \
  " %{mtune=*: %:set_m_flag(tune %*)} %<mtune=*" \
  " %{mfloat-abi=*: %:set_m_flag(float-abi %*)} %<mfloat-abi=*" \
  " %{mfpu=*: %:set_m_flag(fpu %*)} %<mfpu=*" \
  " %:get_normalized_m_opts()"

#define DRIVER_SELF_SPECS \
  DRIVER_HANDLE_MACHINE_OPTIONS

#endif /* LOONGARCH_DRIVER_H */

