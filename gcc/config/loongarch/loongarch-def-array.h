/* A std::array like data structure for LoongArch static properties.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

#ifndef _LOONGARCH_DEF_ARRAY_H
#define _LOONGARCH_DEF_ARRAY_H 1

template <class T, int N>
class loongarch_def_array {
private:
  T arr[N];
public:
  loongarch_def_array () : arr{} {}

  T &operator[] (int n) { return arr[n]; }
  const T &operator[] (int n) const { return arr[n]; }

  loongarch_def_array set (int idx, T &&value)
  {
    (*this)[idx] = value;
    return *this;
  }
};

#endif
