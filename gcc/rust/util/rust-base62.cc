// Copyright (C) 2020-2024 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-base62.h"

namespace Rust {

std::string
base62_integer (uint64_t value)
{
  const static std::string base_64
    = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@$";
  std::string buffer (128, '\0');
  size_t idx = 0;
  size_t base = 62;

  do
    {
      buffer[idx] = base_64[(value % base)];
      idx++;
      value = value / base;
    }
  while (value != 0);

  std::reverse (buffer.begin (), buffer.begin () + idx);
  return buffer.substr (0, idx);
}

} // namespace Rust

// FIXME: Add unit testing using the selftest framework
