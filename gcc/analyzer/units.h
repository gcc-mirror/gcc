/* Traits for expressing units and dimensions in the type system.
   Copyright (C) 2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_ANALYZER_UNITS_H
#define GCC_ANALYZER_UNITS_H

/* Traits types for distinguishing different kinds of scalars
   and ranges, so that we get a type error if we mess up our units.  */

namespace ana {
namespace units {

/* Traits type representing the size of something in memory,
   measured in *bits*.  */

struct bit
{
  // Forbid creating instances: this is just a trait
  bit () = delete;

  static constexpr const char *dump_type_name = "bits";
  static constexpr const char *singular_dump_name = "bit";
  static constexpr const char *plural_dump_name = "bit";
};

/* Traits type representing the size of something in memory,
   measured in *bytes*.  */

struct byte
{
  // Forbid creating instances: this is just a trait
  byte () = delete;

  static constexpr const char *dump_type_name = "bytes";
  static constexpr const char *singular_dump_name = "byte";
  static constexpr const char *plural_dump_name = "bytes";
};

/* Enum for distinguishing different roles of a scalar
   so e.g. "10" vs "byte 10" for an offset vs "10 bytes" for a size.  */

enum class role
{
  generic,
  offset,
  size
};

} // namespace ana::units
} // namespace ana

#endif /* GCC_ANALYZER_UNITS_H */
