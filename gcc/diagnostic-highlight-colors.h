/* Symbolic names for highlighting colors in diagnostics.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTIC_HIGHLIGHT_COLORS_H
#define GCC_DIAGNOSTIC_HIGHLIGHT_COLORS_H

/* Symbolic names for highlight colors in diagnostics, so that e.g.
   in

warning: format `%i' expects argument of type `int',
 but argument 2 has type `const char *' [-Wformat=]
  279 |   printf("hello " INT_FMT " world", msg);
      |          ^~~~~~~~                   ~~~
      |                                     |
      |                                     const char *
note: format string is defined here
  278 | #define INT_FMT "%i"
      |                  ~^
      |                   |
      |                   int
      |                  %s

    we can refer to the color of "int" as highlight_colors::expected
    and the color of "const char *" as highlight_colors::actual
    to help get consistent contrasting colorization, both for the types
    within the diagnostic messages, and the underlined ranges.  */

namespace highlight_colors {

/* Color names for expressing "expected" vs "actual" values.  */
extern const char *const expected;
extern const char *const actual;

/* Color names for expressing "LHS" vs "RHS" values in a binary operation
   or when we are listing two different things.  */
extern const char *const lhs;
extern const char *const rhs;

} // namespace highlight_colors

#endif /* ! GCC_DIAGNOSTIC_HIGHLIGHT_COLORS_H */
