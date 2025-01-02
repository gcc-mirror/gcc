/* Copyright (C) 2023-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

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

#ifndef GCC_PRETTY_PRINT_URLIFIER_H
#define GCC_PRETTY_PRINT_URLIFIER_H

/* Abstract base class for optional use in pretty-printing for adding URLs
   to quoted text strings.  */

class urlifier
{
public:
  virtual ~urlifier () {}
  virtual char *get_url_for_quoted_text (const char *p, size_t sz) const = 0;
};

#endif /* GCC_PRETTY_PRINT_URLIFIER_H */
