/* Concrete subclass of logical_location for use in selftests.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "selftest-logical-location.h"

#if CHECKING_P

namespace selftest {

/* class test_logical_location : public logical_location.  */

test_logical_location::test_logical_location (enum logical_location_kind kind,
					      const char *name)
: m_kind (kind),
  m_name (name)
{
}

const char *
test_logical_location::get_short_name () const
{
  return m_name;
}

const char *
test_logical_location::get_name_with_scope () const
{
  return m_name;
}

const char *
test_logical_location::get_internal_name () const
{
  return m_name;
}

enum logical_location_kind
test_logical_location::get_kind () const
{
  return m_kind;
}

label_text
test_logical_location::get_name_for_path_output () const
{
  return label_text::borrow (m_name);
}

} // namespace selftest

#endif /* #if CHECKING_P */
