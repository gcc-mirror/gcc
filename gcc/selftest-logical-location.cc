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
#include "selftest.h"
#include "selftest-logical-location.h"

#if CHECKING_P

namespace selftest {

/* class test_logical_location_manager : public logical_location_manager.  */

test_logical_location_manager::~test_logical_location_manager ()
{
  for (auto iter : m_name_to_item_map)
    delete iter.second;
}

const char *
test_logical_location_manager::get_short_name (key k) const
{
  auto item = item_from_key (k);
  if (!item)
    return nullptr;
  return item->m_name;
}

const char *
test_logical_location_manager::get_name_with_scope (key k) const
{
  auto item = item_from_key (k);
  return item->m_name;
}

const char *
test_logical_location_manager::get_internal_name (key k) const
{
  auto item = item_from_key (k);
  return item->m_name;
}

enum logical_location_kind
test_logical_location_manager::get_kind (key k) const
{
  auto item = item_from_key (k);
  return item->m_kind;
}

label_text
test_logical_location_manager::get_name_for_path_output (key k) const
{
  auto item = item_from_key (k);
  return label_text::borrow (item->m_name);
}

logical_location
test_logical_location_manager::
logical_location_from_funcname (const char *funcname)
{
  const item *i = item_from_funcname (funcname);
  return key::from_ptr (i);
}

const test_logical_location_manager::item *
test_logical_location_manager::item_from_funcname (const char *funcname)
{
  if (!funcname)
    return nullptr;

  if (item **slot = m_name_to_item_map.get (funcname))
    return *slot;

  item *i = new item (LOGICAL_LOCATION_KIND_FUNCTION, funcname);
  m_name_to_item_map.put (funcname, i);
  return i;
}

/* Run all of the selftests within this file.  */

void
selftest_logical_location_cc_tests ()
{
  test_logical_location_manager mgr;

  ASSERT_FALSE (mgr.logical_location_from_funcname (nullptr));

  logical_location loc_foo = mgr.logical_location_from_funcname ("foo");
  logical_location loc_bar = mgr.logical_location_from_funcname ("bar");

  ASSERT_NE (loc_foo, loc_bar);

  ASSERT_STREQ (mgr.get_short_name (loc_foo), "foo");
  ASSERT_STREQ (mgr.get_short_name (loc_bar), "bar");
}

} // namespace selftest

#endif /* #if CHECKING_P */
