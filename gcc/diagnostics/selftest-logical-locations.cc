/* Concrete subclass of logical_locations::manager for use in selftests.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.
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
#include "diagnostics/selftest-logical-locations.h"
#include "diagnostics/dumping.h"

#if CHECKING_P

namespace diagnostics {
namespace logical_locations {
namespace selftest {

/* class test_manager : public manager.  */

test_manager::~test_manager ()
{
  for (auto iter : m_name_to_item_map)
    delete iter.second;
}

void
test_manager::dump (FILE *outfile, int indent) const
{
  dumping::emit_heading (outfile, indent, "test_manager");
}

const char *
test_manager::get_short_name (key k) const
{
  auto item = item_from_key (k);
  if (!item)
    return nullptr;
  return item->m_name;
}

const char *
test_manager::get_name_with_scope (key k) const
{
  auto item = item_from_key (k);
  return item->m_name;
}

const char *
test_manager::get_internal_name (key k) const
{
  auto item = item_from_key (k);
  return item->m_name;
}

enum diagnostics::logical_locations::kind
test_manager::get_kind (key k) const
{
  auto item = item_from_key (k);
  return item->m_kind;
}

label_text
test_manager::get_name_for_path_output (key k) const
{
  auto item = item_from_key (k);
  return label_text::borrow (item->m_name);
}

diagnostics::logical_locations::key
test_manager::
logical_location_from_funcname (const char *funcname)
{
  const item *i = item_from_funcname (funcname);
  return key::from_ptr (i);
}

const test_manager::item *
test_manager::item_from_funcname (const char *funcname)
{
  if (!funcname)
    return nullptr;

  if (item **slot = m_name_to_item_map.get (funcname))
    return *slot;

  item *i = new item (kind::function, funcname);
  m_name_to_item_map.put (funcname, i);
  return i;
}

/* Run all of the selftests within this file.  */

void
selftest_logical_locations_cc_tests ()
{
  test_manager mgr;

  ASSERT_FALSE (mgr.logical_location_from_funcname (nullptr));

  key loc_foo = mgr.logical_location_from_funcname ("foo");
  key loc_bar = mgr.logical_location_from_funcname ("bar");

  ASSERT_NE (loc_foo, loc_bar);

  ASSERT_STREQ (mgr.get_short_name (loc_foo), "foo");
  ASSERT_STREQ (mgr.get_short_name (loc_bar), "bar");
}

} // namespace diagnostics::logical_locations::selftest
} // namespace diagnostics::logical_locations
} // namespace diagnostics

#endif /* #if CHECKING_P */
