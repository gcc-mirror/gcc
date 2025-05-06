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

#ifndef GCC_SELFTEST_LOGICAL_LOCATION_H
#define GCC_SELFTEST_LOGICAL_LOCATION_H

#include "logical-location.h"

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace selftest {

/* Concrete subclass of logical_location_manager for use in selftests.  */

class test_logical_location_manager : public logical_location_manager
{
public:
  ~test_logical_location_manager ();

  const char *get_short_name (key) const final override;
  const char *get_name_with_scope (key) const final override;
  const char *get_internal_name (key) const final override;
  enum logical_location_kind get_kind (key) const final override;
  label_text get_name_for_path_output (key) const final override;
  key get_parent (key) const final override
  {
    return key ();
  }

  logical_location
  logical_location_from_funcname (const char *funcname);

private:
  struct item
  {
    item (enum logical_location_kind kind,
	  const char *name)
    : m_kind (kind),
      m_name (name)
    {
    }

    enum logical_location_kind m_kind;
    const char *m_name;
  };

  const item *
  item_from_funcname (const char *funcname);

  static const item *item_from_key (logical_location k)
  {
    return k.cast_to<const item *> ();
  }

  hash_map<nofree_string_hash, item *> m_name_to_item_map;
};

} // namespace selftest

#endif /* #if CHECKING_P */


#endif /* GCC_SELFTEST_LOGICAL_LOCATION_H.  */
