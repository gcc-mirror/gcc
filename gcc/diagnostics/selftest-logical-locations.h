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

#ifndef GCC_DIAGNOSTICS_SELFTEST_LOGICAL_LOCATIONS_H
#define GCC_DIAGNOSTICS_SELFTEST_LOGICAL_LOCATIONS_H

#include "diagnostics/logical-locations.h"

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace diagnostics {
namespace logical_locations {
namespace selftest {

/* Concrete subclass of logical_locations::manager for use in selftests.  */

class test_manager : public manager
{
public:
  ~test_manager ();

  void dump (FILE *out, int indent) const final override;

  const char *get_short_name (key) const final override;
  const char *get_name_with_scope (key) const final override;
  const char *get_internal_name (key) const final override;
  kind get_kind (key) const final override;
  label_text get_name_for_path_output (key) const final override;
  key get_parent (key) const final override
  {
    return key ();
  }

  key
  logical_location_from_funcname (const char *funcname);

private:
  struct item
  {
    item (kind kind_,
	  const char *name)
    : m_kind (kind_),
      m_name (name)
    {
    }

    kind m_kind;
    const char *m_name;
  };

  const item *
  item_from_funcname (const char *funcname);

  static const item *item_from_key (key k)
  {
    return k.cast_to<const item *> ();
  }

  hash_map<nofree_string_hash, item *> m_name_to_item_map;
};

} // namespace diagnostics::logical_locations::selftest
} // namespace diagnostics::logical_locations::
} // namespace diagnostics

#endif /* #if CHECKING_P */

#endif /* GCC_DIAGNOSTICS_SELFTEST_LOGICAL_LOCATIONS_H.  */
