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

#include "rust-privacy-ctx.h"
#include "selftest.h"

namespace Rust {
namespace Privacy {

static ReachLevel
insert_if_higher (ReachLevel new_level,
		  std::unordered_map<DefId, ReachLevel>::iterator &existing)
{
  if (new_level > existing->second)
    existing->second = new_level;

  return existing->second;
}

ReachLevel
PrivacyContext::update_reachability (const Analysis::NodeMapping &mapping,
				     ReachLevel reach)
{
  auto def_id = mapping.get_defid ();
  auto existing_reach = reachability_map.find (def_id);
  if (existing_reach != reachability_map.end ())
    return insert_if_higher (reach, existing_reach);

  reachability_map.insert ({def_id, reach});
  return reach;
}

const ReachLevel *
PrivacyContext::lookup_reachability (const Analysis::NodeMapping &mapping)
{
  auto existing_reach = reachability_map.find (mapping.get_defid ());
  if (existing_reach == reachability_map.end ())
    return nullptr;

  return &existing_reach->second;
}
} // namespace Privacy
} // namespace Rust

#if CHECKING_P
namespace selftest {
static void
update_reachability_test (void)
{
  auto ctx = Rust::Privacy::PrivacyContext ();
  // Bogus values for the mappings
  auto mapping = Rust::Analysis::NodeMapping (15, 15, 15, 15);

  auto new_level
    = ctx.update_reachability (mapping, Rust::Privacy::ReachLevel::Unreachable);

  ASSERT_EQ (new_level, Rust::Privacy::ReachLevel::Unreachable);

  ASSERT_TRUE (ctx.lookup_reachability (mapping));
  ASSERT_EQ (*ctx.lookup_reachability (mapping),
	     Rust::Privacy::ReachLevel::Unreachable);

  new_level
    = ctx.update_reachability (mapping, Rust::Privacy::ReachLevel::Reachable);

  ASSERT_EQ (new_level, Rust::Privacy::ReachLevel::Reachable);
  ASSERT_TRUE (ctx.lookup_reachability (mapping));
  ASSERT_EQ (*ctx.lookup_reachability (mapping),
	     Rust::Privacy::ReachLevel::Reachable);
}

void
rust_privacy_ctx_test (void)
{
  update_reachability_test ();
}
} // namespace selftest
#endif // !CHECKING_P
