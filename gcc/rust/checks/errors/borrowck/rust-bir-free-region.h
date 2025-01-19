// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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
// <http://www.gnu.org/licenses/>

#ifndef RUST_BIR_FREE_REGION_H
#define RUST_BIR_FREE_REGION_H

#include "rust-diagnostics.h"
#include "polonius/rust-polonius-ffi.h"

namespace Rust {

using FreeRegion = size_t;

class FreeRegions
{
  std::vector<FreeRegion> regions;

public:
  WARN_UNUSED_RESULT bool has_regions () const { return !regions.empty (); }
  decltype (regions)::const_iterator begin () const { return regions.begin (); }
  decltype (regions)::const_iterator end () const { return regions.end (); }
  size_t size () const { return regions.size (); }
  FreeRegion &operator[] (size_t i) { return regions.at (i); }
  const FreeRegion &operator[] (size_t i) const { return regions.at (i); }
  const std::vector<FreeRegion> &get_regions () const { return regions; }
  void set_from (std::vector<Rust::Polonius::Origin> &&regions)
  {
    this->regions.clear ();
    for (auto &region : regions)
      {
	this->regions.push_back ({region});
      }
  }

  WARN_UNUSED_RESULT FreeRegions prepend (FreeRegion region) const
  {
    std::vector<FreeRegion> new_regions = {region};
    new_regions.insert (new_regions.end (), regions.begin (), regions.end ());
    return FreeRegions (std::move (new_regions));
  }

  FreeRegions (std::vector<FreeRegion> &&regions) : regions (regions) {}

  WARN_UNUSED_RESULT std::string to_string () const
  {
    std::stringstream result;
    for (auto &region : regions)
      {
	result << region;
	result << ", ";
      }
    // Remove the last ", " from the string.
    if (result.tellg () > 2)
      result.seekp (-2, std::ios_base::cur);

    return result.str ();
  }
};

class RegionBinder
{
  FreeRegion &next_free_region;

public:
  explicit RegionBinder (FreeRegion &next_free_region)
    : next_free_region (next_free_region)
  {}

  WARN_UNUSED_RESULT FreeRegion get_next_free_region () const
  {
    return next_free_region++;
  }

  FreeRegions bind_regions (std::vector<TyTy::Region> regions,
			    FreeRegions parent_free_regions)
  {
    std::vector<FreeRegion> free_regions;
    for (auto &region : regions)
      {
	if (region.is_early_bound ())
	  free_regions.push_back (parent_free_regions[region.get_index ()]);
	else if (region.is_static ())
	  free_regions.push_back (0);
	else if (region.is_anonymous ())
	  free_regions.push_back (get_next_free_region ());
	else if (region.is_named ())
	  rust_unreachable (); // FIXME
	else
	  {
	    rust_sorry_at (UNKNOWN_LOCATION, "Unimplemented");
	    rust_unreachable ();
	  }
      }
    // This is necesarry because of clash of current gcc and gcc4.8.
    FreeRegions free_regions_final{std::move (free_regions)};
    return free_regions_final;
  }
};

} // namespace Rust

#endif // RUST_BIR_FREE_REGION_H
