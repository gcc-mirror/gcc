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

struct FreeRegion
{
  size_t value;
  // some overloads for comparision
  bool operator== (const FreeRegion &rhs) const { return value == rhs.value; }
  bool operator!= (const FreeRegion &rhs) const { return !(operator== (rhs)); }
  bool operator< (const FreeRegion &rhs) const { return value < rhs.value; }
  bool operator> (const FreeRegion &rhs) const { return value > rhs.value; }
  bool operator<= (const FreeRegion &rhs) const { return !(operator> (rhs)); }
  bool operator>= (const FreeRegion &rhs) const { return !(operator< (rhs)); }
};

static constexpr FreeRegion STATIC_FREE_REGION = {0};

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

  WARN_UNUSED_RESULT FreeRegions prepend (FreeRegion region) const
  {
    std::vector<FreeRegion> new_regions = {region};
    new_regions.insert (new_regions.end (), regions.begin (), regions.end ());
    return FreeRegions (std::move (new_regions));
  }

  void push_back (FreeRegion region) { regions.push_back (region); }

  FreeRegions () {}
  FreeRegions (std::vector<FreeRegion> &&regions) : regions (regions) {}

  WARN_UNUSED_RESULT std::string to_string () const
  {
    std::stringstream result;
    for (auto &region : regions)
      {
	result << region.value;
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
    ++next_free_region.value;
    return {next_free_region.value - 1};
  }

  FreeRegions bind_regions (std::vector<TyTy::Region> regions,
			    FreeRegions parent_free_regions)
  {
    FreeRegions free_regions;
    for (auto &region : regions)
      {
	if (region.is_early_bound ())
	  free_regions.push_back (parent_free_regions[region.get_index ()]);
	else if (region.is_static ())
	  free_regions.push_back (STATIC_FREE_REGION);
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
    return free_regions;
  }
};

} // namespace Rust

#endif // RUST_BIR_FREE_REGION_H
