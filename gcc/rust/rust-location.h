// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

// rust-location.h -- GCC specific Location declaration.   -*- C++ -*-

#ifndef RUST_LOCATION_H
#define RUST_LOCATION_H

#include "rust-system.h"

// A location in an input source file.

class Location
{
public:
  Location () : gcc_loc_ (UNKNOWN_LOCATION) {}

  explicit Location (location_t loc) : gcc_loc_ (loc) {}

  location_t gcc_location () const { return gcc_loc_; }

  Location operator+= (location_t rhs)
  {
    gcc_loc_ += rhs;
    return *this;
  }

  Location operator-= (location_t rhs)
  {
    gcc_loc_ -= rhs;
    return *this;
  }

  bool operator== (location_t rhs) { return rhs == gcc_loc_; }

private:
  location_t gcc_loc_;
};

// The Rust frontend requires the ability to compare Locations.

inline bool
operator< (Location loca, Location locb)
{
  return loca.gcc_location () < locb.gcc_location ();
}

inline bool
operator== (Location loca, Location locb)
{
  return loca.gcc_location () == locb.gcc_location ();
}

inline Location
operator+ (Location lhs, location_t rhs)
{
  lhs += rhs;
  return lhs;
}

inline Location
operator- (Location lhs, location_t rhs)
{
  lhs -= rhs;
  return lhs;
}

class RichLocation
{
public:
  RichLocation (Location root);
  ~RichLocation ();

  void add_range (Location loc);

  void add_fixit_insert_before (const std::string &new_parent);

  void add_fixit_insert_before (Location where, const std::string &new_parent);

  void add_fixit_insert_after (const std::string &new_parent);

  void add_fixit_insert_after (Location where, const std::string &new_parent);

  const rich_location &get () const { return gcc_rich_loc; }

private:
  rich_location gcc_rich_loc;
};

#endif // !defined(RUST_LOCATION_H)
