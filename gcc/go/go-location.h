// go-location.h -- GCC specific Location declaration.   -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_LOCATION_H
#define GO_LOCATION_H

#include "go-system.h"

// A location in an input source file.

class Location
{
 public:
  Location()
    : gcc_loc_(UNKNOWN_LOCATION)
  { }

  explicit Location(location_t loc)
    : gcc_loc_(loc)
  { }

  location_t
  gcc_location() const
  { return this->gcc_loc_; }

 private:
  location_t gcc_loc_;
};

// The Go frontend requires the ability to compare Locations.

inline bool
operator<(Location loca, Location locb)
{
  return loca.gcc_location() < locb.gcc_location();
}

inline bool
operator==(Location loca, Location locb)
{
  return loca.gcc_location() == locb.gcc_location();
}

#endif // !defined(GO_LOCATION_H)
