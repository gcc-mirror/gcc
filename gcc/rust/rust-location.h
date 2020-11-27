// rust-location.h -- GCC specific Location declaration.   -*- C++ -*-

#ifndef RUST_LOCATION_H
#define RUST_LOCATION_H

#include "rust-system.h"

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

// The Rust frontend requires the ability to compare Locations.

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

#endif // !defined(RUST_LOCATION_H)
