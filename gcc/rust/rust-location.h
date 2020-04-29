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

#endif // !defined(RUST_LOCATION_H)
