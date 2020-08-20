#ifndef RUST_CODEPOINT_H
#define RUST_CODEPOINT_H

#include <string>

namespace Rust {
struct Codepoint
{
  uint32_t value;

  // Creates a zero codepoint.
  Codepoint () : value (0) {}

  // Creates a codepoint from an encoded UTF-8 value.
  Codepoint (uint32_t value) : value (value) {}

  // Returns a C++ string containing string value of codepoint.
  std::string as_string ();

  bool operator== (Codepoint other) const
  {
    return value == other.value;
  }

  bool operator!= (Codepoint other) const
  {
    return !operator== (other);
  }
};
} // namespace Rust

#endif
