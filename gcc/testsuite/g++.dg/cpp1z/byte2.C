// { dg-do compile { target c++17 } }
// { dg-options "-Wall" }

#include <cstddef>

bool white_space(std::byte x) {
  switch (x) {
  case std::byte{' '}: case std::byte{'\t'}: case std::byte{'\v'}:
  case std::byte{'\f'}: case std::byte{'\n'}:
  return true;
  default:
    return false;
  }
}
