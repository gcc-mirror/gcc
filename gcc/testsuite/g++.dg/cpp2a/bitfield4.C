// PR c++/92732
// { dg-do compile { target c++17 } }
// { dg-options "" }

enum class byte : unsigned char { };
using uint8_t = unsigned char;

struct T
{
  byte a : 2 = byte{0};	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  uint8_t b : 2 = 0;	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
} t;
