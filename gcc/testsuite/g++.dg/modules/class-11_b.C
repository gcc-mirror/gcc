// { dg-additional-options "-fmodules -fabi-version=21 -Wabi=15" }
// { dg-skip-if "test assumes that structs have padding" { default_packed } }

import "class-11_a.H";

static_assert(__builtin_is_trivially_relocatable(A), "");
static_assert(__builtin_is_replaceable(B), "");
static_assert(__builtin_is_trivially_relocatable(C) && __builtin_is_replaceable(C), "");

struct M1 : pr106381 {
  char x;  // { dg-warning "offset" "" { target c++14 } }
};

struct M2 : pr120012 {
  unsigned char y;  // { dg-warning "offset" "" { target c++20 } }
};
