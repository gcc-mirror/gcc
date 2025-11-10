// Check for some additional lang_type flags that we'd missed.
// { dg-additional-options "-fmodule-header -fabi-version=21 -Wabi=15" }
// { dg-module-cmi {} }
// { dg-skip-if "test assumes that structs have padding" { default_packed } }

struct pr106381 {
  long l;
  char c = -1;
};
struct L1 : pr106381 {
  char x;  // { dg-warning "offset" "" { target c++14 } }
};
static_assert(sizeof(L1) == sizeof(pr106381), "");


struct pr120012 {
  pr120012(const pr120012&) = default;
  pr120012(pr120012&&) = default;
  pr120012& operator=(pr120012&&) = default;
  unsigned int a;
  unsigned char b;
};
struct L2 : pr120012 {
  unsigned char y;  // { dg-warning "offset" "" { target c++20 } }
};
static_assert(sizeof(L2) > sizeof(pr120012), "");
