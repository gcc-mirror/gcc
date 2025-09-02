// Check for some additional lang_type flags that we'd missed.
// { dg-additional-options "-fmodule-header -fabi-version=21 -Wabi=15" }
// { dg-module-cmi {} }
// { dg-skip-if "test assumes that structs have padding" { default_packed } }

#if __cpp_trivial_relocatability < 202502L
#define trivially_relocatable_if_eligible __trivially_relocatable_if_eligible
#define replaceable_if_eligible __replaceable_if_eligible
#endif

struct A trivially_relocatable_if_eligible { A(A&&); };
struct B replaceable_if_eligible { B(B&&); B& operator=(B&&); };
struct C {};
static_assert(__builtin_is_trivially_relocatable(C) && __builtin_is_replaceable(C), "");


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
