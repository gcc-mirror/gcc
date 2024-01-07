// P0847R7
// { dg-do run { target c++23 } }

// correct constructor selection when initializing a by value xobj parameter

// see explicit-obj-by-value1.C for details on this test

using uintptr_t = __UINTPTR_TYPE__;
inline constexpr uintptr_t magic = 42;
inline constexpr uintptr_t copy_magic = 5;
inline constexpr uintptr_t move_magic = 10;

struct S {
  uintptr_t _v;
  explicit S(uintptr_t v) : _v(v) {}
  S(S const& other) : _v(other._v + copy_magic) {}
  S(S&& other) : _v(other._v + move_magic) {}
  uintptr_t f(this S self) {
    return self._v;
  }
};

int main() 
{
  S s0{magic};
  S s1{magic};
  // prevent (absurdly improbable (^2)) bogus results
  // it's virtually impossible for both to have a bogus result,
  // but we can guarantee correct results from both easily, so why not?
  S& s_copy_from = magic + copy_magic != (uintptr_t)(&s0) ? s0 : s1;
  S& s_move_from = magic + move_magic != (uintptr_t)(&s0) ? s0 : s1;
  uintptr_t const copy_ret = static_cast<S const&>(s_copy_from).f();
  uintptr_t const move_ret = static_cast<S&&>(s_move_from).f();
  // we test specifically for reinterpretation in other
  // by value tests, it's unnecessary to do it again here
  if (copy_ret != magic + copy_magic)
    __builtin_abort ();
  if (move_ret != magic + move_magic)
    __builtin_abort ();
}

