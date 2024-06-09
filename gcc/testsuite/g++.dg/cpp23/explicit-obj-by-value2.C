// P0847R7
// { dg-do run { target c++23 } }

// conversion of the implicit object argument to an xobj parameter
// using a user defined conversion or converting constructor
// when calling by value xobj member functions

// see explicit-obj-by-value1.C for details on this test

using uintptr_t = __UINTPTR_TYPE__;
inline constexpr uintptr_t magic = 42;

struct S;

struct FromS {
  uintptr_t _v;
  FromS(S);
};

struct S {
  operator uintptr_t() const {
    return magic;
  }
  uintptr_t f(this uintptr_t n) {
    return n;
  }
  uintptr_t g(this FromS from_s) {
    return from_s._v;
  }
};

FromS::FromS(S) : _v(magic) {}


int main() 
{
  S s0{};
  S s1{};
  // prevent (absurdly improbable) bogus failures
  S& s = magic != (uintptr_t)(&s0) ? s0 : s1;

  uintptr_t const ret0 = s.f();
  // check for reinterpretation of the object argument
  if (ret0 == (uintptr_t)(&s))
    __builtin_abort ();
  // check for a bugged conversion
  if (ret0 != magic)
    __builtin_abort ();

  uintptr_t const ret1 = s.g();
  // check for reinterpretation of the object argument
  if (ret1 == (uintptr_t)(&s))
    __builtin_abort ();
  // check for a bugged conversion
  if (ret1 != magic)
    __builtin_abort ();
}

