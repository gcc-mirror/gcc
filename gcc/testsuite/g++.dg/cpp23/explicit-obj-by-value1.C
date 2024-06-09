// P0847R7
// { dg-do run { target c++23 } }

// conversion of the implicit object argument to an xobj parameter
// when calling by value xobj member functions

// The initial implementation of xobj member functions incorrectly did not
// convert the implicit object argument when binding to the xobj
// parameter. In spite of this, it did correctly check to see if such a
// conversion would be valid, thus no diagnostic would be emitted when a
// conversion was valid, but instead of applying the conversion, the
// argument would silently be reinterpreted as the type of the parameter. 

// This is why we use uintptr_t for the value in S and compare the result
// of f to &s, we want to test for simple reinterpretation of the
// argument. To accurately test for this we make sure to use an object
// that has a different address than the value of our magic number. It's
// an impossibly improbable edge case but it's trivial to work around. We
// still compare against both the address of s and the magic number so we
// can additionally test for bugged conversions, while also
// differentiating that case from reinterpretation of the argument.

using uintptr_t = __UINTPTR_TYPE__;
inline constexpr uintptr_t magic = 42;

struct S {
    uintptr_t _v;
    uintptr_t f(this S self) {
        return self._v;
    }
};

int main() 
{
  S s0{magic};
  S s1{magic};
  // prevent (absurdly improbable) bogus failures
  S& s = magic != (uintptr_t)(&s0) ? s0 : s1;

  uintptr_t const ret = s.f();
  // check for reinterpretation of the object argument
  if (ret == (uintptr_t)(&s))
    __builtin_abort ();
  // check for a bugged conversion
  if (ret != magic)
    __builtin_abort ();
}

