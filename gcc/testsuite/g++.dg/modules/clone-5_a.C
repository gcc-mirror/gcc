// Test that a random instantiation of a constructor template doesn't end up in
// the overload set for other arguments.

// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules" }

export module M;

export {
  inline int i;

  template <class T>
    struct A {
    A(const T* p, unsigned long len) { ++i; }
    template <class B, class E>
    requires (!__is_convertible(E,unsigned long))
    A(B,E) { ++i; }
  };

  inline void f()
  {
    const char *const p = nullptr;
    A<char> a (p, p);		// instantiate A<const char *, const char *>
  }
}
