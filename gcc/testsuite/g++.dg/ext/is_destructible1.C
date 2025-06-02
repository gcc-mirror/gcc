// PR c++/107600
// { dg-do compile { target c++11 } }

#define SA(X) static_assert ((X), #X)

namespace N1 {
  struct A { ~A() = delete; };

  SA (!__is_destructible (A));
  SA (!__is_nothrow_destructible (A));
  SA (!__is_trivially_destructible (A));

  bool b = __has_trivial_destructor (A); // { dg-message "has_trivial_destructor" }
}

namespace N2 {
  struct A { protected: ~A() = default; };

  SA (!__is_destructible (A));
  SA (!__is_nothrow_destructible (A));
  SA (!__is_trivially_destructible (A));
  SA (__has_trivial_destructor (A));
}

namespace N3 {
  struct A { ~A(); };

  SA (__is_destructible (A));
  SA (__is_nothrow_destructible (A));
  SA (!__is_trivially_destructible (A));
  SA (!__has_trivial_destructor (A));
}

namespace N4 {
  struct A { ~A() noexcept (false); };

  SA (__is_destructible (A));
  SA (!__is_nothrow_destructible (A));
  SA (!__is_trivially_destructible (A));
  SA (!__has_trivial_destructor (A));
}

namespace N5 {
  struct A { ~A() = default; };

  SA (__is_destructible (A));
  SA (__is_nothrow_destructible (A));
  SA (__is_trivially_destructible (A));
  SA (__has_trivial_destructor (A));
}

namespace N6 {
  struct A { };

  SA (__is_destructible (A));
  SA (__is_nothrow_destructible (A));
  SA (__is_trivially_destructible (A));
  SA (__has_trivial_destructor (A));
}

