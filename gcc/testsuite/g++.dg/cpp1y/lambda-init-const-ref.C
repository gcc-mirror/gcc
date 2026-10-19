// PR c++/87638
// PR c++/90789
// PR c++/115471
// { dg-do compile { target c++14 } }

struct S { int x; };
constexpr const S make_const_s () { return S{7}; }
constexpr const int&& make_const_xval (const int& x) { return static_cast<const int&&>(x); }

template <typename T>
T
test_dependent (const T a)
{
  auto l = [&r = static_cast<const T&&>(a)] { return r; };
  return l ();
}

void
test_valid ()
{
  const int ca = 5;
  // Const xvalue via static_cast or function call (PR c++/115471)
  auto l1 = [&r = static_cast<const int&&>(ca)] { return r; };
  auto l2 = [&r = make_const_xval (ca)] { return r; };

  // Const class prvalue (PR c++/87638, PR c++/90789)
  auto l3 = [&r = make_const_s ()] { return r.x; };

  test_dependent (42);
}

void
test_invalid ()
{
  int a = 5;
  const volatile int cva = 5;

  auto b1 = [&r = static_cast<int&&>(a)] {};                // { dg-error "cannot bind" }
  auto b2 = [&r = 5] {};                                    // { dg-error "cannot bind" }
  auto b3 = [&r = static_cast<const volatile int&&>(cva)] {}; // { dg-error "cannot bind" }
}
