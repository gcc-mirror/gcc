// P0624R2
// { dg-do compile { target c++11 } }

#if __cplusplus >= 201402L
#define A auto
#else
#define A int
#endif

void
f1 ()
{
  auto greater = [](A x, A y) { return x > y; };	// { dg-message "a lambda closure type has a deleted (default constructor|copy assignment operator)" "" { target c++17_down } }
  decltype (greater) gt;	// { dg-error "use of deleted function" "" { target c++17_down } }
  gt = greater;			// { dg-error "use of deleted function" "" { target c++17_down } }
}

void
f2 ()
{
  auto greater = [&](A x, A y) { return x > y; };	// { dg-message "a lambda closure type has a deleted (default constructor|copy assignment operator)" }
  decltype (greater) gt;	// { dg-error "use of deleted function" }
  gt = greater;			// { dg-error "use of deleted function" }
}

void
f3 ()
{
  auto greater = [=](A x, A y) { return x > y; };	// { dg-message "a lambda closure type has a deleted (default constructor|copy assignment operator)" }
  decltype (greater) gt;	// { dg-error "use of deleted function" }
  gt = greater;			// { dg-error "use of deleted function" }
}

void
f4 (int i)
{
  auto greater = [i](A x, A y) { return x > y; };	// { dg-message "a lambda closure type has a deleted (default constructor|copy assignment operator)" }
  decltype (greater) gt;	// { dg-error "use of deleted function" }
  gt = greater;			// { dg-error "use of deleted function" }
}

#if __cplusplus > 201703L
void
f5 ()
{
  auto greater = [](auto x, auto y) constexpr { return x > y; };
  decltype (greater) gt;
  static_assert (!gt (1, 2));
  static_assert (gt (4, 3));
  static_assert (!gt (3.5, 3.75));
  static_assert (gt (3.5, 3.25));
  gt = greater;
  static_assert (!gt (1, 2));
  static_assert (gt (4, 3));
  static_assert (!gt (3.5, 3.75));
  static_assert (gt (3.5, 3.25));
}
#endif
