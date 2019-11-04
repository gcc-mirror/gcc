// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++2a } }

template <typename T> void foo(T);

template <typename F, typename T, typename = decltype(foo<T>(F()))>
void test(int) { }

// No other overload, so if the above fails because of the conversion,
// we fail.

void
fn ()
{
  test<int(*)[2], int(*)[]>(0);
  test<int(*)[], int(*)[]>(0);
}
