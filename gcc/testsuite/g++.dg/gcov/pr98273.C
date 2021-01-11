/* PR gcov-profile/98273 */

/* { dg-options "--coverage -std=c++11" } */
/* { dg-do run { target native } } */

int
main ()
{
  int i = 42;
  {
    auto f = [] () {
      auto g = [] () {};
      g ();
      g ();
    };
    f ();
  }
  ++i;
  ++i;
  ++i;
  return 45 - i;
}

/* { dg-final { run-gcov-pytest pr98273.C "test-pr98273.py" } } */
