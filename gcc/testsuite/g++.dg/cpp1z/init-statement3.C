// { dg-do run }
// { dg-options -std=c++17 }
// Test C++17 selection statements with initializer, side-effects.

int
main ()
{
  int g = 0;

  if (g++; g > 1)
    __builtin_abort ();
  if (++g; g > 2)
    __builtin_abort ();
  if (g != 2)
    __builtin_abort ();
}
