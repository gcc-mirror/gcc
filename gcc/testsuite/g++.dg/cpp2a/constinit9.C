// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do run { target c++20 } }
// A run-time test.

constexpr int foo (int x) { return x; }
constinit int b = foo(42);

int
main ()
{
  if (b != 42)
    __builtin_abort ();
  // We can still modify 'b'.
  b = 10;
  if (b != 10)
    __builtin_abort ();

  constinit static int s = foo(14);
  if (s != 14)
    __builtin_abort ();
  s++;
  if (s != 15)
    __builtin_abort ();
}
