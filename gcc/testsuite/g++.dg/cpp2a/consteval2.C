// { dg-do run }
// { dg-options "-std=c++2a" }

extern "C" void abort ();
consteval int foo () { return 42; }
consteval auto bar () { return foo; }
consteval int baz (int (*fn) () = bar ()) { return fn (); }
constexpr int a = baz ();
static_assert (a == 42);
int b = baz ();

int
main ()
{
  if (b != 42)
    abort ();
}
