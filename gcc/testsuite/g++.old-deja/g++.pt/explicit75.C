// { dg-do assemble  }
// Test for not complaining about mismatches during unification.

template <void (*F)(int)> void f();
template <void (*F)(double)> void f();
extern void g(double);

void h ()
{
  f<g>();
}
