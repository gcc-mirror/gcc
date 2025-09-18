// { dg-do compile { target c++26 } }

#include <new>

union U { double d; int i; };

constexpr int f()
{
  U u;
  new (&u.i) int;
  return u.d;			// { dg-error "active" }
}

int main ()
{
  constexpr int i = f();	// { dg-message "" }
}
