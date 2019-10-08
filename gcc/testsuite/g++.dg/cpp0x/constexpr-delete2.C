// PR c++/69203
// { dg-do compile { target c++11 } }

struct A { ~A(); };
constexpr int f(int i) { return i; }
constexpr int g(A* ap)
{
  return f((delete[] ap, 42)); // { dg-message "" "" { target c++17_down } }
}

A a;
constexpr int i = g(&a);	// { dg-error "" }
				// { dg-message "in 'constexpr' expansion of" "" { target c++2a } .-1 }
