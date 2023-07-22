// PR c++/69203
// { dg-do compile { target c++11 } }

struct A { ~A(); };
constexpr int f(int i) { return i; }
constexpr int g(A* ap)
{
  return f((delete[] ap, 42)); // { dg-message "" "" { target c++17_down } }
                               // { dg-error "" "" { target c++2a } .-1 }
}

A a;
constexpr int i = g(&a);  // { dg-error "" "" { target c++17_down } }
			  // { dg-message "in 'constexpr' expansion of" "" { target c++2a } .-1 }
