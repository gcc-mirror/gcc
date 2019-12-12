// P0846R0
// { dg-do compile }

struct A { };
bool operator <(void (*fp)(), A) { return false; }
void f() {}

int
main ()
{
  A a;
  f < a; // { dg-error "invalid" "" { target c++2a } }
  bool b = f < a; // { dg-error "invalid" "" { target c++2a } }
  (f) < a;
}
