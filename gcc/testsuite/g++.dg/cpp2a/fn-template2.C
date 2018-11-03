// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct A { };
bool operator <(void (*fp)(), A) { return false; }
void f() {}

int
main ()
{
  A a;
  f < a; // { dg-error "invalid" }
  bool b = f < a; // { dg-error "invalid" }
  (f) < a;
}
