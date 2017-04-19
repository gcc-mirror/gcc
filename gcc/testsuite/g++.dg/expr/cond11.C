struct A;
struct C
{
  operator A();
};

struct A
{
  A(C);
};

extern A a;
extern C c;

void
foo (bool b)
{
  b ? c : a;			// { dg-error "?:" }
  // { dg-message "ambiguous" "" { target *-*-* } .-1 }
}
