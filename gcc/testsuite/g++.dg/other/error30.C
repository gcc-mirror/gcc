// PR c++/35336
// { dg-do compile }
// { dg-bogus "not supported by" "" { target *-*-* } 0 }

struct A
{
  int i : 2;
};

void foo (bool b)
{
  A a;
  (a.i || b) ();	// { dg-error "cannot be used as" }
}
