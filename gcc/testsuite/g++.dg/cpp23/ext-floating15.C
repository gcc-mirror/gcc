// PR c++/109278
// { dg-do compile { target float128 } }
// { dg-options "-w" }

void foo (long double);	// { dg-bogus "initializing argument 1 of" }

void
bar (_Float128 x)
{
  foo (x);
}
