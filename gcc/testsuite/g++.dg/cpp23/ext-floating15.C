// PR c++/109278
// { dg-do compile { target float128 } }
// { dg-options "-w" }
// { dg-add-options float128 }

void foo (long double);	// { dg-bogus "initializing argument 1 of" }

void
bar (_Float128 x)
{
  foo (x);
}
