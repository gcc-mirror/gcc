// PR c++/85256
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wno-vla }

void foo(int i)
{
  int (*x)[i];
  [=]{ [=]{ 0 ? x : x; }; };	// { dg-bogus "sorry" "" { xfail *-*-* } }

}
