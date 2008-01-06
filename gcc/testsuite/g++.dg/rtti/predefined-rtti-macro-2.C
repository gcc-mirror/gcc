// { dg-do compile }
// { dg-options "-fno-rtti" }

struct B { };

void f(B* bp)
{
  bp =
#ifndef __GXX_RTTI
  static_cast<B*>(0);
#endif
}
