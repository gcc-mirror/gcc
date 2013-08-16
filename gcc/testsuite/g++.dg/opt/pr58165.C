// PR tree-optimization/58165
// { dg-do compile }
// { dg-options "-O2" }

extern "C" float sqrtf (float);

struct A { A (); ~A (); };

void
foo (double d)
{
  A a;
  sqrtf (d);
}
