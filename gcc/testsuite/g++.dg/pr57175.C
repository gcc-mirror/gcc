/* { dg-do compile { target c++11 } } */

extern "C" void do_not_remove ();

struct A
{
  A () { }
  A (A const&) { do_not_remove (); }
};

A
f ()
{
  alignas (2 * alignof (A)) A x;
  return x;
}

/* { dg-final { scan-assembler "do_not_remove" } } */
