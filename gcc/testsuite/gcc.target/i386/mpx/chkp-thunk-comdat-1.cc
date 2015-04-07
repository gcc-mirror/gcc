/* { dg-do compile } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx" } */

namespace
{
  template <int dim>
  int __attribute__((noinline))
  f1 ()
  {
    return dim;
  }
}

int
test ()
{
  return f1<3> ();
}
