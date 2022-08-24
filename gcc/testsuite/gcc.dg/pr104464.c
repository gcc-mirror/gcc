/* { dg-do compile } */
/* { dg-options "-O -fnon-call-exceptions -fno-tree-dce -fno-tree-forwprop -fsignaling-nans" } */

typedef double __attribute__((__vector_size__(16))) F;
F f;

void
foo(void)
{
  f += (F)(f != (F){}[0]);
}
