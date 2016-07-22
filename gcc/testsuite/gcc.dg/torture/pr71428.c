/* { dg-do run } */
/* { dg-additional-options "-fno-tree-forwprop -Wno-psabi -w" } */

typedef unsigned short v64u16 __attribute__ ((vector_size (64)));

v64u16
foo (v64u16 p1)
{
  p1[31] |= p1[1];
  return p1;
}

int
main ()
{
  v64u16 x = foo ((v64u16){ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 });
  if (x[31] != 1)
    __builtin_abort();
  return 0;
}
