/* PR rtl-optimization/96298 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-forwprop" } */

typedef unsigned char __attribute__ ((__vector_size__ (8))) v64u8;

v64u8 a;

int
main (void)
{
  v64u8 x = (a - 1) ^ -a;
  for (unsigned i = 0; i < sizeof (x); i++)
    if (x[i] != 0xff)
      __builtin_abort ();
  return 0;
}

