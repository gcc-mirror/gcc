/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

void  (*b) ();

void fn1 (const int *p1)
{
  static void *a = &&conv_1234_123C;
 conv_1234_123C:
  ;
}

void fn2 ()
{
  b = fn1;
}
