/* PR target/65523 */
/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O -fcheck-pointer-bounds -mmpx" } */

int *fn1()
{
  int *r = fn1();
  if (r == (void *)0)
    return r;
}
