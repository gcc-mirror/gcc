/* PR middle-end/38140 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing" } */

int foo (void *x)
{
  int (*fn) (int);
  *(void **)&fn = x;
  return fn (6);
}
