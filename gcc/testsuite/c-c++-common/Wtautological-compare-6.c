/* PR c/82437 */
/* { dg-do compile { target int32plus } } */
/* { dg-options "-Wtautological-compare" } */

int
foo (unsigned int x)
{
  if ((x & -1879048192) != -1879048192)	/* { dg-bogus "bitwise comparison always evaluates to" } */
    return 0;
  return 1;
}
