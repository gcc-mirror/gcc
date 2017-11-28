/* PR c/82437 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-Wtautological-compare" } */

int
foo (unsigned long long int x)
{
  if ((x | 0x190000000ULL) != -1879048192)	/* { dg-bogus "bitwise comparison always evaluates to" } */
    return 0;
  return 1;
}
