/* { dg-do compile } */
/* { dg-options "-O -Wmaybe-uninitialized" } */

int foo (int x)
{
  int y;
  if (x)
    return *(&y + 1); /* { dg-bogus "may be used uninitialized" } */
  return 0;
}
