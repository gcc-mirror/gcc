/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

char *y;
void foo (int sysnum)
{
  static char *x[] = {};
  int nsyscalls = sizeof x / sizeof x[0];
  if (sysnum < 0 || sysnum >= nsyscalls)
    return;
  else
    y = x[sysnum]; /* { dg-bogus "above array bounds" } */
}
