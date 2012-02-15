/* { dg-do compile } */
/* { dg-options "-O2 -floop-flatten -floop-strip-mine" } */

void f (unsigned *s)
{
  int n;
  for (n = 0; n < 256; n++)
    s[n] = 0;
}
