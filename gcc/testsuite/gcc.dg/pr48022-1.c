/* PR tree-optimization/48022 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-overflow" } */

int
foo (const char *x)
{
  unsigned long l = 1;
  const unsigned char *s = (const unsigned char *) (const char *) (x);
  int r = s[0] - ((const unsigned char *) (const char *) ("/"))[0];
  if (l > 0 && r == 0)
    r = (s[1] - ((const unsigned char *) (const char *) ("/"))[1]);
  return r;
}
