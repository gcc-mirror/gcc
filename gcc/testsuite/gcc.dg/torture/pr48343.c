/* PR debug/48343 */
/* { dg-do compile } */
/* { dg-options "-fcompare-debug" } */

void foo (unsigned char *, unsigned char *);

void
test (unsigned int x, int y)
{
  unsigned int i, j = 0, k;
  unsigned char s[256], t[64];
  foo (s, t);
  t[0] = y;
  for (i = 0; i < 256; i++)
    {
      j = (j + s[i] + t[i % x]) & 0xff;
      k = i; i = j; j = k;
    }
}
