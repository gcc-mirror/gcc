/* PR rtl-optimization/113048 */
/* { dg-do compile } */
/* { dg-options "-O -march=cascadelake -fwrapv" } */

signed char a, b, c;
int d;
const char *e, *q;
short f;
int g;

void
foo (int x, long long y, long long z)
{
  unsigned char h = x;
  int i = __builtin_strncmp (q, e, 2);
  h /= g;
  unsigned long long j = (~z & (0xfb5856dd8a4d4702ULL & f) / 0) * h;	/* { dg-warning "division by zero" } */
  b += __builtin_add_overflow_p (d, c, 0);
  signed char k = y;
  long l = -k & sizeof (0);
  long long m = y + j + z + h + 3 + l;
  int n = m + i;
  short o = n + f;
  signed char p = o + h + k;
  a = p;
}
