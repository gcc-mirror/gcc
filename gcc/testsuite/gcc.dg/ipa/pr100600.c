/* PR ipa/100600 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int a, b, c;
long d(long x, long e, long f, long g) {
  long h, i;
  for (; h < e; h++) {
    i = f;
    for (; i < g; i++)
      c = b + a;
  }
  return h + i;
}

long j(long x, long e, long y, long g) {
  long h, i;
  for (; h < e; h++)
    for (; i < g; i++)
      c = b + a;
  return h + i;
}
