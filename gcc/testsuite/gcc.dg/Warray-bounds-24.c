/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds" } */

int foo(unsigned order)
{
  int c[3] = {1, 2, 3};
  unsigned i, j;
  for (i = 1; i < order; i++) {
      for (j = 0; j < i / 2; j++) {
	  c[j] += c[i] * c[i-j-1]; /* { dg-bogus "array bounds" } */
	  c[i-j-1] += c[i] * c[j]; /* { dg-bogus "array bounds" } */
      }
  }
  return c[0];
}
