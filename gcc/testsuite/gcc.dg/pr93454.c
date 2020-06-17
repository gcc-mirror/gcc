/* PR tree-optimization/93454 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

#if __SIZEOF_INT__ == 4 && __CHAR_BIT__ == 8
#define A(n) n, n + 0x01010101, n + 0x02020202, n + 0x03030303
#define B(n) A (n), A (n + 0x04040404), A (n + 0x08080808), A (n + 0x0c0c0c0c)
#define C(n) B (n), B (n + 0x10101010), B (n + 0x20202020), B (n + 0x30303030)
#define D(n) C (n), C (n + 0x40404040), C (n + 0x80808080U), C (n + 0xc0c0c0c0U)
const unsigned int a[64] = { C (0) };
const unsigned int b[256] = { D (0) };
const unsigned int c[32] = { B (0), B (0x10101010) };
const unsigned int d[16] = { B (0) };
const unsigned int e[8] = { A (0), A (0x04040404) };

void
foo (void)
{
  const unsigned char *s = ((const unsigned char *) a) + 1;
  const unsigned char *t = ((const unsigned char *) b) + 1;
  const unsigned char *u = ((const unsigned char *) c) + 1;
  const unsigned char *v = ((const unsigned char *) d) + 1;
  const unsigned char *w = ((const unsigned char *) e) + 1;
}
#endif
