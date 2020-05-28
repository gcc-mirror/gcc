/* { dg-do compile } */
/* { dg-require-effective-target stdint_types } */
/* { dg-additional-options "-march=cooperlake" { target x86_64-*-* i?86-*-* } } */

#include <stdint.h>

int a;
struct b c;
int64_t d;
struct b {
  uint64_t address;
  uint64_t e;
};
void f()
{
  d = (int64_t)(&a)[0] << 56 | (int64_t)((unsigned char *)&a)[1] << 48 |
      (int64_t)((unsigned char *)&a)[2] << 40 |
      (int64_t)((unsigned char *)&a)[3] << 32 |
      (int64_t)((unsigned char *)&a)[4] << 24 | ((unsigned char *)&a)[5] << 16 |
      ((unsigned char *)&a)[6] << 8 | ((unsigned char *)&a)[7];
  c.address = c.e = d;
}
