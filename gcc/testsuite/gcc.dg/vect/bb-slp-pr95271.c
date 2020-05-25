/* { dg-do compile } */
/* { dg-additional-options "-march=cooperlake" { target x86_64-*-* i?86-*-* } } */

int a;
struct b c;
long d;
struct b {
  unsigned long address;
  unsigned long e;
};
void f()
{
  d = (long)(&a)[0] << 56 | (long)((unsigned char *)&a)[1] << 48 |
      (long)((unsigned char *)&a)[2] << 40 |
      (long)((unsigned char *)&a)[3] << 32 |
      (long)((unsigned char *)&a)[4] << 24 | ((unsigned char *)&a)[5] << 16 |
      ((unsigned char *)&a)[6] << 8 | ((unsigned char *)&a)[7];
  c.address = c.e = d;
}
