/* PR target/107131 */
/* { dg-do run } */
/* { dg-options "-Os -fno-ipa-vrp -fno-tree-bit-ccp -Wno-psabi" } */

typedef unsigned char C;
typedef unsigned long long __attribute__((__vector_size__ (32))) U;
typedef unsigned long long __attribute__((__vector_size__ (64))) V;

static __attribute__((__noclone__)) C
foo (C o, U x, U y, U z)
{
  V a = __builtin_shufflevector (x, x, 3, 1, 3, 0, 0, 1, 1, 3);
  V b = (V) { } >= o;
  V c = b <= (V)(b >= (V) { 0, 0, 0, 0, 0, 0x90DF0BE3990AC871ULL });
  U d = __builtin_shufflevector (y, z, 3, 1, 4, 5);
  V e = a + c;
  U f = ((union { V v; U u[2]; }) e).u[1] + d;
  return ((union { U u; C c[32]; }) f).c[9];
}

int
main ()
{
  if (__SIZEOF_LONG_LONG__ != 8 || __CHAR_BIT__ != 8)
    return 0;
  C x = foo (0, (U) { }, (U) { }, (U) { });
  if (x != 0xff)
    __builtin_abort();  
  return 0;
}
