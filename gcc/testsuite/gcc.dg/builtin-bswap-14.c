/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);


__attribute__ ((noinline, noclone))
static __INT32_TYPE__ rt32 (__INT32_TYPE__ x, int y, __INT32_TYPE__ z) {
  return (__builtin_bswap32(x) >> y) & z;
}
#define TEST32(X,Y,Z) if(((__builtin_bswap32(X)>>Y)&Z)!=rt32(X,Y,Z)) abort()
void test32(__INT32_TYPE__ x)
{
  TEST32(x,0,1);
  TEST32(x,0,255);
  TEST32(x,1,1);
  TEST32(x,2,1);
  TEST32(x,3,1);
  TEST32(x,4,1);
  TEST32(x,5,1);
  TEST32(x,6,1);
  TEST32(x,7,1);
  TEST32(x,8,1);
  TEST32(x,8,255);
  TEST32(x,9,1);
  TEST32(x,10,1);
  TEST32(x,11,1);
  TEST32(x,12,1);
  TEST32(x,13,1);
  TEST32(x,14,1);
  TEST32(x,15,1);
  TEST32(x,16,1);
  TEST32(x,16,255);
  TEST32(x,17,1);
  TEST32(x,18,1);
  TEST32(x,19,1);
  TEST32(x,20,1);
  TEST32(x,21,1);
  TEST32(x,22,1);
  TEST32(x,23,1);
  TEST32(x,24,1);
  TEST32(x,24,255);
  TEST32(x,25,1);
  TEST32(x,26,1);
  TEST32(x,27,1);
  TEST32(x,28,1);
  TEST32(x,29,1);
  TEST32(x,30,1);
  TEST32(x,31,1);
}

#if __SIZEOF_LONG_LONG__ == 8
__attribute__ ((noinline, noclone))
static long long rt64 (long long x, int y, long long z) {
  return (__builtin_bswap64(x) >> y) & z;
}
#define TEST64(X,Y,Z) if(((__builtin_bswap64(X)>>Y)&Z)!=rt64(X,Y,Z)) abort()
void test64(long long x)
{
  TEST64(x,0,1);
  TEST64(x,0,255);
  TEST64(x,1,1);
  TEST64(x,2,1);
  TEST64(x,3,1);
  TEST64(x,4,1);
  TEST64(x,5,1);
  TEST64(x,6,1);
  TEST64(x,7,1);
  TEST64(x,8,1);
  TEST64(x,8,255);
  TEST64(x,9,1);
  TEST64(x,10,1);
  TEST64(x,11,1);
  TEST64(x,12,1);
  TEST64(x,13,1);
  TEST64(x,14,1);
  TEST64(x,15,1);
  TEST64(x,16,1);
  TEST64(x,16,255);
  TEST64(x,17,1);
  TEST64(x,18,1);
  TEST64(x,19,1);
  TEST64(x,20,1);
  TEST64(x,21,1);
  TEST64(x,22,1);
  TEST64(x,23,1);
  TEST64(x,24,1);
  TEST64(x,24,255);
  TEST64(x,25,1);
  TEST64(x,26,1);
  TEST64(x,27,1);
  TEST64(x,28,1);
  TEST64(x,29,1);
  TEST64(x,30,1);
  TEST64(x,31,1);
  TEST64(x,32,1);
  TEST64(x,32,255);
  TEST64(x,33,1);
  TEST64(x,34,1);
  TEST64(x,35,1);
  TEST64(x,36,1);
  TEST64(x,37,1);
  TEST64(x,38,1);
  TEST64(x,39,1);
  TEST64(x,40,1);
  TEST64(x,40,255);
  TEST64(x,41,1);
  TEST64(x,42,1);
  TEST64(x,43,1);
  TEST64(x,44,1);
  TEST64(x,45,1);
  TEST64(x,46,1);
  TEST64(x,47,1);
  TEST64(x,48,1);
  TEST64(x,48,255);
  TEST64(x,49,1);
  TEST64(x,50,1);
  TEST64(x,51,1);
  TEST64(x,52,1);
  TEST64(x,53,1);
  TEST64(x,54,1);
  TEST64(x,55,1);
  TEST64(x,56,1);
  TEST64(x,56,255);
  TEST64(x,57,1);
  TEST64(x,58,1);
  TEST64(x,59,1);
  TEST64(x,60,1);
  TEST64(x,61,1);
  TEST64(x,62,1);
  TEST64(x,63,1);
}
#endif

__attribute__ ((noinline, noclone))
static int rt16 (int x, int y, int z) {
  return (__builtin_bswap16(x) >> y) & z;
}
#define TEST16(X,Y,Z) if(((__builtin_bswap16(X)>>Y)&Z)!=rt16(X,Y,Z)) abort()
void test16(int x)
{
  TEST16(x,0,1);
  TEST16(x,0,255);
  TEST16(x,1,1);
  TEST16(x,2,1);
  TEST16(x,3,1);
  TEST16(x,4,1);
  TEST16(x,5,1);
  TEST16(x,6,1);
  TEST16(x,7,1);
  TEST16(x,8,1);
  TEST16(x,8,255);
  TEST16(x,9,1);
  TEST16(x,10,1);
  TEST16(x,11,1);
  TEST16(x,12,1);
  TEST16(x,13,1);
  TEST16(x,14,1);
  TEST16(x,15,1);
}

int main()
{
  test32(0x00000000);
  test32(0xffffffff);
  test32(0x00000001);
  test32(0x00000002);
  test32(0x00000004);
  test32(0x00000008);
  test32(0x00000010);
  test32(0x00000020);
  test32(0x00000040);
  test32(0x00000080);
  test32(0x00000100);
  test32(0x00000200);
  test32(0x00000400);
  test32(0x00000800);
  test32(0x00001000);
  test32(0x00002000);
  test32(0x00004000);
  test32(0x00008000);
  test32(0x00010000);
  test32(0x00020000);
  test32(0x00040000);
  test32(0x00080000);
  test32(0x00100000);
  test32(0x00200000);
  test32(0x00400000);
  test32(0x00800000);
  test32(0x01000000);
  test32(0x02000000);
  test32(0x04000000);
  test32(0x08000000);
  test32(0x10000000);
  test32(0x20000000);
  test32(0x40000000);
  test32(0x80000000);
  test32(0x12345678);
  test32(0x87654321);
  test32(0xdeadbeef);
  test32(0xcafebabe);

#if __SIZEOF_LONG_LONG__ == 8
  test64(0x0000000000000000ll);
  test64(0xffffffffffffffffll);
  test64(0x0000000000000001ll);
  test64(0x0000000000000002ll);
  test64(0x0000000000000004ll);
  test64(0x0000000000000008ll);
  test64(0x0000000000000010ll);
  test64(0x0000000000000020ll);
  test64(0x0000000000000040ll);
  test64(0x0000000000000080ll);
  test64(0x0000000000000100ll);
  test64(0x0000000000000200ll);
  test64(0x0000000000000400ll);
  test64(0x0000000000000800ll);
  test64(0x0000000000001000ll);
  test64(0x0000000000002000ll);
  test64(0x0000000000004000ll);
  test64(0x0000000000008000ll);
  test64(0x0000000000010000ll);
  test64(0x0000000000020000ll);
  test64(0x0000000000040000ll);
  test64(0x0000000000080000ll);
  test64(0x0000000000100000ll);
  test64(0x0000000000200000ll);
  test64(0x0000000000400000ll);
  test64(0x0000000000800000ll);
  test64(0x0000000001000000ll);
  test64(0x0000000002000000ll);
  test64(0x0000000004000000ll);
  test64(0x0000000008000000ll);
  test64(0x0000000010000000ll);
  test64(0x0000000020000000ll);
  test64(0x0000000040000000ll);
  test64(0x0000000080000000ll);
  test64(0x0000000100000000ll);
  test64(0x0000000200000000ll);
  test64(0x0000000400000000ll);
  test64(0x0000000800000000ll);
  test64(0x0000001000000000ll);
  test64(0x0000002000000000ll);
  test64(0x0000004000000000ll);
  test64(0x0000008000000000ll);
  test64(0x0000010000000000ll);
  test64(0x0000020000000000ll);
  test64(0x0000040000000000ll);
  test64(0x0000080000000000ll);
  test64(0x0000100000000000ll);
  test64(0x0000200000000000ll);
  test64(0x0000400000000000ll);
  test64(0x0000800000000000ll);
  test64(0x0001000000000000ll);
  test64(0x0002000000000000ll);
  test64(0x0004000000000000ll);
  test64(0x0008000000000000ll);
  test64(0x0010000000000000ll);
  test64(0x0020000000000000ll);
  test64(0x0040000000000000ll);
  test64(0x0080000000000000ll);
  test64(0x0100000000000000ll);
  test64(0x0200000000000000ll);
  test64(0x0400000000000000ll);
  test64(0x0800000000000000ll);
  test64(0x1000000000000000ll);
  test64(0x2000000000000000ll);
  test64(0x4000000000000000ll);
  test64(0x8000000000000000ll);
  test64(0x0123456789abcdefll);
  test64(0xfedcba9876543210ll);
  test64(0xdeadbeefdeadbeefll);
  test64(0xcafebabecafebabell);
#endif

  test16(0x0000);
  test16(0xffff);
  test16(0x0001);
  test16(0x0002);
  test16(0x0004);
  test16(0x0008);
  test16(0x0010);
  test16(0x0020);
  test16(0x0040);
  test16(0x0080);
  test16(0x0100);
  test16(0x0200);
  test16(0x0400);
  test16(0x0800);
  test16(0x1000);
  test16(0x2000);
  test16(0x4000);
  test16(0x8000);
  test16(0x1234);
  test16(0x4321);
  test16(0xdead);
  test16(0xbeef);
  test16(0xcafe);
  test16(0xbabe);

  return 0;
}
