/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2 -msse4.1" } */

typedef __int128 v1ti __attribute__ ((__vector_size__ (16)));
typedef __int128 ti;

ti ashr(ti x, unsigned int i) { return x >> i; }

v1ti ashr_1(v1ti x) { return x >> 1; }
v1ti ashr_2(v1ti x) { return x >> 2; }
v1ti ashr_7(v1ti x) { return x >> 7; }
v1ti ashr_8(v1ti x) { return x >> 8; }
v1ti ashr_9(v1ti x) { return x >> 9; }
v1ti ashr_15(v1ti x) { return x >> 15; }
v1ti ashr_16(v1ti x) { return x >> 16; }
v1ti ashr_17(v1ti x) { return x >> 17; }
v1ti ashr_23(v1ti x) { return x >> 23; }
v1ti ashr_24(v1ti x) { return x >> 24; }
v1ti ashr_25(v1ti x) { return x >> 25; }
v1ti ashr_31(v1ti x) { return x >> 31; }
v1ti ashr_32(v1ti x) { return x >> 32; }
v1ti ashr_33(v1ti x) { return x >> 33; }
v1ti ashr_47(v1ti x) { return x >> 47; }
v1ti ashr_48(v1ti x) { return x >> 48; }
v1ti ashr_49(v1ti x) { return x >> 49; }
v1ti ashr_63(v1ti x) { return x >> 63; }
v1ti ashr_64(v1ti x) { return x >> 64; }
v1ti ashr_65(v1ti x) { return x >> 65; }
v1ti ashr_72(v1ti x) { return x >> 72; }
v1ti ashr_79(v1ti x) { return x >> 79; }
v1ti ashr_80(v1ti x) { return x >> 80; }
v1ti ashr_81(v1ti x) { return x >> 81; }
v1ti ashr_95(v1ti x) { return x >> 95; }
v1ti ashr_96(v1ti x) { return x >> 96; }
v1ti ashr_97(v1ti x) { return x >> 97; }
v1ti ashr_111(v1ti x) { return x >> 111; }
v1ti ashr_112(v1ti x) { return x >> 112; }
v1ti ashr_113(v1ti x) { return x >> 113; }
v1ti ashr_119(v1ti x) { return x >> 119; }
v1ti ashr_120(v1ti x) { return x >> 120; }
v1ti ashr_121(v1ti x) { return x >> 121; }
v1ti ashr_126(v1ti x) { return x >> 126; }
v1ti ashr_127(v1ti x) { return x >> 127; }

typedef v1ti (*fun)(v1ti);

struct {
  unsigned int i;
  fun ashr;
} table[35] = {
  {   1, ashr_1   },
  {   2, ashr_2   },
  {   7, ashr_7   },
  {   8, ashr_8   },
  {   9, ashr_9   },
  {  15, ashr_15  },
  {  16, ashr_16  },
  {  17, ashr_17  },
  {  23, ashr_23  },
  {  24, ashr_24  },
  {  25, ashr_25  },
  {  31, ashr_31  },
  {  32, ashr_32  },
  {  33, ashr_33  },
  {  47, ashr_47  },
  {  48, ashr_48  },
  {  49, ashr_49  },
  {  63, ashr_63  },
  {  64, ashr_64  },
  {  65, ashr_65  },
  {  72, ashr_72  },
  {  79, ashr_79  },
  {  80, ashr_80  },
  {  81, ashr_81  },
  {  95, ashr_95  },
  {  96, ashr_96  },
  {  97, ashr_97  },
  { 111, ashr_111 },
  { 112, ashr_112 },
  { 113, ashr_113 },
  { 119, ashr_119 },
  { 120, ashr_120 },
  { 121, ashr_121 },
  { 126, ashr_126 },
  { 127, ashr_127 }
};

void test(ti x)
{
  unsigned int i;
  v1ti t = (v1ti)x;

  for (i=0; i<(sizeof(table)/sizeof(table[0])); i++) {
    if ((ti)(*table[i].ashr)(t) != ashr(x,table[i].i))
      __builtin_abort();
  }
}

int main()
{
  ti x;

  x = ((ti)0x0011223344556677ull)<<64 | 0x8899aabbccddeeffull;
  test(x);
  x = ((ti)0xffeeddccbbaa9988ull)<<64 | 0x7766554433221100ull;
  test(x);
  x = ((ti)0x0123456789abcdefull)<<64 | 0x0123456789abcdefull;
  test(x);
  x = ((ti)0xfedcba9876543210ull)<<64 | 0xfedcba9876543210ull;
  test(x);
  x = ((ti)0x0123456789abcdefull)<<64 | 0xfedcba9876543210ull;
  test(x);
  x = ((ti)0xfedcba9876543210ull)<<64 | 0x0123456789abcdefull;
  test(x);
  x = 0;
  test(x);
  x = 0xffffffffffffffffull;
  test(x);
  x = ((ti)0xffffffffffffffffull)<<64;
  test(x);
  x = ((ti)0xffffffffffffffffull)<<64 | 0xffffffffffffffffull;
  test(x);
  x = ((ti)0x5a5a5a5a5a5a5a5aull)<<64 | 0x5a5a5a5a5a5a5a5aull;
  test(x);
  x = ((ti)0xa5a5a5a5a5a5a5a5ull)<<64 | 0xa5a5a5a5a5a5a5a5ull;
  test(x);
  x = 0xffull;
  test(x);
  x = 0xff00ull;
  test(x);
  x = 0xff0000ull;
  test(x);
  x = 0xff000000ull;
  test(x);
  x = 0xff00000000ull;
  test(x);
  x = 0xff0000000000ull;
  test(x);
  x = 0xff000000000000ull;
  test(x);
  x = 0xff00000000000000ull;
  test(x);
  x = ((ti)0xffull)<<64;
  test(x);
  x = ((ti)0xff00ull)<<64;
  test(x);
  x = ((ti)0xff0000ull)<<64;
  test(x);
  x = ((ti)0xff000000ull)<<64;
  test(x);
  x = ((ti)0xff00000000ull)<<64;
  test(x);
  x = ((ti)0xff0000000000ull)<<64;
  test(x);
  x = ((ti)0xff000000000000ull)<<64;
  test(x);
  x = ((ti)0xff00000000000000ull)<<64;
  test(x);
  x = 0xdeadbeefcafebabeull;
  test(x);
  x = ((ti)0xdeadbeefcafebabeull)<<64;
  test(x);

  return 0;
}

