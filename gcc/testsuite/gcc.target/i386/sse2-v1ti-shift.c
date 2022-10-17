/* { dg-do run { target int128 } } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

typedef unsigned __int128 v1ti __attribute__ ((__vector_size__ (16)));
typedef unsigned __int128 ti;

ti ashl(ti x, unsigned int i) { return x << i; }
ti lshr(ti x, unsigned int i) { return x >> i; }
ti rotr(ti x, unsigned int i) { return (x >> i) | (x << (128-i)); }

v1ti ashl_1(v1ti x) { return x << 1; }
v1ti ashl_2(v1ti x) { return x << 2; }
v1ti ashl_7(v1ti x) { return x << 7; }
v1ti ashl_8(v1ti x) { return x << 8; }
v1ti ashl_9(v1ti x) { return x << 9; }
v1ti ashl_15(v1ti x) { return x << 15; }
v1ti ashl_16(v1ti x) { return x << 16; }
v1ti ashl_17(v1ti x) { return x << 17; }
v1ti ashl_31(v1ti x) { return x << 31; }
v1ti ashl_32(v1ti x) { return x << 32; }
v1ti ashl_33(v1ti x) { return x << 33; }
v1ti ashl_63(v1ti x) { return x << 63; }
v1ti ashl_64(v1ti x) { return x << 64; }
v1ti ashl_65(v1ti x) { return x << 65; }
v1ti ashl_72(v1ti x) { return x << 72; }
v1ti ashl_95(v1ti x) { return x << 95; }
v1ti ashl_96(v1ti x) { return x << 96; }
v1ti ashl_97(v1ti x) { return x << 97; }
v1ti ashl_111(v1ti x) { return x << 111; }
v1ti ashl_112(v1ti x) { return x << 112; }
v1ti ashl_113(v1ti x) { return x << 113; }
v1ti ashl_119(v1ti x) { return x << 119; }
v1ti ashl_120(v1ti x) { return x << 120; }
v1ti ashl_121(v1ti x) { return x << 121; }
v1ti ashl_126(v1ti x) { return x << 126; }
v1ti ashl_127(v1ti x) { return x << 127; }

v1ti lshr_1(v1ti x) { return x >> 1; }
v1ti lshr_2(v1ti x) { return x >> 2; }
v1ti lshr_7(v1ti x) { return x >> 7; }
v1ti lshr_8(v1ti x) { return x >> 8; }
v1ti lshr_9(v1ti x) { return x >> 9; }
v1ti lshr_15(v1ti x) { return x >> 15; }
v1ti lshr_16(v1ti x) { return x >> 16; }
v1ti lshr_17(v1ti x) { return x >> 17; }
v1ti lshr_31(v1ti x) { return x >> 31; }
v1ti lshr_32(v1ti x) { return x >> 32; }
v1ti lshr_33(v1ti x) { return x >> 33; }
v1ti lshr_63(v1ti x) { return x >> 63; }
v1ti lshr_64(v1ti x) { return x >> 64; }
v1ti lshr_65(v1ti x) { return x >> 65; }
v1ti lshr_72(v1ti x) { return x >> 72; }
v1ti lshr_95(v1ti x) { return x >> 95; }
v1ti lshr_96(v1ti x) { return x >> 96; }
v1ti lshr_97(v1ti x) { return x >> 97; }
v1ti lshr_111(v1ti x) { return x >> 111; }
v1ti lshr_112(v1ti x) { return x >> 112; }
v1ti lshr_113(v1ti x) { return x >> 113; }
v1ti lshr_119(v1ti x) { return x >> 119; }
v1ti lshr_120(v1ti x) { return x >> 120; }
v1ti lshr_121(v1ti x) { return x >> 121; }
v1ti lshr_126(v1ti x) { return x >> 126; }
v1ti lshr_127(v1ti x) { return x >> 127; }

v1ti rotr_1(v1ti x) { return (x >> 1) | (x << 127); }
v1ti rotr_2(v1ti x) { return (x >> 2) | (x << 126); }
v1ti rotr_7(v1ti x) { return (x >> 7) | (x << 121); }
v1ti rotr_8(v1ti x) { return (x >> 8) | (x << 120); }
v1ti rotr_9(v1ti x) { return (x >> 9) | (x << 119); }
v1ti rotr_15(v1ti x) { return (x >> 15) | (x << 113); }
v1ti rotr_16(v1ti x) { return (x >> 16) | (x << 112); }
v1ti rotr_17(v1ti x) { return (x >> 17) | (x << 111); }
v1ti rotr_31(v1ti x) { return (x >> 31) | (x << 97); }
v1ti rotr_32(v1ti x) { return (x >> 32) | (x << 96); }
v1ti rotr_33(v1ti x) { return (x >> 33) | (x << 95); }
v1ti rotr_63(v1ti x) { return (x >> 63) | (x << 65); }
v1ti rotr_64(v1ti x) { return (x >> 64) | (x << 64); }
v1ti rotr_65(v1ti x) { return (x >> 65) | (x << 63); }
v1ti rotr_72(v1ti x) { return (x >> 72) | (x << 56); }
v1ti rotr_95(v1ti x) { return (x >> 95) | (x << 33); }
v1ti rotr_96(v1ti x) { return (x >> 96) | (x << 32); }
v1ti rotr_97(v1ti x) { return (x >> 97) | (x << 31); }
v1ti rotr_111(v1ti x) { return (x >> 111) | (x << 17); }
v1ti rotr_112(v1ti x) { return (x >> 112) | (x << 16); }
v1ti rotr_113(v1ti x) { return (x >> 113) | (x << 15); }
v1ti rotr_119(v1ti x) { return (x >> 119) | (x << 9); }
v1ti rotr_120(v1ti x) { return (x >> 120) | (x << 8); }
v1ti rotr_121(v1ti x) { return (x >> 121) | (x << 7); }
v1ti rotr_126(v1ti x) { return (x >> 126) | (x << 2); }
v1ti rotr_127(v1ti x) { return (x >> 127) | (x << 1); }


typedef v1ti (*fun)(v1ti);

struct {
  unsigned int i;
  fun ashl;
  fun lshr;
  fun rotr;
} table[26] = {
  {   1, ashl_1,   lshr_1,   rotr_1   },
  {   2, ashl_2,   lshr_2,   rotr_2   },
  {   7, ashl_7,   lshr_7,   rotr_7   },
  {   8, ashl_8,   lshr_8,   rotr_8   },
  {   9, ashl_9,   lshr_9,   rotr_9   },
  {  15, ashl_15,  lshr_15,  rotr_15  },
  {  16, ashl_16,  lshr_16,  rotr_16  },
  {  17, ashl_17,  lshr_17,  rotr_17  },
  {  31, ashl_31,  lshr_31,  rotr_31  },
  {  32, ashl_32,  lshr_32,  rotr_32  },
  {  33, ashl_33,  lshr_33,  rotr_33  },
  {  63, ashl_63,  lshr_63,  rotr_63  },
  {  64, ashl_64,  lshr_64,  rotr_64  },
  {  65, ashl_65,  lshr_65,  rotr_65  },
  {  72, ashl_72,  lshr_72,  rotr_72  },
  {  95, ashl_95,  lshr_95,  rotr_95  },
  {  96, ashl_96,  lshr_96,  rotr_96  },
  {  97, ashl_97,  lshr_97,  rotr_97  },
  { 111, ashl_111, lshr_111, rotr_111 },
  { 112, ashl_112, lshr_112, rotr_112 },
  { 113, ashl_113, lshr_113, rotr_113 },
  { 119, ashl_119, lshr_119, rotr_119 },
  { 120, ashl_120, lshr_120, rotr_120 },
  { 121, ashl_121, lshr_121, rotr_121 },
  { 126, ashl_126, lshr_126, rotr_126 },
  { 127, ashl_127, lshr_127, rotr_127 }
};

void test(ti x)
{
  unsigned int i;
  v1ti t = (v1ti)x;

  for (i=0; i<(sizeof(table)/sizeof(table[0])); i++) {
    if ((ti)(*table[i].ashl)(t) != ashl(x,table[i].i))
      __builtin_abort();
    if ((ti)(*table[i].lshr)(t) != lshr(x,table[i].i))
      __builtin_abort();
    if ((ti)(*table[i].rotr)(t) != rotr(x,table[i].i))
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

