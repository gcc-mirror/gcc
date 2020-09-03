/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -fdump-tree-optimized -Wno-psabi -mno-avx512f" } */

typedef unsigned int u32v4 __attribute__((vector_size(16)));
typedef unsigned short u16v16 __attribute__((vector_size(32)));
typedef unsigned char u8v16 __attribute__((vector_size(16)));

union vec128 {
  u8v16 u8;
  u32v4 u32;
};

#define memcpy __builtin_memcpy

static u16v16 zxt(u8v16 x)
{
  return (u16v16) {
    x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
    x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15]
  };
}

static u8v16 narrow(u16v16 x)
{
  return (u8v16) {
    x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
    x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15]
  };
}

void f(char *dst, char *src, unsigned long n, unsigned c)
{
  unsigned ia = 255 - (c >> 24);
  ia += ia >> 7;

  union vec128 c4 = {0}, ia16 = {0};
  c4.u32 += c;
  ia16.u8 += (unsigned char)ia;

  u16v16 c16 = (zxt(c4.u8) << 8) + 128;

  for (; n; src += 16, dst += 16, n -= 4) {
    union vec128 s;
    memcpy(&s, src, sizeof s);
    s.u8 = narrow((zxt(s.u8)*zxt(ia16.u8) + c16) >> 8);
    memcpy(dst, &s, sizeof s);
  }
}

/* { dg-final { scan-tree-dump-times "\\(vector\\(16\\) short unsigned int\\)" 3 "optimized" } } */
/* We're missing an opportunity to, after later optimizations, combine
   a uniform CTOR with a vector promotion to a CTOR on a promoted
   element.  */
/* { dg-final { scan-tree-dump-times "\\(vector\\(16\\) short unsigned int\\)" 2 "optimized" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "VEC_PACK_TRUNC" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 2 "optimized" } } */
