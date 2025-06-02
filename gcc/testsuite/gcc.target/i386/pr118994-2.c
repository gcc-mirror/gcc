/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "\.AVG_CEIL" 6 "optimized"} } */

#define VecRoundingAvg(a, b) ((a | b) - ((a ^ b) >> 1))

typedef unsigned char GccU8x16Vec __attribute__((__vector_size__(16)));
typedef unsigned short GccU16x8Vec __attribute__((__vector_size__(16)));
typedef unsigned char GccU8x32Vec __attribute__((__vector_size__(32)));
typedef unsigned short GccU16x16Vec __attribute__((__vector_size__(32)));
typedef unsigned char GccU8x64Vec __attribute__((__vector_size__(64)));
typedef unsigned short GccU16x32Vec __attribute__((__vector_size__(64)));

GccU8x16Vec U8x16VecRoundingAvg(GccU8x16Vec a, GccU8x16Vec b) {
  return VecRoundingAvg(a, b);
}

GccU16x8Vec U16x8VecRoundingAvg(GccU16x8Vec a, GccU16x8Vec b) {
  return VecRoundingAvg(a, b);
}

GccU8x32Vec U8x32VecRoundingAvg(GccU8x32Vec a, GccU8x32Vec b) {
  return VecRoundingAvg(a, b);
}

GccU16x16Vec U16x16VecRoundingAvg(GccU16x16Vec a, GccU16x16Vec b) {
  return VecRoundingAvg(a, b);
}

GccU8x64Vec U8x64VecRoundingAvg(GccU8x64Vec a, GccU8x64Vec b) {
  return VecRoundingAvg(a, b);
}

GccU16x32Vec U16x32VecRoundingAvg(GccU16x32Vec a, GccU16x32Vec b) {
  return VecRoundingAvg(a, b);
}

