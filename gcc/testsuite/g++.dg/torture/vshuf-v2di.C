// { dg-options "-std=c++11" }
// // { dg-do run }

#if __SIZEOF_LONG_LONG__ == 8
typedef unsigned long long V __attribute__((vector_size(16)));
typedef V VI;
#else
#define UNSUPPORTED
#endif

#define A	0x1112131415161718
#define B	0x2122232425262728

#define X	0xc1c2c3c4c5c6c7c8
#define Y	0xd1d2d3d4d5d6d7d8

#include "vshuf-2.inc"
#include "vshuf-main.inc"
