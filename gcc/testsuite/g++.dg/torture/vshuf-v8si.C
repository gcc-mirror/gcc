// { dg-options "-std=c++11" }
// { dg-do run }

#if __SIZEOF_INT__ == 4
typedef unsigned int V __attribute__((vector_size(32)));
typedef V VI;
#elif __SIZEOF_LONG__ == 4
typedef unsigned long V __attribute__((vector_size(32)));
typedef V VI;
#else
# define UNSUPPORTED
#endif

#define A1	0x11121314
#define B1	0x21222324
#define C1	0x31323334
#define D1	0x41424344
#define E1	0x51525354
#define F1	0x61626364
#define G1	0x71727374
#define H1	0x81828384

#define A2	0x91929394
#define B2	0xa1a2a3a4
#define C2	0xb1b2b3b4
#define D2	0xc1c2c3c4
#define E2	0xd1d2d3d4
#define F2	0xe1e2e3e4
#define G2	0xf1f2f3f4
#define H2	0x01020304

#include "vshuf-8.inc"
#include "vshuf-main.inc"
