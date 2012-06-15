#if __SIZEOF_LONG_LONG__ == 8
typedef unsigned long long V __attribute__((vector_size(32)));
typedef V VI;
#else
#define UNSUPPORTED
#endif

#define A	0x1112131415161718
#define B	0x2122232425262728
#define C	0x3132333435363738
#define D	0x4142434445464748

#define W	0xc1c2c3c4c5c6c7c8
#define X	0xd1d2d3d4d5d6d7d8
#define Y	0xe1e2e3e4e5e6e7e8
#define Z	0xf1f2f3f4f5f6f7f8

#include "vshuf-4.inc"
#include "vshuf-main.inc"
