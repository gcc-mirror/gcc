#if __SIZEOF_INT__ == 4
typedef unsigned int V __attribute__((vector_size(16)));
typedef V VI;
#elif __SIZEOF_LONG__ == 4
typedef unsigned long V __attribute__((vector_size(16)));
typedef V VI;
#else
# define UNSUPPORTED
#endif

#define A	0x11121314
#define B	0x21222324
#define C	0x31323334
#define D	0x41424344

#define W	0xc1c2c3c4
#define X	0xd1d2d3d4
#define Y	0xe1e2e3e4
#define Z	0xf1f2f3f4

#include "vshuf-4.inc"
#include "vshuf-main.inc"
