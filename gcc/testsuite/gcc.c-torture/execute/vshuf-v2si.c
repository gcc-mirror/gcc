#if __SIZEOF_INT__ == 4
typedef unsigned int V __attribute__((vector_size(8)));
typedef V VI;
#elif __SIZEOF_LONG__ == 4
typedef unsigned long V __attribute__((vector_size(8)));
typedef V VI;
#else
#define UNSUPPORTED
#endif

#define A	0x11121314
#define B	0x21222324

#define X	0xd1d2d3d4
#define Y	0xe1e2e3e4

#include "vshuf-2.inc"
#include "vshuf-main.inc"
