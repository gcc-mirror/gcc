typedef unsigned short V __attribute__((vector_size(8)));
typedef V VI;

#define A	0x1112
#define B	0x2122
#define C	0x3132
#define D	0x4142

#define W	0xc1c2
#define X	0xd1d2
#define Y	0xe1e2
#define Z	0xf1f2

#include "vshuf-4.inc"
#include "vshuf-main.inc"
