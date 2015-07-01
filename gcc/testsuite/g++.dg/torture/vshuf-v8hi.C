// { dg-options "-std=c++11" }
// { dg-do run }

typedef unsigned short V __attribute__((vector_size(16)));
typedef V VI;

#define A1	0x1112
#define B1	0x2122
#define C1	0x3132
#define D1	0x4142
#define E1	0x5152
#define F1	0x6162
#define G1	0x7172
#define H1	0x8182

#define A2	0x9192
#define B2	0xa1a2
#define C2	0xb1b2
#define D2	0xc1c2
#define E2	0xd1d2
#define F2	0xe1e2
#define G2	0xf1f2
#define H2	0x0102

#include "vshuf-8.inc"
#include "vshuf-main.inc"
