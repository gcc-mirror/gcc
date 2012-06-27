// { dg-options "-std=c++11" }
// { dg-do run }

typedef unsigned char V __attribute__((vector_size(8)));
typedef V VI;

#define A1	0x11
#define B1	0x12
#define C1	0x13
#define D1	0x14
#define E1	0x15
#define F1	0x16
#define G1	0x17
#define H1	0x18

#define A2	0xf1
#define B2	0xf2
#define C2	0xf3
#define D2	0xf4
#define E2	0xf5
#define F2	0xf6
#define G2	0xf7
#define H2	0xf8

#include "vshuf-8.inc"
#include "vshuf-main.inc"
