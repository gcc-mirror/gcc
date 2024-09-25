/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -fno-vect-cost-model -msse4.1" } */

struct aq { long x,y; };
long testq(struct aq a) { return a.x+a.y; }

struct aw { short a0,a1,a2,a3,a4,a5,a6,a7; };
short testw(struct aw a) { return a.a0+a.a1+a.a2+a.a3+a.a4+a.a5+a.a6+a.a7; }

struct ad { int x,y,z,w; };
int testd(struct ad a) { return a.x+a.y+a.z+a.w; }
/* { dg-final { scan-assembler-not "%rsp" } } */
