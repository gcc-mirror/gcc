/* { dg-do compile  { target { ! ia32 } } } */
/* { dg-options "-msse2 -O2" } */

typedef double v2df __attribute__((vector_size (16)));

struct big { char pad[0x7FFFFFF0]; v2df v; };

v2df f (struct big b) { return b.v; }
