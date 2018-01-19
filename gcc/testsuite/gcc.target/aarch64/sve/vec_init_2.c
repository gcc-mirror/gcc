/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256" } */

typedef unsigned int vnx4si __attribute__ ((vector_size(32)));

void
f (vnx4si *ptr, int x)
{
  *ptr += (vnx4si) { x, x, 1, 2, 3, x, x, 4 };
}
