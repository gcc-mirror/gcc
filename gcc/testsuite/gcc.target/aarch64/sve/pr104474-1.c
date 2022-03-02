/* { dg-options "-mcpu=neoverse-512tvb -frounding-math -msve-vector-bits=512" } */

typedef float __attribute__((__vector_size__ (64))) F;

F
foo (void)
{
  return (F){68435453, 0, 0, 0, 0, 0, 0, 5, 0, 431144844};
}
