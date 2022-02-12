/* { dg-options "-mcpu=neoverse-512tvb -msve-vector-bits=512" } */

typedef float __attribute__((__vector_size__ (64))) F;

F
foo (float t)
{
  return (F){t, 0, 0, 0, 0, 0, 0, 5, 0, t};
}
