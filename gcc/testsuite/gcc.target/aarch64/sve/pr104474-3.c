/* { dg-options "-mcpu=neoverse-v1 -frounding-math -msve-vector-bits=256" } */

typedef _Float16 __attribute__((__vector_size__ (32))) F;

F
foo (void)
{
  return (F){0, 6270, 0, 0, 0, 0, 0, 0, 3229, 0, 40};
}
