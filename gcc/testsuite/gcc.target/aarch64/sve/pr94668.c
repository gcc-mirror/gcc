/* { dg-options "-O -msve-vector-bits=512" } */

typedef float v16sf __attribute__ ((vector_size(64)));
v16sf
foo (float a)
{
  return (v16sf) { 0, 0, 0, a, 0, 0, 0, 0, 0, a, 0, 0, 0, 0, 0, 0 };
}
