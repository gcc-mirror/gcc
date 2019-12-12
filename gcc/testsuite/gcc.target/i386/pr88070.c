/* PR target/88070 */
/* { dg-do compile } */
/* { dg-options "-O -fexpensive-optimizations -fnon-call-exceptions -fschedule-insns -fno-dce -fno-dse -mavx" } */

typedef float vfloat2 __attribute__ ((__vector_size__ (2 * sizeof (float))));

vfloat2
test1float2 (float c)
{
  vfloat2 v = { c, c };
  return v;
}
