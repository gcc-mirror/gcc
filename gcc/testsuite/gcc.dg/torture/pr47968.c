/* { dg-do compile } */
/* { dg-options "-w -Wno-psabi" } */

typedef __attribute__ ((vector_size (16))) float float4;
typedef __attribute__ ((vector_size (16))) double double2;

float foo (double2 d2)
{
  float4 f4 = (float4) d2;
  return *(float *) &f4;
}
