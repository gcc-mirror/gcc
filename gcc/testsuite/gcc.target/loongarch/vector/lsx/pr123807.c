/* { dg-do compile } */
/* { dg-options "-O0 -msimd=lsx" } */

typedef long long v2i64 __attribute__ ((__vector_size__ (16)));
v2i64 a, b;
void
test (int imm8)
{
  b = a << imm8;
}
