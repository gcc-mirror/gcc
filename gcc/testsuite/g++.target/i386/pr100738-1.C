/* { dg-do compile } */
/* { dg-options "-Ofast -mavx2" } */
/* { dg-final {scan-assembler-times "vblendvps\[ \\t\]" 2 } } */
/* { dg-final {scan-assembler-not "vpcmpeqd\[ \\t\]" } } */
/* { dg-final {scan-assembler-not "vpxor\[ \\t\]" } } */

typedef int v4si __attribute__((vector_size(16)));
typedef char v16qi __attribute__((vector_size(16)));
v4si
foo_1 (v16qi a, v4si b, v4si c, v4si d)
{
  return ((v4si)~a) < 0 ? c : d;
}

v4si
foo_2 (v16qi a, v4si b, v4si c, v4si d)
{
  return ((v4si)~a) >= 0 ? c : d;
}
