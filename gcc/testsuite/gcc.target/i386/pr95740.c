/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse2 -O2 -mtune=generic -mtune-ctrl=use_incdec -masm=att -mfpmath=sse" } */
/* { dg-final { scan-assembler-times {(?n)movd[\t ]*%xmm0.*%eax} 1 } } */
/* { dg-final { scan-assembler-times {(?n)incl[\t ]*%eax} 1 } } */
/* { dg-final { scan-assembler-times {(?n)movq[\t ]*%xmm0.*%rax} 1 } } */
/* { dg-final { scan-assembler-times {(?n)incq[\t ]*%rax} 1 } } */

int
foo (float a)
{
  union{
    int b;
    float a;}u;
  u.a = a;
  return u.b + 1;
}

long long
foo1 (double a)
{
  union{
    long long b;
    double a;}u;
  u.a = a;
  return u.b + 1;
}
