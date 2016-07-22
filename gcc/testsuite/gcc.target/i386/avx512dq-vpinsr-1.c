/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mavx512dq" } */

typedef int v4si __attribute__((vector_size (16)));
typedef long long v2di __attribute__((vector_size (16)));

v4si
f1 (v4si a, int b)
{
  register v4si c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v4si d = c;
  ((int *) &d)[3] = b;
  c = d;
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler "vpinsrd\[^\n\r]*xmm16" } } */

v2di
f2 (v2di a, long long b)
{
  register v2di c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v2di d = c;
  ((long long *) &d)[1] = b;
  c = d;
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler "vpinsrq\[^\n\r]*xmm16" } } */
