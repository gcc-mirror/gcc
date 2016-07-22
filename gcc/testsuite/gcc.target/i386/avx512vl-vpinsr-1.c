/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mno-avx512bw -mno-avx512dq" } */

typedef char v16qi __attribute__((vector_size (16)));
typedef short v8hi __attribute__((vector_size (16)));
typedef int v4si __attribute__((vector_size (16)));
typedef long long v2di __attribute__((vector_size (16)));

v16qi
f1 (v16qi a, char b)
{
  register v16qi c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v16qi d = c;
  ((char *) &d)[3] = b;
  c = d;
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-not "vpinsrb\[^\n\r]*xmm16" } } */

v8hi
f2 (v8hi a, short b)
{
  register v8hi c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v8hi d = c;
  ((short *) &d)[3] = b;
  c = d;
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-not "vpinsrw\[^\n\r]*xmm16" } } */

v4si
f3 (v4si a, int b)
{
  register v4si c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v4si d = c;
  ((int *) &d)[3] = b;
  c = d;
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-not "vpinsrd\[^\n\r]*xmm16" } } */

v2di
f4 (v2di a, char b)
{
  register v2di c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v2di d = c;
  ((long long *) &d)[1] = b;
  c = d;
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-not "vpinsrq\[^\n\r]*xmm16" } } */
