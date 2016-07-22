/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mavx512bw" } */

typedef char v16qi __attribute__((vector_size (16)));
typedef short v8hi __attribute__((vector_size (16)));

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

/* { dg-final { scan-assembler "vpinsrb\[^\n\r]*xmm16" } } */

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

/* { dg-final { scan-assembler "vpinsrw\[^\n\r]*xmm16" } } */
