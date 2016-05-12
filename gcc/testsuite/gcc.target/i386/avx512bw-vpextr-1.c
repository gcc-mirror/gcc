/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mavx512bw" } */

typedef char v16qi __attribute__((vector_size (16)));
typedef short v8hi __attribute__((vector_size (16)));
typedef int v4si __attribute__((vector_size (16)));
typedef long long v2di __attribute__((vector_size (16)));

void
f1 (v16qi a)
{
  register v16qi c __asm ("xmm16") = a;
  register unsigned char e __asm ("dl");
  asm volatile ("" : "+v" (c));
  v16qi d = c;
  e = ((unsigned char *) &d)[3];
  asm volatile ("" : : "q" (e));
}

unsigned short
f2 (v8hi a)
{
  register v8hi c __asm ("xmm16") = a;
  register unsigned short e __asm ("dx");
  asm volatile ("" : "+v" (c));
  v8hi d = c;
  e = ((unsigned short *) &d)[3];
  asm volatile ("" : : "r" (e));
}

unsigned int
f3 (v16qi a)
{
  register v16qi c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v16qi d = c;
  return ((unsigned char *) &d)[3];
}

unsigned int
f4 (v8hi a)
{
  register v8hi c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v8hi d = c;
  return ((unsigned short *) &d)[3];
}

unsigned long long
f5 (v16qi a)
{
  register v16qi c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v16qi d = c;
  return ((unsigned char *) &d)[3];
}

unsigned long long
f6 (v8hi a)
{
  register v8hi c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v8hi d = c;
  return ((unsigned short *) &d)[3];
}

void
f7 (v16qi a, unsigned char *p)
{
  register v16qi c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v16qi d = c;
  *p = ((unsigned char *) &d)[3];
}

void
f8 (v8hi a, unsigned short *p)
{
  register v8hi c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v8hi d = c;
  *p = ((unsigned short *) &d)[3];
}

void
f9 (v4si a)
{
  register v4si c __asm ("xmm16") = a;
  register unsigned int e __asm ("xmm17");
  asm volatile ("" : "+v" (c));
  v4si d = c;
  e = ((unsigned int *) &d)[3];
  asm volatile ("" : "+v" (e));
}

void
f10 (v2di a)
{
  register v2di c __asm ("xmm16") = a;
  register unsigned long long e __asm ("xmm17");
  asm volatile ("" : "+v" (c));
  v2di d = c;
  e = ((unsigned long long *) &d)[1];
  asm volatile ("" : "+v" (e));
}

/* { dg-final { scan-assembler-times "vpextrb\[^\n\r]*xmm16" 4 } } */
/* { dg-final { scan-assembler-times "vpextrw\[^\n\r]*xmm16" 4 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[^\n\r]*xmm1\[67\]\[^\n\r]*xmm1\[67\]" 2 } } */
