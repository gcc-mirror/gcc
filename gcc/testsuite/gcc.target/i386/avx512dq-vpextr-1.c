/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mavx512dq" } */

typedef int v4si __attribute__((vector_size (16)));
typedef long long v2di __attribute__((vector_size (16)));

unsigned int
f1 (v4si a)
{
  register v4si c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v4si d = c;
  return ((unsigned int *) &d)[3];
}

unsigned long long
f2 (v2di a)
{
  register v2di c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v2di d = c;
  return ((unsigned long long *) &d)[1];
}

unsigned long long
f3 (v4si a)
{
  register v4si c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v4si d = c;
  return ((unsigned int *) &d)[3];
}

void
f4 (v4si a, unsigned int *p)
{
  register v4si c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v4si d = c;
  *p = ((unsigned int *) &d)[3];
}

void
f5 (v2di a, unsigned long long *p)
{
  register v2di c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  v2di d = c;
  *p = ((unsigned long long *) &d)[1];
}

/* { dg-final { scan-assembler-times "vpextrd\[^\n\r]*xmm16" 3 } } */
/* { dg-final { scan-assembler-times "vpextrq\[^\n\r]*xmm16" 2 } } */
