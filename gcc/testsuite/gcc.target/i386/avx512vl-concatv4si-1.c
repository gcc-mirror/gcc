/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl" } */

typedef int V __attribute__((vector_size (8)));
typedef int W __attribute__((vector_size (16)));

void
f1 (V x, V y)
{
  register W c __asm ("xmm16");
  c = (W) { x[0], x[1], x[0], x[1] };
  asm volatile ("" : "+v" (c));
}

void
f2 (V x, V *y)
{
  register W c __asm ("xmm16");
  c = (W) { x[0], x[1], (*y)[0], (*y)[1] };
  asm volatile ("" : "+v" (c));
}

/* { dg-final { scan-assembler-times "vpunpcklqdq\[^\n\r]*xmm16" 2 } } */
