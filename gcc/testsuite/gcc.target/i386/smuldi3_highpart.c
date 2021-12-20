/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */
typedef int __attribute ((mode(TI))) ti_t;

long foo(long x)
{
  return ((ti_t)x * 19065) >> 72;
}

/* { dg-final { scan-assembler "movl\[ \\t]+\\\$19065, %eax" } } */
/* { dg-final { scan-assembler-times "movq" 1 } } */
