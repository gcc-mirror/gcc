#/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mbmi2 -O2" } */
/* { dg-final { scan-assembler-times {(?n)shrx[\t ]+} 1 } } */
/* { dg-final { scan-assembler-times {(?n)bzhi[\t ]+} 1 } } */

unsigned long long bextr_u64(unsigned long long w, unsigned off, unsigned int len)
{
    return (w >> off) & ((1U << len) - 1U);
}
