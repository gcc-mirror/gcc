/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "beq|bne" 1 } } */

char
t (char *p, char x)
{
  return __atomic_exchange_n (p, x, __ATOMIC_RELAXED);
}
