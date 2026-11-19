/* { dg-do compile { target rv64 } } */
/* { dg-options "-O2 -march=rv64gcv -mabi=lp64d" } */
/* { dg-additional-options "-mstringop-strategy=vector" } */
/* { dg-additional-options "--param=memmove-size-threshold=512" } */

void
copy (void *dst, const void *src, __SIZE_TYPE__ n)
{
  __builtin_memcpy (dst, src, n);
}

/* { dg-final { scan-assembler-not "\\t(call|tail)\\tmemcpy" } } */
/* { dg-final { scan-assembler "\\tvle8\\.v\\t" } } */
