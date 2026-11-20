/* { dg-do compile { target rv64 } } */
/* { dg-options "-O2 -march=rv64gcv -mabi=lp64d -mtune=generic-ooo --param=memset-size-threshold=3" } */

void
clear_short (void *dst)
{
  __builtin_memset (dst, 0, 7);
}

void
clear_words (void *dst)
{
  __builtin_memset (dst, 0, 32);
}

void
fill_short (void *dst, int value)
{
  __builtin_memset (dst, value, 7);
}

void
fill_words (void *dst, int value)
{
  __builtin_memset (dst, value, 32);
}

/* { dg-final { scan-assembler-times "\\t(call|tail)\\tmemset" 4 } } */
