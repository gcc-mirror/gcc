/* { dg-do compile { target rv64 } } */
/* { dg-options "-O2 -march=rv64gcv_zvl256b_zic64b_zicboz -mabi=lp64d --param=memset-size-threshold=63" } */

void
clear_block (void *dst)
{
  dst = __builtin_assume_aligned (dst, 64);
  __builtin_memset (dst, 0, 256);
}

/* { dg-final { scan-assembler-not "cbo\\.zero" } } */
/* { dg-final { scan-assembler "\\t(call|tail)\\tmemset" } } */
