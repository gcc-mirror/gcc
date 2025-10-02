/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mtune-ctrl=avx256_move_by_pieces" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 0, 33);
}

/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "movb\[\\t \]+.+, 32\\(%\[\^,\]+\\)" 1 } } */
