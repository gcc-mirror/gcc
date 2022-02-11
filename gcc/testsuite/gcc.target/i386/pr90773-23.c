/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mtune-ctrl=avx256_store_by_pieces" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 0, 34);
}

/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "(?:movw|pextrw)\[\\t \]+.+, 32\\(%\[\^,\]+\\)" 1 } } */
