/* { dg-do compile } */
/* { dg-options "-mno-bwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned int unaligned_dst_si[17] = { [0 ... 16] = 0xc8c9cacb };

void
memcpy_unaligned_src_si (const void *src)
{
  __builtin_memcpy (unaligned_dst_si + 1, src, 60);
}

/* { dg-final { scan-assembler-times "\\sldq_u\\s" 10 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s" 15 } } */
/* { dg-final { scan-assembler-not "\\s(?:ldl|stq_u)\\s" } } */
