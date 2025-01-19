/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned long unaligned_dst_di[9] = { [0 ... 8] = 0xc4c5c6c7c8c9cacb };

void
memcpy_unaligned_src_di (const void *src)
{
  __builtin_memcpy (unaligned_dst_di + 1, src, 56);
}

/* { dg-final { scan-assembler-times "\\sstq\\s" 7 } } */
/* { dg-final { scan-assembler-times "\\sldq_u\\s" 8 } } */
/* { dg-final { scan-assembler-not "\\s(?:ldq|stq_u)\\s" } } */
