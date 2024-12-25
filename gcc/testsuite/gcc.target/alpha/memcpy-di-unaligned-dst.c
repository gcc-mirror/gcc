/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned long unaligned_src_di[9] = { [0 ... 8] = 0xfefdfcfbfaf9f8f7 };

void
memcpy_unaligned_dst_di (void *dst)
{
  __builtin_memcpy (dst, unaligned_src_di + 1, 56);
}

/* { dg-final { scan-assembler-times "\\sldq\\s" 7 } } */
/* { dg-final { scan-assembler-times "\\sldq_u\\s" 2 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 8 } } */
/* { dg-final { scan-assembler-not "\\sstq\\s" } } */
