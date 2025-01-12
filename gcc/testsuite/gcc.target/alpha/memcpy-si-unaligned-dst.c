/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned int unaligned_src_si[17] = { [0 ... 16] = 0xfefdfcfb };

void
memcpy_unaligned_dst_si (void *dst)
{
  __builtin_memcpy (dst, unaligned_src_si + 1, 60);
}

/* { dg-final { scan-assembler-times "\\sldl\\s" 15 } } */
/* { dg-final { scan-assembler-times "\\sldq_u\\s" 4 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 10 } } */
/* { dg-final { scan-assembler-not "\\sstl\\s" } } */
