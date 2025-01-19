/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned int aligned_src_si[17] = { [0 ... 16] = 0xeaebeced };
unsigned int aligned_dst_si[17] = { [0 ... 16] = 0xdcdbdad9 };

void
memcpy_aligned_data_si (void)
{
  __builtin_memcpy (aligned_dst_si + 1, aligned_src_si + 1, 60);
}

/* { dg-final { scan-assembler-times "\\sldl\\s" 15 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s" 15 } } */
/* { dg-final { scan-assembler-not "\\s(?:ldq_u|stq_u)\\s" } } */
