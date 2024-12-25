/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned long aligned_src_di[9] = { [0 ... 8] = 0xe6e7e8e9eaebeced };
unsigned long aligned_dst_di[9] = { [0 ... 8] = 0xdcdbdad9d8d7d6d5 };

void
memcpy_aligned_data_di (void)
{
  __builtin_memcpy (aligned_dst_di + 1, aligned_src_di + 1, 56);
}

/* { dg-final { scan-assembler-times "\\sldq\\s" 7 } } */
/* { dg-final { scan-assembler-times "\\sstq\\s" 7 } } */
/* { dg-final { scan-assembler-not "\\s(?:ldq_u|stq_u)\\s" } } */
