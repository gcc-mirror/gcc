/* pic used here to prevent the assembler to emit .nopic directive.  */
/* { dg-do compile } */
/* { dg-options "-march=rv32imafd_xmipscbop -fpic" { target { rv32 } } } */
/* { dg-options "-march=rv64imafd_xmipscbop -fpic -mabi=lp64d" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */


void prefetch_read(char *a)
{
  __builtin_prefetch (&a[3], 0, 0);
}

void prefetch_write(char *a)
{
  __builtin_prefetch (&a[1], 1, 0);
}

void prefetch_read_out_range_offset(char *a)
{
  __builtin_prefetch (&a[512], 0, 1);
}

void prefetch_write_out_range_offset(char *a)
{
  __builtin_prefetch (&a[1024], 1, 1);
}

/* { dg-final { scan-assembler-times "mips.pref\t8,0\\(\[a-x0-9\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "mips.pref\t8,3\\(\[a-x0-9\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "nop" 2 } } */

