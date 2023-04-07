/* { dg-do compile } */
/* { dg-skip-if "" { ! *-linux-* } } */
/* { dg-options "-O2 -mxtls" } */
/* { dg-final { scan-assembler "jsr __m68k_read_tp" } } */
/* { dg-final { scan-assembler "#foo\\+4@TLSLE,\%\[ad\]\[0-7\]" } } */

static int __thread foo[2];

int *
bar (void)
{
  return foo + 1;
}
