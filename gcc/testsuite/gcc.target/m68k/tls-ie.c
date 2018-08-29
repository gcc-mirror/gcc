/* { dg-do compile } */
/* { dg-skip-if "" { ! *-linux-* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "jsr __m68k_read_tp" } } */
/* { dg-final { scan-assembler "foo@TLSIE\\(\%a5\\)" } } */

extern int __thread foo;

int *
bar (void)
{
  return &foo;
}
