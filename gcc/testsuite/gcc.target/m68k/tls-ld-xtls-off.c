/* { dg-do compile } */
/* { dg-skip-if "" { ! *-linux-* } } */
/* { dg-options "-O2 -fpic -mxtls" } */
/* { dg-final { scan-assembler "foo@TLSLDM\\(\%a5\\)" } } */
/* { dg-final { scan-assembler "bsr.l __tls_get_addr@PLTPC" } } */
/* { dg-final { scan-assembler "#foo\\+4@TLSLDO,\%\[ad\]\[0-7\]" } } */

static int __thread foo[2];

int *
bar (void)
{
  return foo + 1;
}
