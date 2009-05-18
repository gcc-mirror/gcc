/* { dg-do compile } */
/* { dg-skip-if "" { ! *-linux-* } { "*" } { "" } } */
/* { dg-options "-O2 -fpic -mxgot -mxtls" } */
/* { dg-final { scan-assembler "#foo@TLSLDM,\%\[ad\]\[0-7\]" } } */
/* { dg-final { scan-assembler "bsr.l __tls_get_addr@PLTPC" } } */
/* { dg-final { scan-assembler "#foo@TLSLDO,\%\[ad\]\[0-7\]" } } */

static int __thread foo;

int *
bar (void)
{
  return &foo;
}
