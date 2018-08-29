/* { dg-do compile } */
/* { dg-skip-if "" { ! *-linux-* } } */
/* { dg-options "-O2 -fpic" } */
/* { dg-final { scan-assembler "foo@TLSGD\\(\%a5\\)" } } */
/* { dg-final { scan-assembler "bsr.l __tls_get_addr@PLTPC" } } */

extern int __thread foo;

int *
bar (void)
{
  return &foo;
}
