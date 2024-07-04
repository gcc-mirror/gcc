/* { dg-do compile } */
/* { dg-options "-O0 -fno-plt -mcmodel=normal -mtls-dialect=trad -mexplicit-relocs" } */
/* { dg-final { scan-assembler "pcalau12i\t.*%got_pc_hi20\\(__tls_get_addr\\)\n\tld\.d.*%got_pc_lo12\\(__tls_get_addr\\)" { target tls_native } } } */

__attribute__ ((tls_model ("global-dynamic"))) __thread int a;

void
test (void)
{
  a = 10;
}

