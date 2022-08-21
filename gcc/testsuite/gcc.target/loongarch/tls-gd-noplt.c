/* { dg-do compile } */
/* { dg-options "-O2 -fno-plt -mcmodel=normal" } */
/* { dg-final { scan-assembler "pcalau12i\t.*%got_pc_hi20\\(__tls_get_addr\\)" } } */

__attribute__ ((tls_model ("global-dynamic"))) __thread int a;

void
test (void)
{
  a = 10;
}

