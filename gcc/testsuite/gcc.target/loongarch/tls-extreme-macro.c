/* { dg-do compile } */
/* { dg-options "-march=loongarch64 -mabi=lp64d -O2 -mcmodel=extreme -mtls-dialect=trad -fno-plt -mexplicit-relocs=none" } */
/* { dg-final { scan-assembler "test_le:.*la.tls.le\t\\\$r\[0-9\]+,\\\.L" { target tls_native } } } */
/* { dg-final { scan-assembler "test_ie:.*la.tls.ie\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,\\\.L" { target tls_native } } } */
/* { dg-final { scan-assembler "test_ld:.*la.tls.ld\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,\\\.L.*la.global\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,__tls_get_addr" { target tls_native } } } */
/* { dg-final { scan-assembler "test_le:.*la.tls.gd\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,\\\.L.*la.global\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,__tls_get_addr" { target tls_native } } } */

__thread int c __attribute__ ((tls_model ("local-exec")));
__thread int d __attribute__ ((tls_model ("initial-exec")));
__thread int e __attribute__ ((tls_model ("local-dynamic")));
__thread int f __attribute__ ((tls_model ("global-dynamic")));

int
test_le (void)
{
  return c;
}

int
test_ie (void)
{
  return d;
}

int
test_ld (void)
{
  return e;
}

int
test_gd (void)
{
  return f;
}
