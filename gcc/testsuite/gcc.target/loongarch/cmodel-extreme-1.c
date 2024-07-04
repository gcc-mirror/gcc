/* { dg-do compile } */
/* { dg-options "-march=loongarch64 -mabi=lp64d -O2 -mcmodel=extreme -mtls-dialect=trad -fno-plt -mexplicit-relocs=always -fdump-rtl-final" } */

int a;
extern int b;
__thread int c __attribute__ ((tls_model ("local-exec")));
__thread int d __attribute__ ((tls_model ("initial-exec")));
__thread int e __attribute__ ((tls_model ("local-dynamic")));
__thread int f __attribute__ ((tls_model ("global-dynamic")));

void
test (void)
{
  a = b + c + d + e + f;
}

/* a, b, d, e, f, and __tls_get_addr.  */
/* { dg-final { scan-rtl-dump-times "la_pcrel64_two_parts" 6 "final" } } */
