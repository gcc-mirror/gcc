/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O0 -fno-pic -fno-plt -mexplicit-relocs -mcmodel=medium" } */
/* { dg-final { scan-assembler "test:.*pcalau12i\t.*%got_pc_hi20\\(g\\)\n\tld\.d\t.*%got_pc_lo12\\(g\\)\n\tjirl" } } */
/* { dg-final { scan-assembler "test1:.*pcalau12i\t.*%pc_hi20\\(f\\)\n\tjirl.*%pc_lo12\\(f\\)" } } */
/* { dg-final { scan-assembler "test2:.*pcalau12i\t.*%pc_hi20\\(l\\)\n\tjirl.*%pc_lo12\\(l\\)" } } */
/* { dg-final { scan-assembler "test3:.*pcalau12i.*%got_pc_hi20\\(__tls_get_addr\\)\n\tld\.d.*%got_pc_lo12\\(__tls_get_addr\\)" { target tls_native } } } */
/* { dg-final { scan-assembler "test3:.*pcalau12i.*%got_pc_hi20\\(__tls_get_addr\\)\n\tld\.d.*%got_pc_lo12\\(__tls_get_addr\\)" { target tls_native } } } */

extern void g (void);

void
f (void)
{}

static void
l (void)
{}

void
test (void)
{
  g ();
}

void
test1 (void)
{
  f ();
}

void
test2 (void)
{
  l ();
}

__attribute__ ((tls_model ("global-dynamic"))) __thread int a;

void
test3 (void)
{
  a = 10;
}
