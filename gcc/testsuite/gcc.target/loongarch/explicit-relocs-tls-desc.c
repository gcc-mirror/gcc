/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mexplicit-relocs -mtls-dialect=desc" } */

__thread int a __attribute__((visibility("hidden")));
extern __thread int b __attribute__((visibility("default")));

int test() { return a + b; }

/* { dg-final { scan-assembler "pcalau12i\t\\\$r4,%desc_pc_hi20\\\(\\.LANCHOR0\\\)" { target tls_native } } } */
/* { dg-final { scan-assembler "addi.d\t\\\$r4,\\\$r4,%desc_pc_lo12\\\(\\.LANCHOR0\\\)" { target tls_native } } } */
/* { dg-final { scan-assembler "ld.d\t\\\$r1,\\\$r4,%desc_ld\\\(\\.LANCHOR0\\\)" { target tls_native } } } */
/* { dg-final { scan-assembler "jirl\t\\\$r1,\\\$r1,%desc_call\\\(\\.LANCHOR0\\\)" { target tls_native } } } */
/* { dg-final { scan-assembler "add.d\t\\\$r12,\\\$r4,\\\$r2" { target tls_native } } } */
