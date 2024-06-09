/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mexplicit-relocs=auto -mtls-dialect=desc" } */

__thread int a __attribute__((visibility("hidden")));
extern __thread int b __attribute__((visibility("default")));

int test() { return a + b; }

/* { dg-final { scan-assembler "la\\.tls\\.desc\t\\\$r4,\\.LANCHOR0" { target tls_native } } } */
/* { dg-final { scan-assembler "la\\.tls\\.desc\t\\\$r4,\\.LANCHOR0" { target tls_native } } } */
