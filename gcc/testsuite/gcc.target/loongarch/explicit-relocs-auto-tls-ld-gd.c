/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mexplicit-relocs=auto -mtls-dialect=trad" } */

__thread int a __attribute__((visibility("hidden")));
extern __thread int b __attribute__((visibility("default")));

int test() { return a + b; }

/* { dg-final { scan-assembler "la\\.tls\\.ld" { target tls_native } } } */
/* { dg-final { scan-assembler "la\\.tls\\.gd" { target tls_native } } } */
