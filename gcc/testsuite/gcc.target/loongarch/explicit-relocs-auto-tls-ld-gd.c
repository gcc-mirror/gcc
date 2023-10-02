/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mexplicit-relocs=auto" } */

__thread int a __attribute__((visibility("hidden")));
extern __thread int b __attribute__((visibility("default")));

int test() { return a + b; }

/* { dg-final { scan-assembler-not "la.tls" { target tls_native } } } */
