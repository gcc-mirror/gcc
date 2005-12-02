/* { dg-do compile } */
/* { dg-options "-O2 -fPIC" } */
/* { dg-require-effective-target tls } */

static __thread void *baz [4] __attribute__((tls_model ("initial-exec")));
void foo (void)
{
  void **u = (void **) baz;
  
  u[0] = 0;
  u[1] = 0;
}

/* { dg-final { scan-assembler-not "\[48\]\\+baz" { target i?86-*-* x86_64-*-* } } } */
