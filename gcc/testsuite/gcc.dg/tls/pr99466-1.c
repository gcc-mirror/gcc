/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-require-effective-target tls_emulated } */
/* { dg-add-options tls } */
__attribute__((weak))
__thread int tlsvar = 3;
/* { dg-final { scan-assembler ".weak_definition ___emutls_t.tlsvar" { target *-*-darwin* } } } */
/* { dg-final { scan-assembler-not ".private_extern ___emutls_t.tlsvar" { target *-*-darwin* } } } */
