/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-require-visibility "" } */
/* { dg-require-effective-target tls_emulated } */
/* { dg-add-options tls } */
__attribute__((weak))
__attribute__((visibility ("hidden")))
__thread int tlsvar = 3;
/* { dg-final { scan-assembler ".weak_definition ___emutls_t.tlsvar" { target *-*-darwin* } } } */
/* { dg-final { scan-assembler ".private_extern ___emutls_t.tlsvar" { target *-*-darwin* } } } */
