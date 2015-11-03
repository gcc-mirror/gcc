/* { dg-do run } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-O2 -fpic -ftls-model=local-exec -mtls-size=32 -mcmodel=tiny --save-temps" } */
/* { dg-skip-if "TLS size trunc for tiny" { aarch64*-*-* }  { "-mcmodel=small" "-mcmodel=large" } { "" } } */

#include "tls_1.x"

/* { dg-final { scan-assembler-times "#:tprel_lo12_nc" 2 } } */
/* { dg-final { scan-assembler-times "#:tprel_hi12" 2 } } */
