/* { dg-do run } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target aarch64_tlsle32 } */
/* { dg-options "-O2 -fpic -ftls-model=local-exec -mtls-size=48 --save-temps" } */
/* { dg-require-effective-target fpic } */
/* { dg-skip-if "TLS size trunc for small" { aarch64*-*-* }  { "-mcmodel=tiny" "-mcmodel=large" } { "" } } */

#include "tls_1.x"

/* { dg-final { scan-assembler-times "#:tprel_g1" 2 } } */
/* { dg-final { scan-assembler-times "#:tprel_g0_nc" 2 } } */
