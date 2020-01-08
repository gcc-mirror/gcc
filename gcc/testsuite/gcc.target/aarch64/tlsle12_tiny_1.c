/* { dg-do run } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-O2 -fpic -ftls-model=local-exec -mtls-size=12 -mcmodel=tiny --save-temps" } */
/* { dg-require-effective-target fpic } */
/* { dg-skip-if "TLS 12bit size for tiny" { aarch64*-*-* }  { "-mcmodel=small" "-mcmodel=large" } { "" } } */

#include "tls_1.x"

/* { dg-final { scan-assembler-times "#:tprel_lo12" 2 } } */
