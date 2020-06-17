/* { dg-do run } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-O2 -fpic -ftls-model=local-exec -mtls-size=24 --save-temps" } */
/* { dg-require-effective-target fpic } */

#include "tls_1.x"

/* { dg-final { scan-assembler-times "#:tprel_lo12_nc" 2 } } */
/* { dg-final { scan-assembler-times "#:tprel_hi12" 2 } } */
/* { dg-final { cleanup-saved-temps } } */
