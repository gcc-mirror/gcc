/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-O -mtp=tpidrprw" } */

#include "mtp.c"

/* { dg-final { scan-assembler-times {mrc\tp15, 0, r3, c13, c0, 4} 1 } } */
