/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mtp=tpidrurw" } */

#include "mtp.c"

/* { dg-final { scan-assembler-times {mrc\tp15, 0, r3, c13, c0, 2} 1 } } */
