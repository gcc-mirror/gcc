/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mtp=tpidruro" } */

#include "mtp.c"

/* { dg-final { scan-assembler-times {mrc\tp15, 0, r3, c13, c0, 3} 1 } } */
