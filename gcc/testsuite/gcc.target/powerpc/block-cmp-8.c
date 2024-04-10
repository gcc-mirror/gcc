/* { dg-do run { target ilp32 } } */
/* { dg-options "-O2 -mpowerpc64" } */
/* { dg-require-effective-target has_arch_ppc64 } */
/* { dg-timeout-factor 2 } */

/* Verify memcmp on m32 mpowerpc64 */

#include "../../gcc.dg/memcmp-1.c"
