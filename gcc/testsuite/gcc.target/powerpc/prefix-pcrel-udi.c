/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether pc-relative prefixed instructions are generated for the
   unsigned long long type.  */

#define TYPE unsigned long long

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mpld\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstd\M} 2 } } */
