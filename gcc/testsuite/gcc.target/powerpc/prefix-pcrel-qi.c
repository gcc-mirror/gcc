/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether pc-relative prefixed instructions are generated for the
   signed char type.  */

#define TYPE signed char

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplbz\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstb\M}  2 } } */
