/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether prefixed instructions with large numeric offsets are generated
   for the unsigned char type.  */

#define TYPE unsigned char

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mplbz\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstb\M}  2 } } */
