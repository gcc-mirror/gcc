/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether prefixed instructions with large numeric offsets are generated
   for the unsigned int type.  */

#define TYPE unsigned int

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mplwz\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstw\M}  2 } } */
