/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether prefixed instructions with large numeric offsets are generated
   for the _Decimal32 type.  Note, the _Decimal32 type will not generate any
   prefixed load or stores, because there is no prefixed load/store instruction
   to load up a vector register as a zero extended 32-bit integer.  So we count
   the number of load addresses that are generated.  */

#define TYPE _Decimal32

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mpli\M}    3 } } */
/* { dg-final { scan-assembler-times {\mlfiwzx\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstfiwx\M} 2 } } */


