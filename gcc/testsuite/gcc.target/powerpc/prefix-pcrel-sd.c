/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether pc-relative prefixed instructions are generated for the
   _Decimal32 type.  Note, the _Decimal32 type will not generate any prefixed
   load or stores, because there is no prefixed load/store instruction to load
   up a vector register as a zero extended 32-bit integer.  So we count the
   number of load addresses that are generated.  */

#define TYPE _Decimal32

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mpla\M}  3 } } */
