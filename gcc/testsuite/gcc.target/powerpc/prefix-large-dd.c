/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether prefixed instructions with large numeric offsets are generated
   for the _Decimal64 type.  */

#define TYPE _Decimal64

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mplfd\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstfd\M} 2 } } */
