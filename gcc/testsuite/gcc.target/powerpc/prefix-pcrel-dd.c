/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether pc-relative prefixed instructions are generated for the
   _Decimal64 type.  */

#define TYPE _Decimal64

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplfd\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstfd\M} 2 } } */
