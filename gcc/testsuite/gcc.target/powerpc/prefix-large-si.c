/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether prefixed instructions with large numeric offsets are generated
   for the _Decimal64 type.  */

#define TYPE int

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mplw[az]\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstw\M}     2 } } */
