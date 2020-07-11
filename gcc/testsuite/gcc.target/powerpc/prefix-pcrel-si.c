/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether pc-relative prefixed instructions are generated for the
   int type.  */

#define TYPE int

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplw[az]\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstw\M}     2 } } */
