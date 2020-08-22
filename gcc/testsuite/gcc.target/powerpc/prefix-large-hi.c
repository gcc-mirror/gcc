/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether prefixed instructions with large numeric offsets are generated
   for the short type.  */

#define TYPE short

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mplh[az]\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpsth\M}     2 } } */
