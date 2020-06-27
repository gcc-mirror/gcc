/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether pc-relative prefixed instructions are generated for the
   short type.  */

#define TYPE short

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplh[az]\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpsth\M}     2 } } */
