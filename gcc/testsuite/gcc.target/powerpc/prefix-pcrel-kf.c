/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Tests whether pc-relative prefixed instructions are generated for the
   _Float128 type.  */

#define TYPE _Float128

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplxv\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstxv\M} 2 } } */
