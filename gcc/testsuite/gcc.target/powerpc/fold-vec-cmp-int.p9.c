/* Verify that overloaded built-ins for vec_cmp with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2" } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */

#include "fold-vec-cmp-int.h"

/* { dg-final { scan-assembler-times "vcmpequw" 2 } } */
/* { dg-final { scan-assembler-times "vcmpnew" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtsw" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtuw" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 4 } } */
