/* Verify that overloaded built-ins for vec_cmp with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mcpu=power7 -O2" } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */

#include "fold-vec-cmp-int.h"

/* { dg-final { scan-assembler-times "vcmpequw" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtsw" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtuw" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 6 } } */
