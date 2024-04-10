/* Verify that overloaded built-ins for vec_cmp{eq,ge,gt,le,lt,ne} with
   char inputs produce the right code when -mcpu=power8 is specified.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */

#include "fold-vec-cmp-char.h"

/* { dg-final { scan-assembler-times "vcmpequb" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtsb" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtub" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 6 } } */

