/* Check that atomic ops utilize insns with immediate values.  */
/* { dg-do compile { target { atomic_model_soft_gusa_available } } }  */
/* { dg-options "-O2 -matomic-model=soft-gusa,strict" }  */
/* { dg-final { scan-assembler-times "add\t#1" 6 } }  */
/* { dg-final { scan-assembler-times "add\t#-1" 6 } }  */

#include "pr64659-0.h"
