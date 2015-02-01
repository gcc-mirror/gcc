/* Check that atomic not ops are generated.  */
/* { dg-do compile { target { atomic_model_soft_gusa_available } } }  */
/* { dg-options "-O2 -matomic-model=soft-gusa,strict" }  */
/* { dg-final { scan-assembler-times "not\t" 12 } }  */

#include "pr64851-0.h"
