/* Check that the appropriate atomic insns are used if the result values
   are unused.  */
/* { dg-do compile { target { atomic_model_soft_tcb_available } } }  */
/* { dg-options "-dp -O2 -matomic-model=soft-tcb,gbr-offset=0,strict" }  */
/* { dg-final { scan-assembler-times "atomic_add" 12 } }  */
/* { dg-final { scan-assembler-times "atomic_and" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_or" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_xor" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_nand" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_not" 12 } }  */
/* { dg-final { scan-assembler-not "fetch" } }  */

#include "pr64660-0.h"
