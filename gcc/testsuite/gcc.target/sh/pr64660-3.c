/* Check that the appropriate atomic insns are used if the result values
   are unused.  */
/* { dg-do compile { target { atomic_model_soft_imask_available } } }  */
/* { dg-options "-dp -O2 -matomic-model=soft-imask,strict -mno-usermode" }  */
/* { dg-final { scan-assembler-times "atomic_add_fetch" 12 } }  */
/* { dg-final { scan-assembler-times "atomic_and_fetch" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_or_fetch" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_xor_fetch" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_nand_fetch" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_not_fetch" 12 } }  */

#include "pr64660-0.h"
