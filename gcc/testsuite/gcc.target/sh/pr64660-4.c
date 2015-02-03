/* Check that atomic not ops are generated.  */
/* { dg-do compile { target { atomic_model_hard_llcs_available } } }  */
/* { dg-options "-dp -O2 -matomic-model=hard-llcs,strict" }  */
/* { dg-final { scan-assembler-times "atomic_add" 12 } }  */
/* { dg-final { scan-assembler-times "atomic_add_fetch" 4 } }  */
/* { dg-final { scan-assembler-times "atomic_and" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_and_fetch" 2 } }  */
/* { dg-final { scan-assembler-times "atomic_or" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_or_fetch" 2 } }  */
/* { dg-final { scan-assembler-times "atomic_xor" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_xor_fetch" 2 } }  */
/* { dg-final { scan-assembler-times "atomic_nand" 6 } }  */
/* { dg-final { scan-assembler-times "atomic_nand_fetch" 2 } }  */
/* { dg-final { scan-assembler-times "atomic_not" 12 } }  */
/* { dg-final { scan-assembler-times "atomic_not_fetch" 4 } }  */

#include "pr64660-0.h"
