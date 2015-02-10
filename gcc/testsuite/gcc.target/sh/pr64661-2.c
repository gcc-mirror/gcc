/* Check that addressing modes for atomics are generated as expected.  */
/* { dg-do compile { target { atomic_model_soft_tcb_available } } }  */
/* { dg-options "-O2 -matomic-model=soft-tcb,gbr-offset=128,strict" }  */
/* { dg-final { scan-assembler-times "@\\(16,r\[0-9\]\\)" 44 } }  */
/* { dg-final { scan-assembler-times "@\\(8,r\[0-9\]\\)" 36 } }  */
/* { dg-final { scan-assembler-times "@\\(4,r\[0-9\]\\)" 36 } }  */
/* { dg-final { scan-assembler-times "@\\(16,gbr\\)" 28 } }  */
/* { dg-final { scan-assembler-times "@\\(8,gbr\\)" 28 } }  */
/* { dg-final { scan-assembler-times "@\\(4,gbr\\)" 28 } }  */

#include "pr64661-0.h"
