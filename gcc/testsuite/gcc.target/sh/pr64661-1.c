/* Check that addressing modes for atomics are generated as expected.  */
/* { dg-do compile { target { atomic_model_soft_gusa_available } } }  */
/* { dg-options "-O2 -matomic-model=soft-gusa,strict" }  */
/* { dg-final { scan-assembler-times "@\\(16,r\[0-9\]\\)" 72 } }  */

#include "pr64661-0.h"
