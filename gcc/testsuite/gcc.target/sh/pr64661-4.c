/* Check that addressing modes for atomics are generated as expected.
   The LLCS patterns are limited to simple register addresses, so there's not
   much to check here.  */
/* { dg-do compile { target { atomic_model_hard_llcs_available } } }  */
/* { dg-options "-dp -O2 -matomic-model=hard-llcs,strict" }  */
/* { dg-final { scan-assembler-times "hard_1" 112 } }  */

#include "pr64661-0.h"
