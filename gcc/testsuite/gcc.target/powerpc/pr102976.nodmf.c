/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mno-dense-math" } */

#include "pr102976.h"

/* { dg-final { scan-assembler-times {(?p)\mxxlor \d+,44,44\M} 1 } } */
/* { dg-final { scan-assembler-times {(?p)\mxxlor \d+,32,32\M} 1 } } */
