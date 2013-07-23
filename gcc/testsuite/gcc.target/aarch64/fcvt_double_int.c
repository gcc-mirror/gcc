/* { dg-do compile } */
/* { dg-options "-O2" } */

#define GPF double
#define SUFFIX(x) x
#define GPI int

#include "fcvt.x"

/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\]+, *d\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtps\tx\[0-9\]+, *d\[0-9\]" 1 {target lp64} } } */
/* { dg-final { scan-assembler-times "fcvtps\tw\[0-9\]+, *d\[0-9\]" 2 {target lp64} } } */
/* { dg-final { scan-assembler-times "fcvtps\tw\[0-9\]+, *d\[0-9\]" 3 {target ilp32} } } */
/* { dg-final { scan-assembler-times "fcvtms\tx\[0-9\]+, *d\[0-9\]" 1 {target lp64} } } */
/* { dg-final { scan-assembler-times "fcvtms\tw\[0-9\]+, *d\[0-9\]" 2 {target lp64} } } */
/* { dg-final { scan-assembler-times "fcvtms\tw\[0-9\]+, *d\[0-9\]" 3 {target ilp32} } } */
/* { dg-final { scan-assembler-times "fcvtas\tw\[0-9\]+, *d\[0-9\]" 2 } } */
