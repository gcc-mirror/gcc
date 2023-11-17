/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -mmemcpy-strategy=libcall" } */

#include "cpymem-strategy.h"

/* { dg-final { scan-assembler-times {call\tmemcpy} 2 } } */
