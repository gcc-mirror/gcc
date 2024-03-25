/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -mstringop-strategy=vector" } */

#include "cpymem-strategy.h"

/* { dg-final { scan-assembler-times {call\tmemcpy} 2 } } */
