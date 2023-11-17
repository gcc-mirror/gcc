/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -mmemcpy-strategy=auto" } */

#include "cpymem-strategy.h"

/* { dg-final { scan-assembler-times {v[ls]+e[0-9]+\.v\tv[0-9]+\,0\([a-z0-9]+\)} 4 } } */
