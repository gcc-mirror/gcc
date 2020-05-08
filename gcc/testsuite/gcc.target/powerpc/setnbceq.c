/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=future" } */

#define NAME eq
#define CODE ==

#include "setnbc.h"

/* { dg-final { scan-assembler-times {\msetnbc\M} 20 } } */
