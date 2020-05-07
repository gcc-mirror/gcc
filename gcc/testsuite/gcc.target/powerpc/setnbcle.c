/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=future" } */

#define NAME le
#define CODE <=

#include "setnbc.h"

/* "x <= -1" is done without setnbc.  */
/* { dg-final { scan-assembler-times {\msetnbcr\M} 16 } } */
