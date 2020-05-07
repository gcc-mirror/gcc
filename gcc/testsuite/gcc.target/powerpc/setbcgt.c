/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=future" } */

#define NAME gt
#define CODE >

#include "setbc.h"

/* "x > -1" is done without setbc.  */
/* { dg-final { scan-assembler-times {\msetbc\M} 16 } } */
