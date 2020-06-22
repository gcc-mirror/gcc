/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#define NAME le
#define CODE <=

#include "setbc.h"

/* "x <= -1" is done without setbc.  */
/* { dg-final { scan-assembler-times {\msetbcr\M} 16 } } */
