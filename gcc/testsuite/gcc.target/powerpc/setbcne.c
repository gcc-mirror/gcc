/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#define NAME ne
#define CODE !=

#include "setbc.h"

/* { dg-final { scan-assembler-times {\msetbcr\M} 20 } } */
