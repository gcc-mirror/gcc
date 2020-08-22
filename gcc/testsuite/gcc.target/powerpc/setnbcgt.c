/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#define NAME gt
#define CODE >

#include "setnbc.h"

/* "x > -1" is done without setnbc.  */
/* { dg-final { scan-assembler-times {\msetnbc\M} 16 } } */
