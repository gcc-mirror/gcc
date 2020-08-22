/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#define NAME eq
#define CODE ==

#include "setnbc.h"

/* { dg-final { scan-assembler-times {\msetnbc\M} 20 } } */
