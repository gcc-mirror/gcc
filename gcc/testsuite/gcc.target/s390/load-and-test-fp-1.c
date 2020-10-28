/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z196" } */

/* Use load-and-test instructions if compared for (in)equality and if variable
   `a` is dead after the comparison.  For all other cases use
   compare-and-signal instructions.  */

#include "load-and-test-fp.h"

/* { dg-final { scan-assembler-times "ltdbr\t" 2 } } */
/* { dg-final { scan-assembler-times "cdbr\t" 2 } } */
/* { dg-final { scan-assembler-times "kdbr\t" 8 } } */
