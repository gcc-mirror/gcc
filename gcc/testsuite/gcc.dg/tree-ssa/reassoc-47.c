/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -ftree-vectorize" } */

#define MODIFY
#include "reassoc-46.h"

/* Check that if the loop accumulator is saved into a global variable, it's
   still added last.  */
/* { dg-final { scan-tree-dump-times {(?:vect_)?sum_[\d._]+ = (?:(?:vect_)?_[\d._]+ \+ (?:vect_)?sum_[\d._]+|(?:vect_)?sum_[\d._]+ \+ (?:vect_)?_[\d._]+)} 1 "optimized" } } */
