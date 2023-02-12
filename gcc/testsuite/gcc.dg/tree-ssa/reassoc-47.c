/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -ftree-vectorize" } */

#define MODIFY
#include "reassoc-46.h"

/* Check that if the loop accumulator is modified using a chain of operations
   other than addition, its new value is still added last.  */
/* { dg-final { scan-tree-dump-times {(?:vect_)?sum_[\d._]+ = (?:(?:vect_)?_[\d._]+ \+ (?:vect_)?sum_[\d._]+|(?:vect_)?sum_[\d._]+ \+ (?:vect_)?_[\d._]+)} 1 "optimized" } } */
