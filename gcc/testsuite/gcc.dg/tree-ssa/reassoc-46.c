/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -ftree-vectorize" } */

#include "reassoc-46.h"

/* Check that the loop accumulator is added last.  */
/* { dg-final { scan-tree-dump-times {(?:vect_)?sum_[\d._]+ = (?:(?:vect_)?_[\d._]+ \+ (?:vect_)?sum_[\d._]+|(?:vect_)?sum_[\d._]+ \+ (?:vect_)?_[\d._]+)} 1 "optimized" { target { ! vect_partial_vectors } } } } */
