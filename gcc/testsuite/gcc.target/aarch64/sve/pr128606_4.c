/* { dg-options "-fgimple -fdump-tree-gimple -msve-vector-bits=scalable" } */
/* { dg-final { scan-tree-dump {v = \{ 2, 3, 5, 7, 11, 13, 17, 19, (0, ){8}\.\.\. \};} "gimple" } } */

#define ELEM_TYPE uint8_t
#define NUM_PRIMES 8
#include "pr128606_primes.h"
