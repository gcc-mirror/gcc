/* { dg-options "-fgimple -fdump-tree-gimple -msve-vector-bits=256" } */
/* { dg-final { scan-tree-dump {v = \{ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131 \};} "gimple" } } */

#define ELEM_TYPE uint8_t
#define NUM_PRIMES 32
#include "pr128606_primes.h"
