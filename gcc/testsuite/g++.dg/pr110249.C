/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-alias" } */

#include <stdint.h>
#include <string.h>

uint64_t read64r(const uint64_t &x) {
    if ((uint64_t) &x % 8 ) {
        __builtin_unreachable();
    }
     uint64_t value;
     memcpy( &value, &x, sizeof(uint64_t) );
     return value;
}

/* { dg-final { scan-tree-dump "fff8" "vrp1" } } */
