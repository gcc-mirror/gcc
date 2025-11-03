/* { dg-do compile } */
/* { dg-additional-options "-mautovec-preference=sve-only -fdump-tree-vect-details -O3 --param vect-epilogues-nomask=0" } */

#include <stdbool.h>
#include <stdint.h>

void vec_slp_cmp (char* restrict a, char* restrict b, int n) {
    bool x0 = b[0] != 0;
    bool x1 = b[1] != 0;
    bool x2 = b[2] != 0;
    bool x3 = b[3] != 0;
    for (int i = 0; i < n; ++i) {
        x0 &= (a[i * 4] != 0);
        x1 &= (a[i * 4 + 1] != 0);
        x2 &= (a[i * 4 + 2] != 0);
        x3 &= (a[i * 4 + 3] != 0);
    }
    b[0] = x0;
    b[1] = x1;
    b[2] = x2;
    b[3] = x3;
}

void vec_slp_cmp1 (char* restrict a, char* restrict b, int n) {
    bool x0 = b[0] != 0;
    for (int i = 0; i < n; ++i) {
        x0 &= (a[i] != 0);
    }
    b[0] = x0;
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 2 "vect" { target aarch64*-*-* } } } */
