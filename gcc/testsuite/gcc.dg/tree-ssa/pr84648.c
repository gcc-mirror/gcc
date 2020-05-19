/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-cddce1-details" } */

typedef unsigned uint32_t __attribute__((mode (__SI__)));

int main() {
    for (uint32_t i = 0; i < (1UL << 31); ++i) {
    }
    return 0;
}

/* { dg-final { scan-tree-dump-times "Found loop 1 to be finite: upper bound found" 1 "cddce1" } } */
