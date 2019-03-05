/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-cddce1-details" } */

int main() {
    for (unsigned i = 0; i < (1u << 31); ++i) {
    }
    return 0;
}

/* { dg-final { scan-tree-dump-times "Found loop 1 to be finite: upper bound found" 1 "cddce1" } } */
