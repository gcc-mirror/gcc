/* { dg-do compile } */
/* { dg-options "-mavx2 -Ofast -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 1 "vect" } } */

typedef unsigned long long uint64_t;
uint64_t d[512];
float f[1024];

void foo() {
    for (int i=0; i<512; ++i) {
        uint64_t k = d[i];
        f[i]=(k & 0x3F30FFFF);
    }
}

