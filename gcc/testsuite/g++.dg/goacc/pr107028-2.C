// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }

#include <cstdlib>

typedef float real_t;

struct foo {
  real_t *data;
};

#define n 1024

int test3() {
    real_t *a = (real_t *)malloc(n * sizeof(real_t));
    struct foo b;
    b.data = (real_t *)malloc(n * sizeof(real_t));

    #pragma acc data copyin(a[0:n], b, b.data[0:n])
// { dg-final { scan-tree-dump {map\(to:\*_[0-9]+ \[len: [0-9]+\]\) map\(attach:b.data \[bias: 0\]\) map\(to:b \[len: [0-9]+\]\) map\(to:\*a \[len: [0-9]+\]\)} "gimple" } }
    { }

    free (b.data);
    free (a);

    return 0;
}
