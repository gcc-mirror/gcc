/* { dg-do compile } */
/* { dg-additional-options "-O3 -mcpu=neoverse-v2 -fdump-tree-vect-all -std=c99" } */

#define iterations 100000
#define LEN_1D 32000

float a[LEN_1D];

int main()
{
    float x;
    for (int nl = 0; nl < iterations; nl++) {
        x = a[0];
        for (int i = 0; i < LEN_1D; ++i) {
            if (a[i] > x) {
                x = a[i];
            }
        }
    }

    return x > 1;
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "OUTER LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "low throughput of per iteration due to splats" "vect" } } */
