/* { dg-do compile } */
/* { dg-additional-options "-O3 -mcpu=neoverse-v2 -fdump-tree-vect-all -std=c99" } */

#define iterations 100000
#define LEN_1D 32000

float a[LEN_1D];
float b[LEN_1D];

int main()
{    
    float dot;
    for (int nl = 0; nl < iterations*5; nl++) {
        dot = 0.0f;
        for (int i = 0; i < LEN_1D; i++) {
            dot += a[i] * b[i];
        }
    }
    
    return dot;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "OUTER LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "low throughput of per iteration due to splats" "vect" } } */
