/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zba_zbb -mabi=lp64d -mrvv-max-lmul=dynamic -O3 -fdump-tree-vect-details" } */

typedef float real_t;
__attribute__((aligned(64))) real_t a[32000];
real_t s315()
{
    for (int i = 0; i < 32000; i++)
        a[i] = (i * 7) % 32000;
    real_t x, chksum;
    int index;
    for (int nl = 0; nl < 256; nl++) {
        x = a[0];
        index = 0;
        for (int i = 0; i < 32000; ++i) {
            if (a[i] > x) {
                x = a[i];
                index = i;
            }
        }
        chksum = x + (real_t) index;
    }
    return index + x + 1;
}

/* { dg-final { scan-assembler {e32,m4} } } */
/* { dg-final { scan-assembler-not {e32,m8} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-tree-dump-times "Preferring smaller LMUL loop because it has unexpected spills" 1 "vect" } } */
