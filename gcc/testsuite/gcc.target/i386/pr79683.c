/* { dg-do compile } */
/* { dg-options "-O3 -msse2 -fvect-cost-model=unlimited" } */

struct s {
    __INT64_TYPE__ a;
    __INT64_TYPE__ b;
};
void test(struct s __seg_gs *x) {
    x->a += 1;
    x->b -= 1;
}

/* We get the function vectorized, verify the load and store are
   address-space qualified.  */
/* { dg-final { scan-assembler-times "padd" 1 } } */
/* { dg-final { scan-assembler-times "%gs" 2 } } */
