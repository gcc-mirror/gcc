/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/115449 */

void setBit_un(unsigned char *a, int b) {
   unsigned char c = 0x1UL << b;
   *a &= ~c;
   *a |= c;
}

void setBit_sign(signed char *a, int b) {
   signed char c = 0x1UL << b;
   *a &= ~c;
   *a |= c;
}

void setBit(char *a, int b) {
   char c = 0x1UL << b;
   *a &= ~c;
   *a |= c;
}
/*
   All three should produce:
    _1 = 1 << b_4(D);
    c_5 = (cast) _1;
    _2 = *a_7(D);
    _3 = _2 | c_5;
    *a_7(D) = _3;
   Removing the `&~c` as we are matching `(~x & y) | x` -> `x | y`
   match pattern even with extra casts are being involved. */

/* { dg-final { scan-tree-dump-not "bit_not_expr, " "optimized" } } */
/* { dg-final { scan-tree-dump-not "bit_and_expr, " "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr, " 3 "optimized" } } */
