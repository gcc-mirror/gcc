/* { dg-options "-O2" } */

int test (int x), test2 (int x);

int foo (int x, int y) {
    test (x);
    int lhs = test2 (y);
    return x + lhs;
}

/* { dg-final { scan-assembler {\tstp\tx19, x20, \[sp,} } } */
/* { dg-final { scan-assembler {\tldp\tx19, x20, \[sp,} } } */
