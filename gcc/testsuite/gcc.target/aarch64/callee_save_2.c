/* { dg-options "-O2 -fomit-frame-pointer" } */

int test (int x), test2 (int x);

int foo (int x, int y) {
    test (x);
    int lhs = test2 (y);
    return x + lhs;
}

/* { dg-final { scan-assembler {\tstp\tx30, x19, \[sp,} } } */
/* { dg-final { scan-assembler {\tldp\tx30, x19, \[sp\],} } } */
/* { dg-final { scan-assembler {\tstr\tw1, \[sp,} } } */
/* { dg-final { scan-assembler {\tldr\tw0, \[sp,} } } */
