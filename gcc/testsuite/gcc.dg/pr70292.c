/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O1 -fno-inline -fno-dce -fschedule-insns -fselective-scheduling -fno-tree-dce" } */

void bar() {}

int t106_1mul(unsigned int x, unsigned int y) {
    int r;
    if (__builtin_mul_overflow(x, y, &r)) {
        bar();
    }
    return r;
}
