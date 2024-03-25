/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp2 -fdump-tree-optimized -fno-ipa-vrp" } */

void foo(void);
static int a = 1, c;
static int *b = &a;
static int **d = &b;
static int ***e = &d;
void __assert_fail() __attribute__((__noreturn__));
static int f() {
    if (a) return a;
    for (; c;) *e = 0;
    if (b) __assert_fail();
    return 6;
}
int main() {
    if (f()) {
        *d = 0;
        if (b == 0)
            ;
        else {
            __builtin_unreachable();
            __assert_fail();
        }
    }
    if (b == 0)
        ;
    else
        foo();
    ;
}

/* { dg-final { scan-tree-dump-times "Folding predicate" 2 "ccp2" } } */
/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
