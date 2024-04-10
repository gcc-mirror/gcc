/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/110841  */
/* The call to foo should be able to removed */

void foo(void);
static int b, c, d;
static signed char(a)(signed char e, signed char f) { return e - f; }
int main() {
    for (; b <= 4; b++)
        ;
    c = 0;
    for (; c >= -16; c = a(c, 4))
        ;
    signed char g = b;
    for (; d <= 0; d++) {
        if (!(((g) >= 5) && ((g) <= 5))) {
            __builtin_unreachable();
        }
        if (c) return 0;
        g = 0;
        for (;;) {
            foo();
            break;
        }
    }
}

/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
