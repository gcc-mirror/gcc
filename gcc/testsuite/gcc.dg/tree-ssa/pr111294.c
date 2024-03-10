/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */

void foo(void);
static short a;
static int b, c, d;
static int *e, *f = &d;
static int **g = &e;
static unsigned char h;
static short(i)(short j, int k) { return j > k ?: j; }
static char l() {
    if (a) return b;
    return c;
}
int main() {
    b = 0;
    for (; b < 5; ++b)
        ;
    h = l();
    if (a ^ 3 >= i(h, 11))
        a = 0;
    else {
        *g = f;
        if (e == &d & b) {
            __builtin_unreachable();
        } else
            foo();
        ;
    }
}

/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
