/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-pre-stats -fdump-tree-optimized" } */

void foo(void);
static int c = 76, f, g;
static int *h, *j, *k = &g;
static int **i = &h;
static short a;
static signed char(l)(signed char b) {
    if (!(((b) >= 77) && ((b) <= 77))) {
        __builtin_unreachable();
    }
    return 0;
}
static short(m)(short d, short e) { return d + e; }
static short n(signed char) {
    j = *i;
    if (j == 0)
        ;
    else
        *i = 0;
    *k = 0;
    return 0;
}
static signed char o() {
    l(0);
    return 0;
}
static signed char p(int ad) {
    a = m(!0, ad);
    l(a);
    if (f) {
        *i &&n(o());
        *i = 0;
    } else
        n(0);
    if (h == &f || h == 0)
        ;
    else
        foo();
    return 0;
}
int main() {
    p(c);
    c = 8;
}

/* Even with main being cold we should optimize the redundant load of h
   which is available on all incoming edges (but none considered worth
   optimizing for speed) when doing that doesn't needlessly increase
   code size.  */

/* { dg-final { scan-tree-dump "Insertions: 1" "pre" } } */
/* { dg-final { scan-tree-dump "HOIST inserted: 1" "pre" } } */
/* { dg-final { scan-tree-dump "Eliminated: 3" "pre" } } */
/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
