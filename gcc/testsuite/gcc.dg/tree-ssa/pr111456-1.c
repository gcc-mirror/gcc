/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/111456 */

void foo(void);
static int i;
static int *j = &i;
static char l;
static void(a)(char) {}
static short(b)(short c, short d) { return c - d; }
static short(e)(short f, int g) {
    return f < 0 || g < 0 || g >= 32 ? f : f >> g;
}
static short(h)(short f, int g) { return g >= 2 ?: f >> g; }
static char k(char m, short n) {
    short o;
    int *p = &i;
    if (!(((m) >= 1) && ((m) <= 1))) {
        __builtin_unreachable();
    }
    o = e(i, i);
    if (h(1, o))
        ;
    else {
        m = 0;
        for (; m >= -20; m = b(m, 9))
            if (a(i), n) {
                if (*p)
                    ;
                else
                    foo();
                ;
            } else
                return l;
    }
    return i;
}
int main() { k(0 <= 0 > *j, i); }


/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */

