/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/110450 */
/* the ranger should be able to figure out that based on the
   unreachable part of d not being zero, *b is also never 0.
*/


void foo(void);
static int a = 1;
static int *b = &a, *c = &a;
static short d, e;
static signed char f = 11;
static signed char(g)(signed char h, int i) { return h << i; }
int main() {
    if (f) *c = g(0 >= a, 3);
    e = *c;
    d = e % f;
    if (d) {
        __builtin_unreachable();
    } else if (*b)
        foo();
    ;
}

/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
