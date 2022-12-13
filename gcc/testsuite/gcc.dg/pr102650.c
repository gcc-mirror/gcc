/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-vrp1" } */

static int a = 2, b, c, d;
void foo(void);
int main() {
    short e;
    int f = -1;
    if (b)
        c = 0;
    c || (f = 2);
    for (; d < 1; d++)
        e = f + a;
    if (!e)
        foo();
    return 0;
}

/* { dg-final { scan-tree-dump-not "foo" "vrp1" } } */

