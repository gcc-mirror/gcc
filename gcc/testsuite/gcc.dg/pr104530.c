/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void foo(void);

static int a, *b = &a, c, d = 1;

int main() {
    c = 0 == b;
    a = *b;
    if (c % d)
        for (; d; --d)
            foo();
    b = 0;
}


/* { dg-final { scan-tree-dump-not "foo" "evrp" } }  */

