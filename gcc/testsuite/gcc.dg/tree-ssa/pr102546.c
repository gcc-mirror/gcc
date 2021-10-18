// { dg-do compile }
// { dg-options "-O3 -fdump-tree-optimized" }

static int a;
static char b, c, d;
void bar(void);
void foo(void);

int main() {
    int f = 0;
    for (; f <= 5; f++) {
        bar();
        b = b && f;
        d = f << f;
        if (!(a >= d || f))
            foo();
        c = 1;
        for (; c; c = 0)
            ;
    }
}

// { dg-final { scan-tree-dump-not "foo" "optimized" } }
