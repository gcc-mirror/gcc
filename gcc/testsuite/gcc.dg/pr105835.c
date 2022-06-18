/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

void foo();

static int b;

static short a(short c, unsigned short d) { return c - d; }

int main() {
    int e = -(0 < b);
    if (a(1, e))
        b = 0;
    else
        foo();
}

/* { dg-final { scan-tree-dump-not "goto" "optimized" } } */
