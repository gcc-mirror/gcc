/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* PR tree-optimization/105832 */

void foo(void);

static struct {
    short a;
    signed char b;
} c;

static signed char d;

int main() {
    signed char g = c.b > 4U ? c.b : c.b << 2;
    for (int h = 0; h < 5; h++) {
        d = (g >= 2 || 1 >> g) ? g : g << 1;
        if (d && 1 == g)
            foo();
        c.a = 0;
    }
}

/* The call of foo should have been removed. */
/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
