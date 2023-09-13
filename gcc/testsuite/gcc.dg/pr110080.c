/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void foo(void);
static unsigned char a = 131;
static int *b;
static int **c = &b;
static void d(int e, unsigned f) {
    int *g;
    if (f != 131) {
        __builtin_unreachable();
    }
    if (!e){
        for (; a; ++a)
            for (e = 0; 0;)
                ;
        g = &e;
        int **h = &g;
        if (**h) {
            foo();
        }
    }
    *c = &e;
}
int main() { d(4 & a, a); }

/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
