/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp2" } */

void foo(void);
static int a, b;
static int *c = &a, *d;
static unsigned e;
static short f;
static unsigned g(unsigned char h, char i) { return h + i; }
int main() {
    d = &a;
    int *j = d;
    e = -27;
    for (; e > 18; e = g(e, 6)) {
        a = 0;
        for (; a != -3; a--) {
            if (0 != a ^ *j)
                for (; b; b++) f = -f;
            else if (*c) {
                foo();
                break;
            }
            if (!(((e) >= 235) && ((e) <= 4294967269))) {
                __builtin_unreachable();
            }
            b = 0;
        }
    }
}


/* { dg-final { scan-tree-dump-not "foo" "vrp2" } } */


