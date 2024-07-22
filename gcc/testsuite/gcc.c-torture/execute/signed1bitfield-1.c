/* PR tree-optimization/115154 */
/* This was being miscompiled to `(signed:1)(t*5)`
   being transformed into `-((signed:1)t)` which is undefined.
   Note there is a pattern which removes the negative in some cases
   which works around the issue.  */

struct {
  signed b : 1;
} f;
int i = 55;
__attribute__((noinline))
void check(int a)
{
        if (!a)
        __builtin_abort();
}
int main() {
    int t = i != 5;
    t = t*5;
    f.b = t;
    int tt = f.b;
    check(f.b);
}
