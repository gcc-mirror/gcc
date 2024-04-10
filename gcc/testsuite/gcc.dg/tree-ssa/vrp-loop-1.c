/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */
/* PR tree-optimization/110768 */
/* The call to foo should be able to removed,
   The branch to unreachable is unreachable as
   VRP (ranger) figure out that c there can only
   be -20409 or 0. before r14-5109-ga291237b628f41
   ranger could not figure that out.  */
   

void foo(void);
static int a, b;
int main() {
    {
        short c = 45127;
        signed char d;
        b = 0;
        for (; b <= 3; b++) {
            if (b) continue;
            d = 0;
            for (; d <= 100; d++) {
                if (!(((c) >= -20409) && ((c) <= 1))) {
                    __builtin_unreachable();
                }
                if (~(0 == a) & 1) return b;
                c = 0;
                for (; c <= 0; c++) a = 3;
            }
        }
        foo();
    }
}

/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
