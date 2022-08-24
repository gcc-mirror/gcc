/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp " } */

void foo();
static void __attribute__ ((noinline)) DCEMarker0_() {foo ();}

void f(bool s, bool c) {
    if ((!c == !s) && !c) {
        if (s) {
            DCEMarker0_();
        }
    }
}

// With equivalences, vrp should be able to remove all IFs.
/* { dg-final { scan-tree-dump-not "goto" "evrp" } } */
