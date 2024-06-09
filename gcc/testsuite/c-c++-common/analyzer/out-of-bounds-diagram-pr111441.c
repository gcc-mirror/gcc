/* Verify we don't ICE generating out-of-bounds diagram.  */

/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include "analyzer-decls.h"

void f() {
    int a[] = {3, 0, 3, 3, 0, 3, 40883};
    for (int c = 6; c; c--) {
        __analyzer_describe(0, a[c]);
        90 > c || a[c]; /* { dg-bogus "stack-based buffer over-read" } */
    }
}
int main() { f(); }

/* We don't care about the exact diagram, just that we don't ICE.  */

/* { dg-allow-blank-lines-in-output 1 } */
/* { dg-prune-output ".*" } */
