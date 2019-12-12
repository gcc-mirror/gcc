/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cselim-details" } */

unsigned a[2];
unsigned test(unsigned k, unsigned b) {
        if (b < a[k]) {
                a[k] = b;
        }
        return a[0]+a[1];
}

/* { dg-final { scan-tree-dump-not "Conditional store replacement" "cselim" } } */
