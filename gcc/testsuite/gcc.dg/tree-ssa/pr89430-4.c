/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cselim-details" } */

int *p;
unsigned test(unsigned k, unsigned b) {
        unsigned a[2];
	p = a;
        if (b < a[k]) {
                a[k] = b;
        }
        return a[0]+a[1];
}

/* { dg-final { scan-tree-dump-not "Conditional store replacement" "cselim" } } */
