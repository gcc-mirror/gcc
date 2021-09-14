/* { dg-do compile } */
/* { dg-options "-O2 -ftree-cselim -fdump-tree-cselim-details" } */

int c;
unsigned test(unsigned k, unsigned b) {
        unsigned a[2];
	a[k] = c;
        if (b < a[k]) {
                a[k] = b;
        }
        return a[0]+a[1];
}

/* { dg-final { scan-tree-dump "Conditional store replacement" "cselim" } } */
