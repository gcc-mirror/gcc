/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cselim-details" } */

int test(int b, int k) {
    typedef struct {
	    int x;
    } SS;
    struct {
        SS data[2];
    } a;

    if (b < a.data[k].x) {
        a.data[k].x = b;
    }

    return a.data[0].x + a.data[1].x;
}

/* { dg-final { scan-tree-dump "Conditional store replacement" "cselim" } } */
