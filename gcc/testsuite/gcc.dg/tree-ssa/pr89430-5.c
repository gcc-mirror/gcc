/* { dg-do compile } */
/* { dg-options "-O2 -ftree-cselim -fdump-tree-cselim-details" } */

int test(int b, int k) {
    struct {
        int data[2];
    } a;

    if (b < a.data[k]) {
        a.data[k] = b;
    }

    return a.data[0] + a.data[1];
}

/* { dg-final { scan-tree-dump "Conditional store replacement" "cselim" } } */
