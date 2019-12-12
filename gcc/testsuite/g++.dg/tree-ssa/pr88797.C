/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-split-paths-details" } */


void use(unsigned);
bool f(unsigned x, unsigned y) {
    return x < 1111 + (y <= 2222);
}
void test_f(unsigned x, unsigned y) {
    for (unsigned i = 0; i < 3333; ++i)
        use(f(x++, y++));
}

/* { dg-final { scan-tree-dump-not "Duplicating join block" "split-paths" } } */
/* { dg-final { scan-tree-dump-times "Block . is a join that does not expose" 1 "split-paths" } } */

