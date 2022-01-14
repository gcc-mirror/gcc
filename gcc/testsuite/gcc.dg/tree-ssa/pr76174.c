/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

void foo();

void l(unsigned int r) {
  	unsigned int q = 0;
        unsigned int c = r;
        for (unsigned int x = 0; x<r; x++) {
            if (q == c) {
                foo();
                c *= 2;
            }
            q++;
        }
}

/* We should be able to elide the body of the function by means of
   figuring out the equality between the two IVs and then simplifying
   the q == c test.  */
/* { dg-final { scan-tree-dump-not "foo" "cddce1" } } */
/* { dg-final { scan-tree-dump-times "bb" 1 "cddce1" } } */
