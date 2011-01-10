/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void f(unsigned char *s, unsigned char *d, int n) {
    int i;
    for (i = 0; i < n; i += 4) {
        d[i + 0] += s[i + 0];
        d[i + 1] += s[i + 1];
        d[i + 2] += s[i + 2];
        d[i + 3] += s[i + 3];
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail  { vect_no_align && ilp32 } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail { vect_no_align && ilp32 } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
