/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre-details" } */

/* From PR21608.  */

static inline char wrap(char f) { return f; }
char bar(char f)
{
        return wrap(f);
}

/* { dg-final { scan-tree-dump "Replaced \\\(char\\\) .*with " "fre" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "Replaced \\\(int\\\) .*with " "fre" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
