/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre-details" } */

/* From PR21608.  */

#define bool _Bool
static inline bool wrap(bool f) { return f; }
bool bar(bool f)
{
        return wrap(f);
}

/* { dg-final { scan-tree-dump "Replaced \\\(_Bool\\\) .*with " "fre" } } */
/* { dg-final { scan-tree-dump "Replaced \\\(int\\\) .*with " "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
