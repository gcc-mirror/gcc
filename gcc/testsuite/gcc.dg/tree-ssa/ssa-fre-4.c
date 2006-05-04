/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre-details" } */

/* From PR21608.  */

#define bool _Bool
static inline bool wrap(bool f) { return f; }
bool bar(bool f)
{
        return wrap(f);
}

/* { dg-final { scan-tree-dump "Replaced \\\(_Bool\\\) D.*with f_" "fre" } } */
/* { dg-final { scan-tree-dump "Replaced \\\(int\\\) f_.*with D" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
