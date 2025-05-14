/* { dg-do compile } */
/* { dg-options "-O -fno-tree-ccp -fno-tree-forwprop -fdump-tree-fre1-details" } */

/* From PR21608.  */

static inline char wrap(char f) { return f; }
char bar(char f)
{
        return wrap(f);
}

/* { dg-final { scan-tree-dump-not " = \\\(\[^)\]*\\\)" "fre1" } } */
