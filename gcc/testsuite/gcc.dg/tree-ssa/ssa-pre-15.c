/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Verify we PRE the strlen call, as strlen("") folds to zero.  */

extern __SIZE_TYPE__ strlen (const char *);

__SIZE_TYPE__ mystrlen (const char *s)
{
  if (!s)
    s = "";
  return strlen(s);
}

/* { dg-final { scan-tree-dump "= 0;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
