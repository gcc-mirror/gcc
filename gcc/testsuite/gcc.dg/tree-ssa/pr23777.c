/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern void f(char *const *);
void g (char **o)
{
  static const char *const multilib_exclusions_raw[] = { 0 };
  const char *const *q = multilib_exclusions_raw;

  f (o);
  while (*q++)
    f (o);
}

/* The last DCE pass is able to remove the load from
    multilib_exclusions_raw.  */

/* { dg-final { scan-tree-dump-not "multilib_exclusions_raw" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
