/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

void zconfdump(void)
{
  char *p, *p2;
  for (p2 = p; p2; )
    {
      char __a0, __a1, __a2;
      __a0 = ((__const char *) ("\"\\"))[0];
      if (__a0)
        return;
    }
}

/* { dg-final { scan-tree-dump-not "goto" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
