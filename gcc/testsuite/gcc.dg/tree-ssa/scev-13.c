/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ldist" } */

int
foo (char *p, unsigned n)
{
  while(n--)
    {
      p[n]='A';
    }
  return 0;
}

/* Loop can be transformed into builtin memset since &p[n] is SCEV.  */
/* { dg-final { scan-tree-dump "builtin_memset" "ldist" } } */
