/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

void bla(void);

void
foo(int c, int d)
{
  goto skip;

ebef:
  goto xxx;

skip:

  if (c)
    {
xxx:;
    if (!c)
      bla ();
    }
    
  if (d)
    goto ebef;
}

/* Bla should not be optimized away.  */
/* { dg-final { scan-tree-dump-times "bla" 1 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
