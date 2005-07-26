/* { do-go compile } */
/* { dg-options "-fdump-tree-gimple" } */

typedef struct { int i; } Foo;
Foo foo;
Foo *bar(void)
{
  return (Foo *)&foo.i;
}

/* { dg-final { scan-tree-dump "&foo;" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
