/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cfg" } */
extern void foo (void);
extern int i;
void
bar (void)
{
  switch (i)
    {
    case 0:
      foo ();
      break;
    case 1:
      break;
    }


  switch (i)
    {
    case 0:
      foo ();
      break;
    case 1:
      break;
    }
}
/* { dg-final { scan-tree-dump-times "case 1:" 0 "cfg" } } */
/* { dg-final { cleanup-tree-dump "cfg" } } */
