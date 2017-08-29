/* PR tree-optimization/18046  */
/* { dg-options "-O2 -fdump-tree-optimized" }  */
/* { dg-final { scan-tree-dump-times "switch" 1 "switchlower" } }  */

void foo (void);
void bar (void);
void baz (void);

void
test (int i)
{
  switch (i)
    {
    case 1:
      foo ();
      break;
    case 2:
      bar ();
      break;
    default:
      break;
    }

  /* This switch should be gone after threading/VRP.  */
  switch (i)
    {
    case 1:
      foo ();
      break;
    case 2:
      baz ();
      break;
    default:
      break;
    }
}
