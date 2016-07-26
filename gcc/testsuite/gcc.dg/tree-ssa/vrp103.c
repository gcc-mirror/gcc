/* PR tree-optimization/18046  */
/* { dg-options "-O2 -fdump-tree-vrp" }  */
/* { dg-final { scan-tree-dump-times "baz \\(0\\);" 4 "vrp1" } }  */

void foo (void);
void bar (void);
void baz (int);

void
test (int i)
{
  switch (i)
    {
    case 1:
    case 2:
    case 3:
      foo ();
      break;
    case 5:
      bar ();
      break;
    default:
      /* These tests should be folded to 0, resulting in 4 calls of baz(0).  */
      baz (i == 1);
      baz (i == 2);
      baz (i == 3);
      baz (i == 5);
      break;
    }
}
