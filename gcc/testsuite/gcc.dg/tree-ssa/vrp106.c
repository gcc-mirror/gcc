/* PR tree-optimization/18046  */
/* { dg-options "-O2 -fdump-tree-ethread-details" }  */
/* { dg-final { scan-tree-dump-times "Registering jump thread" 1 "ethread" } }  */
/* During VRP we expect to thread the true arm of the conditional through the switch
   and to the BB that corresponds to the 7 ... 9 case label.  */
extern void foo (void);
extern void bar (void);
extern void baz (void);

void
test (int i)
{
  if (i >= 7 && i <= 8)
    foo ();

  switch (i)
  {
    case 1:
      bar ();
      break;
    case 7:
    case 8:
    case 9:
      baz ();
      break;
  }
}
