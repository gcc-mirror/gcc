/* { dg-options "-O2 -fdump-tree-vrp1" }  */
/* { dg-final { scan-tree-dump "case 1:" "vrp1" } }  */
/* { dg-final { scan-tree-dump "case 9:" "vrp1" } }  */

extern void foo (void);
extern void bar (void);
extern void baz (void);

void
test (int i)
{
  if (i < 2 || i > 8)
  switch (i)
    {
    case 1:
    case 2: /* Redundant label.  */
      bar ();
      break;
    case 7: /* Redundant label.  */
    case 8: /* Redundant label.  */
    case 9:
      baz ();
      break;
    }
}
