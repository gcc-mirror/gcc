/* { dg-options "-O2 -fdump-tree-vrp1" }  */
/* { dg-final { scan-tree-dump "case 2:" "vrp1" } }  */
/* { dg-final { scan-tree-dump "case 7 ... 8:" "vrp1" } }  */

extern void foo (void);
extern void bar (void);
extern void baz (void);

void
test (int i)
{
  if (i >= 2 && i <= 8)
  switch (i)
    {
    case 1: /* Redundant label.  */
    case 2:
      bar ();
      break;
    case 7:
    case 8:
    case 9: /* Redundant label.  */
      baz ();
      break;
    }
}
