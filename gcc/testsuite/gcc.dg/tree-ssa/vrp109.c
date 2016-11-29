/* { dg-options "-O2 -fdump-tree-vrp1" }  */
/* { dg-final { scan-tree-dump "case 9 ... 10:" "vrp1" } }  */
/* { dg-final { scan-tree-dump "case 17 ... 18:" "vrp1" } }  */
/* { dg-final { scan-tree-dump "case 27 ... 30:" "vrp1" } }  */

extern void foo (void);
extern void bar (void);

void
test1 (int i)
{
  if (i != 7 && i != 8)
    switch (i)
      {
      case 1:
      case 2:
        foo ();
        break;
      case 7: /* Redundant label.  */
      case 8: /* Redundant label.  */
      case 9:
      case 10:
        bar ();
        break;
      }
}

void
test3 (int i)
{
  if (i != 19 && i != 20)
    switch (i)
      {
      case 1:
      case 2:
        foo ();
        break;
      case 17:
      case 18:
      case 19: /* Redundant label.  */
      case 20: /* Redundant label.  */
        bar ();
        break;
      }
}

void
test2 (int i)
{
  if (i != 28 && i != 29)
    switch (i)
      {
      case 1:
      case 2:
        foo ();
        break;
      /* No redundancy.  */
      case 27:
      case 28:
      case 29:
      case 30:
        bar ();
        break;
      }
}
