/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

extern void foo (void);
extern void bar (void);
extern void baz (void);

/* Tests the ability to remove cases that are subranges.  */

void
test (int i)
{
  if (i < 0 || i > 45)
    return;
  if (i >= 7 && i <= 8)
    return;

  switch (i)
  {
    case 1:
      bar ();
      break;
    case 7 ... 8:
      foo ();
    case 14:
      baz ();
      break;
    default:
      break;
  }
}
/* { dg-final { scan-tree-dump-not "foo " "evrp" } } */
