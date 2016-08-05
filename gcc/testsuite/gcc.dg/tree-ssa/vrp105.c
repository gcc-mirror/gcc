/* PR tree-optimization/18046  */
/* { dg-options "-O2 -fdump-tree-vrp2-details" }  */
/* { dg-final { scan-tree-dump-times "Threaded jump" 1 "vrp2" } }  */
/* In the 2nd VRP pass (after PRE) we expect to thread the default label of the
   1st switch straight to that of the 2nd switch.  */

extern void foo (void);
extern void bar (void);

extern int i;
void
test (void)
{
  switch (i)
    {
    case 0:
      foo ();
      break;
    case 1:
      bar ();
      break;
    default:
      break;
    }

  switch (i)
    {
    case 0:
      foo ();
      break;
    case 1:
      bar ();
      break;
    default:
      break;
    }
}
