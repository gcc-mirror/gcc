/* { dg-do compile { target { weak_undefined } } } */
/* { dg-options "-O1 -fdump-tree-phiprop1-details" } */
/* { dg-add-options weak_undefined } */

/* PR tree-optimization/60183 */

unsigned char c;
extern unsigned char d __attribute__((weak));
int j = 2;

unsigned long
foo (void)
{
  unsigned char *y = &c;
  int i;
  unsigned w = 0;
  for (i = 0; i < j; i++)
    {
      w = *y;
      y = &d;
    }
  return w;
}
/* the load from d can trap so this should not cause a phiprop. */
/* { dg-final { scan-tree-dump-not "Removing dead stmt:" "phiprop1"} } */
/* { dg-final { scan-tree-dump-not "Inserting PHI for result of load" "phiprop1"} } */
