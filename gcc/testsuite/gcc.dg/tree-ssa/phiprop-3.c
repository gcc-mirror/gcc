/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiprop1-details" } */

/* PR tree-optimization/60183 */

unsigned char c;
extern unsigned char d;
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
/* the load from c/d does not trap so we should able to remove the phi.  */
/* { dg-final { scan-tree-dump "Removing dead stmt:" "phiprop1"} } */
/* { dg-final { scan-tree-dump "Inserting PHI for result of load" "phiprop1"} } */
