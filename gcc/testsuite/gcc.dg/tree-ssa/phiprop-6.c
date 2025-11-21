/* { dg-do compile { target { weak_undefined } } } */
/* { dg-options "-O2 -fdump-tree-phiprop-details" } */
/* { dg-add-options weak_undefined } */

/* PR tree-optimization/116835 */

extern int a __attribute__((weak));
int b;

int 
bar (int c)
{
  int *p = &a;
  for (int j = 0; j < c; ++j)
    p = &b;
  return *p;
}
/* The weak load is conditional with the loop so we cannot make it unconditional.  */
/* { dg-final { scan-tree-dump-not "Removing dead stmt:" "phiprop1"} } */
/* { dg-final { scan-tree-dump-not "Inserting PHI for result of load" "phiprop1"} } */

