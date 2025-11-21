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
  for (int j = 0; j < *p; ++j)
    p = &b;
  return *p;
}
/* The weak load is unconditional due to the conditional so we can remove it unconditionally. */
/* { dg-final { scan-tree-dump "Removing dead stmt:" "phiprop1"} } */
/* { dg-final { scan-tree-dump "Inserting PHI for result of load" "phiprop1"} } */

