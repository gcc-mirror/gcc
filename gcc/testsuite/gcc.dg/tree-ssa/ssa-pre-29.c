/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-details" } */

void bark (void);
int flag, hoist, y, z;

void
foo (void)
{
  if (flag)
    y = hoist + 4;
  else
    flag = 888;
  z = hoist + 4;
  bark ();
}

/* We should see the partial redundancy of hoist + 4, not being confused
   about bark () possibly clobbering hoist.  */

/* { dg-final { scan-tree-dump "Replaced hoist" "pre" } } */
