/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sink" } */

int x;
void foo (int b)
{
  if (b)
    x = b;
  else
    x = 2;
}

/* We should have sunk the store and inserted a PHI to merge the
   stored values.  */

/* { dg-final { scan-tree-dump-times " = PHI" 1 "sink" } } */
/* { dg-final { scan-tree-dump-times "x = " 1 "sink" } } */
