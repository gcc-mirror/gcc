/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -fdump-tree-dom-details" } */

int f(int x, int y)
{
  int ret = 10;
  if (x == y)
    ret = x  -  y;
  return ret;
}

/* { dg-final { scan-tree-dump "Replaced redundant expr \[^\r\n\]* with .0."  "dom2" } } */

