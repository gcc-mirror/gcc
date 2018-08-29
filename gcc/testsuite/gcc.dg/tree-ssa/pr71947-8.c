/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -fdump-tree-dom-details" } */


int f(int x, int y)
{
  int ret;
  if (x == y)
    ret = x / y;
  else
    ret = 0;

  return ret;
}
/* { dg-final { scan-tree-dump "Replaced redundant expr \[^\r\n\]* with .1."  "dom2" } } */


