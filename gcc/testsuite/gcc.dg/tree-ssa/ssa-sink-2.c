/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-sink-stats" } */
int
bar (int a, int b, int c)
{
  int y = a * b;
  if (c)
    y = 12;
  return y;
}
/* We should sink the x = a * b calculation into the else branch  */
/* { dg-final { scan-tree-dump-times "Sunk statements:1" 1 "sink" } } */
/* { dg-final { cleanup-tree-dump "sink" } } */
