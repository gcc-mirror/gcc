/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-pre" } */

int a, d;
struct {
  char b;
} c;
int main()
{
  int f;
  for (; d; d++)
    c.b = f = a ? c.b : 0;
  return 0;
}
