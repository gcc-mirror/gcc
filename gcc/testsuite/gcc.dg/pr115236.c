/* { dg-do run } */
/* { dg-options "-O -fno-tree-fre" } */

int a, *b = &a;
int main()
{
  int *c, *volatile *d = &c;
  *d = b;
  if (c != &a)
    __builtin_abort();
  return 0;
}
