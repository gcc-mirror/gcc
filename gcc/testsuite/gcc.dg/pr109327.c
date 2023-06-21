/* { dg-do compile } */
/* { dg-options "-O -fno-tree-ccp" } */

int a;
void b(int c) {}
int main()
{
  int d = 0, *e = &a;
  if (d) {
    int *f = e;
    while (a)
      b(e != f);
  }
  return 0;
}
