/* { dg-do run } */

int a = 3, d, e;
int *b = &a;
char c;
short f;
const int **g;
static long h(int **i, int **j)
{
  const int *k[46];
  const int **l = &k[5];
  *j = &e;
  g = l;
  for (; d; d = d + 1)
    ;
  **i = 0;
  return f;
}
int main()
{
  int *m = &a;
  h(&m, &m);
  c = *b;
  if (c != 3)
    __builtin_abort ();
}
