/* { dg-do run } */

int a = 1, b, c, *d = &a, *e = &a, f;
void g(int h) {}
void k(int *l)
{
  int ***j;
  if (c)
    {
      *j = &l;
      ***j;
    }
  g(*l);
  *e = f;
  if (*l)
    {
      int i = b / a;
      a = i;
    }
}
int main()
{
  k(d);
  return 0;
}
