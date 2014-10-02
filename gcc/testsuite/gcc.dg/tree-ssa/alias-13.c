/* { dg-do link } */
/* { dg-options "-O2" } */

void link_failure (void);

struct a
{
  char a1;
};

int *aa;

void g(int *a)
{
  aa = a;
  *a = 2;
}

int t(int i, struct a *b)
{
  g(&i);
  b->a1 = 1;
  i = 2;
  if (b->a1 != 1)
    link_failure ();
}
int main(void)
{
  struct a b;
  t(1, &b);
  return 0;
}


