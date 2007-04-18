struct foo { int i; int j; };

int bar (struct foo *k, int k2, int f, int f2)
{
  int *p, *q;
  int res;
  if (f)
    p = &k->i;
  else
    p = &k->j;
  res = *p;
  k->i = 1;
  if (f2)
    q = p;
  else
    q = &k2;
  return res + *q;
}

extern void abort (void);

int main()
{
  struct foo k;
  k.i = 0;
  k.j = 1;
  if (bar (&k, 1, 1, 1) != 1)
    abort ();
  return 0;
}
