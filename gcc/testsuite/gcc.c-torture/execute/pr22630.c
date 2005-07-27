void abort (void);

int j;

void bla (int *r)
{
  int *p, *q;

  p = q = r;
  if (!p)
    p = &j;
  
  if (p != q)
    j = 1;
}

int main (void)
{
  bla (0);
  if (!j)
    abort ();
  return 0;
}
