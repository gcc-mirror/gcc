void b (int *);
void c (int, int);
void d (int);

int e;

void a (int x, int y)
{
  int f = x ? e : 0;
  int z = y;

  b (&y);
  c (z, y);
  d (f);
}

void b (int *y)
{
  (*y)++;
}

void c (int x, int y)
{
  if (x == y)
    abort ();
}

void d (int x)
{
}

int main (void)
{
  a (0, 0);
  exit (0);
}
