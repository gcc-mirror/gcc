struct a
{
  struct a * x;
};

void
foo (struct a * b)
{
  int i;

  for (i = 0; i < 1000; i++)
    {
      b->x = b;
      b++;
    }
}

void
bar (struct a * b)
{
  int i;

  for (i = 0; i < 1000; i++)
    {
      b->x = b;
      b--;
    }
}
