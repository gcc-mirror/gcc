void *g, *c;
int a, b;

int f()
{
  if ((0 == a) != (b || g == c))
    return 1;
  return 0;
}

