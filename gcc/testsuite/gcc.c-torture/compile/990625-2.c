void
broken030599(int *n)
{
  int i, x;
  for (i = 0; i < 32; i++) {
    x=0;
    x++;
    if (i & 4)
      x++;
    x++;
  }
}
