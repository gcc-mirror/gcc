foo (a, b)
{
  b++;
  if (a <= b)
    if ((int) a < (int) b)
      b--;
    else
      b++;
  return b;
}
