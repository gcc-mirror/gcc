a (c)
     unsigned char c;
{
  unsigned u = c;
  if ((int)u < 0)
    return 1;
  else
    return 0;
}

b (x, y)
     unsigned x, y;
{
  x /= y;
  if ((int)x < 0)
    return 1;
  else
    return 0;
}
