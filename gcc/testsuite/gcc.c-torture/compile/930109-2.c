int
f(int r)
{
  int i;
  for (i = 0; i < 2; i++)
    {
      r+= (4 >> i*2);
      r+= (2 >> i*2);
      r+= (1 >> i*2);
    }
  return r;
}
