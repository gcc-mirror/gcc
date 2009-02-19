static int
f (int x)
{
  static union 
    {
      int i;
    };
  int j = i;
  i = x;
  return j;
}
