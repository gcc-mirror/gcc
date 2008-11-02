int foo (int i, int b)
{
  int mask;
  int result;
  if (b)
    mask = -1;
  else
    mask = 0;
  result = i + 1;
  result = result & mask;
  return result;
}
