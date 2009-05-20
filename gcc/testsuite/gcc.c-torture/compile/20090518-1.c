float
foo(int i)
{
  int j = i == 42;
  return *(float *)&j;
}
