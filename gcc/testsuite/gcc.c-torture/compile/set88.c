foo (a)
{
  return -1 << a;
}

bar (a, b)
{
  return b | (-1 << a);
}
