inline int f ()
{
  static int k;
  return ++k;
}

int g ()
{
  return f();
}
