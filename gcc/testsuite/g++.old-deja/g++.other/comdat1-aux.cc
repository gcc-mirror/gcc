inline int f ()
{
  static int i;
  return ++i;
}

int g ()
{
  return f();
}
