int foo (double x, double y)
{
  return !__builtin_isunordered (x, y);
}
