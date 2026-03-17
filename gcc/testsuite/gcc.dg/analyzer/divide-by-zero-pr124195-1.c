short s;

int
foo()
{
  s %= (0, 0);
  return s > 0;
}
