static int foo(void)
{
  static int counter;
  return counter++;
}
