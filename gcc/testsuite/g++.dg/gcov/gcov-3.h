/* Leave unused lines for at least the length of the including source file. */



















struct T {
  int i;
  T() { i = 0; }
};

T t;

int foo()
{
  return t.i;                          /* count(1) */
}
