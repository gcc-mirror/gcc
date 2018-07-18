// { dg-do run { target native } }
// { dg-options "-fprofile-arcs -ftest-coverage" }
void
test_swap(int& x, int& y) throw()
{
  int tmp = x;
  x = y;
  y = tmp;
}

int
main()
{
  int i = 5;
  int j = 7;
  test_swap(i, j);
  return 0;
}

/* { dg-final { cleanup-coverage-files } } */
