// Skip if not native
// Special g++ Options: -fprofile-arcs -ftest-coverage
void
swap(int& x, int& y) throw()
{
  int tmp = x;
  x = y;
  y = tmp;
}

main()
{
  int i = 5;
  int j = 7;
  swap(i, j);
}
