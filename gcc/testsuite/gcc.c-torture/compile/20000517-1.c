void test2 (int*, int, int, int);

void test ()
{
  int l;

  test2 (0, 0, 0, 0);
  test2 (&l, 0, 0, 0);
}
