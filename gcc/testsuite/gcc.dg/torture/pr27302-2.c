/* { dg-do run } */
/* { dg-options "-fstrict-overflow" } */

extern void link_error (void);

void test0 (int a, int b)
{
  if ((a < b) != (b > a))
    link_error ();

  if ((a - 1 < b) != (a <= b))
    link_error ();
  if ((a - 2 < b) != (a - 1 <= b))
    link_error ();
  if ((a + -1 < b) != (a <= b))
    link_error ();
  if ((a + -2 < b) != (a + -1 <= b))
    link_error ();

  if ((a + 1 > b) != (a >= b))
    link_error ();
  if ((a + 2 > b) != (a + 1 >= b))
    link_error ();
  if ((a - -1 > b) != (a >= b))
    link_error ();
  if ((a - -2 > b) != (a - -1 >= b))
    link_error ();

  if ((a + 1 <= b) != (a < b))
    link_error ();
  if ((a + 2 <= b) != (a + 1 < b))
    link_error ();
  if ((a - -1 <= b) != (a < b))
    link_error ();
  if ((a - -2 <= b) != (a - -1 < b))
    link_error ();

  if ((a - 1 >= b) != (a > b))
    link_error ();
  if ((a - 2 >= b) != (a - 1 > b))
    link_error ();
  if ((a + -1 >= b) != (a > b))
    link_error ();
  if ((a + -2 >= b) != (a + -1 > b))
    link_error ();
}

int main()
{
  test0 (1, 2);
  return 0;
}
