/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error (void);


void test(int x)
{
  if ((x & ~x) != 0)
    link_error ();
  if ((~x & x) != 0)
    link_error ();

  if ((x ^ ~x) != -1)
    link_error ();
  if ((~x ^ x) != -1)
    link_error ();

  if ((x | ~x) != -1)
    link_error ();
  if ((~x | x) != -1)
    link_error ();

  if (x && !x)
    link_error ();
  if (!x && x)
    link_error ();

  if (!(x || !x))
    link_error ();
  if (!(!x || x))
    link_error ();
}

int main()
{
  test (0);
  test (1);
  test (-1);
  return 0;
}

