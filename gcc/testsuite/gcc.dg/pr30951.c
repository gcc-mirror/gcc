/* { dg-do link } */

extern void link_error (void);

void test (int x, unsigned int y)
{
  if (x + 5 == x)
    link_error ();
  if (x == x + 10)
    link_error ();
  if (y + 5 == y)
    link_error ();
  if (y == y + 10)
    link_error ();
  if (x + 5 != x)
    ;
  else
    link_error ();
  if (x != x + 10)
    ;
  else
    link_error ();
  if (y + 5 != y)
    ;
  else
    link_error ();
  if (y != y + 10)
    ;
  else
    link_error ();
}

int main()
{
  return 0;
}
