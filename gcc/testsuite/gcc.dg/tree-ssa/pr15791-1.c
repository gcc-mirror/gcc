/* { dg-do link } */

void link_error ();

int main ()
{
  struct { int b[2]; } x;
  int b[2];
  if (&b[1] != &b[1])
    link_error ();
  if (&b[0] != b)
    link_error ();
  if (b == &b[2])
    link_error ();
  if (b != b)
    link_error ();
  if (&x.b[1] == &x.b[0])
    link_error ();
  if (x.b != &x.b[0])
    link_error ();
  if (&x.b[1] == x.b)
    link_error ();
  return 0;
}

