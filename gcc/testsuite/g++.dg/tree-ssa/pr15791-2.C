/* { dg-do link } */
/* { dg-options "" } */

void link_error ();
struct a {};
int main ()
{
  struct a b[2];
  if (&b[0] == &b[1])
    link_error ();
  return 0;
}

