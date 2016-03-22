// PR c++/70204
// { dg-do compile { target c++11 } }

int a;

template < int N, int I >
void fn1 ()
{
  const int x = I * a, y = x;
  fn1 < y, I > (); // { dg-error "constant|no match" }
}

int
main ()
{
  fn1 < 0, 0 > ();
  return 0;
}
