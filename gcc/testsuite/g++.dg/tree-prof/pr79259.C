/* { dg-options "-O1" } */

inline bool
a (int b)
{
  return (b & 5) != b;
}
int c;
int
fn2 ()
{
  if (a (c == 0))
    return 0;
}

int main()
{
  fn2();
}

