// PR c++/70285

int a;

struct S
{
  int i:8;
} b;

int
fn1 (bool x)
{
  (&fn1 ? b.i : a) = 42;
  return (&fn1 ? b.i : a);
}
