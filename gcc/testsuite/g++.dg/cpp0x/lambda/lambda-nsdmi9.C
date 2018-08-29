// PR c++/84708
// { dg-do run { target c++11 } }

int main()
{
  struct Z
  {
    int i;
    int b = ([&] { return i; }());
    Z(int i): i(i) {}
  } z (42);

  if (z.b != 42)
    __builtin_abort ();
}
