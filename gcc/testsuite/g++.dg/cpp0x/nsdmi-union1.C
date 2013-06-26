// PR c++/52377
// { dg-do run { target c++11 } }

union Test
{
  int a{4};
};

union B
{
  int i = 42;
  double d;
  B() = default;
  B(double d): d(d) { }
};

int main()
{
  Test t;
  B b;
  B b2(4.2);

  if (t.a != 4 || b.i != 42 || b2.d != 4.2)
    __builtin_abort();
}
