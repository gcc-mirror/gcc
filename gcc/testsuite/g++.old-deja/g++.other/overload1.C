struct A {
  A operator+ (int) const { return *this; }
};

A operator+ (A, float);

main ()
{
  A a;
  a + 1;
}
