struct A {
  A operator+ (int) const { return *this; }
};

A operator+ (A, float);

int main ()
{
  A a;
  a + 1;
}
