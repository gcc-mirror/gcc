// This testcase used to cause a crash on the Alpha.
// Special g++ Options: 
// Build don't link:

struct A {
  int i;
  ~A() { }
};

A f (int n)
{
  A a[n];
}
