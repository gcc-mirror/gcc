// PRMS Id: 4656
// Testcase for use of member pointers in template resolution

template <class T> class A {
 public:
  A() : a(1) {}
  T a;
};

template <class T>
int foo (T A<int>::*p)
{
  return 0;
}
int main()
{
  int A<int>::*pm = &A<int>::a;	// gets bogus error - failed temp resolution
  foo (pm);
  return 0;
}
