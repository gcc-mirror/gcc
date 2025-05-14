// PR c++/117512

struct __attribute__((aligned (2 * sizeof (int)))) A {
  int b;
  ~A ();
};
A foo (int);

void
bar ()
{
  A e = { 0 };
  A d = foo (0) = e;
}
