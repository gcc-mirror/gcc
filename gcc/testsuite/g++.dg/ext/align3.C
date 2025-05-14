// PR c++/117512

struct A {
  __attribute__((aligned)) int b;
  ~A ();
};
A foo (int);

void
bar ()
{
  A e = { 0 };
  A d = foo (0) = e;
}
