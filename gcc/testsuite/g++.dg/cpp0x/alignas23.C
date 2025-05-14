// PR c++/117512
// { dg-do compile { target c++11 } }

struct A {
  alignas(sizeof (long long)) int b;
  ~A ();
};
A foo (int);

void
bar ()
{
  A e = { 0 };
  A d = foo (0) = e;
}
