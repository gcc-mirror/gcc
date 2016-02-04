// PR c++/69000
// { dg-do compile }
// { dg-require-effective-target tls }

class A {};

template <typename T>
struct B
{
  static int *& foo () { static __thread int *c = 0; return c; }
};

B<A> d;

void
bar ()
{
  d.foo ();
}
