// Test whether exception specifier dependent on template parameter
// is accepted during template decl processing.
// { dg-do run }

extern "C" void abort();

class A {};

template <class T>
struct B
{
  typedef A E;
};

template <class T>
struct C
{
  typedef B<T> D;
  typedef typename D::E E;
  void f() throw(E) { throw E(); }
};

int main()
{
  int caught = 0;
  try
    {
      C<int> x;
      x.f();
    }
  catch (A)
    {
      ++caught;
    }
  if (caught != 1)
    abort ();
  return 0;
}
