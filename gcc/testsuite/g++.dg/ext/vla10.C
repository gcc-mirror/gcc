// PR c++/48446
// { dg-options "" }

template<typename T>
struct A
{
  ~A ();
  T *operator-> () const;
};

struct B
{
  typedef A <B> P;
  static P foo (int);
};

struct C
{
  typedef A<C> P;
  static const int c = 80;
};

C::P bar ();

void
baz ()
{
  char z[bar ()->c];
  {
    B::P m = B::foo (sizeof (z));
  }
}
