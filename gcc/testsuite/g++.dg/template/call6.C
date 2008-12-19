// PR c++/38577
// { dg-do compile }

struct A
{
  static A *bar ();
};

struct B : public A
{
  static void baz ();
};

template <class T>
void foo ()
{
  (static_cast<B *> (A::bar ()))->baz ();
}

void
bar ()
{
  foo<int> ();
}
