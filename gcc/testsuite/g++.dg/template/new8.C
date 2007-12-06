// PR c++/34336
// { dg-do compile }

struct A;

template <class T>
struct S
{
  T *m;
  T &operator* () { return *m; }
};

struct B
{
  B (const A &);
};

template <class T>
struct C
{
  C ();
  S<A> c;
};

template <class T>
C<T>::C ()
{
  B *b = new B (*c);
}
