// { dg-do compile }

struct A {};

template <class T> struct B
{
  T a, b;
  B() {}
  B(T x, T y) : a(x), b(y) {}
  template <class U> operator B<U> () const
  { return B<U>((U)(this->a), (U)(this->b)); }
};

template <class T> struct C : public B<int>
{
  T *c;
  inline T & operator *() { return *c; }
};

template <class T> struct D : virtual public C<T> { };

void
foo (D<A> x)
{
  *x;
}
