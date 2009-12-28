// PR c++/41305
// We got into infinite recursion instantiating the B<U> series.

template <class T> struct A { };
template <class T, class U = A<T> > struct B;
template <class T> struct C { };

template <class T, class U> struct B: C<B<U> >
{
  friend void f(B) { }
};

B<int> b;

int main()
{
  f(b);
}
