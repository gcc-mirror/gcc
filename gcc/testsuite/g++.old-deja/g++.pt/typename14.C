// Build don't link:
// Special g++ Options:

template <class T>
struct B {
  typedef T X;
};

template <class T>
struct S : public B<T>
{
  struct I {
    void f(X x);
  };
};
