// Build don't link:

template <class T>
struct S1 {};

template <class T>
void f(T);

template <class C>
struct S2
{
  template <class T>
  void f<S1<T> >(T) {}  // ERROR - bad specialization.
};


template <class T>
struct S3
{
  friend class S2<T>;
};
