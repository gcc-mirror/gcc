// Build don't link:

template <class T>
struct S
{
  typedef T S_Type;
};


template <class T>
void foo(typename S<T>::S_Type)
{
}


template <class T>
void foo(T)
{
}


struct S2 {};

void bar()
{
  foo(S2()); // We can't unify with the first foo, so we get the second.
}
