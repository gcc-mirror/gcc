// Build don't link:
// GROUPS passed templates
template <class T>
struct bar { 
  typedef typename T::baz baz;
};

template <class T>
void foo(T)
{
  bar<T>::baz(); // ERROR - T is int.
}

void foobar()
{
  foo(3);
}
