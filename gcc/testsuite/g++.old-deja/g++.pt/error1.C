// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S
{
  template <class U>
  void f ();
  
};

template <class T>
template <class U>
void S<T>::f ()
{
  U& u; // { dg-error "" } uninitialized reference
}

template void S<int>::f<double>();
