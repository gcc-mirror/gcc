// Build don't link:
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
  U& u; // ERROR - uninitialized reference
}

template void S<int>::f<double>(); // ERROR - instantiated from here
