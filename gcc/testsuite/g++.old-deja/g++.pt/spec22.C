// Build don't link:

template <class T>
struct S
{
  template <class U>
  void f();
};


template <class T> 
template <> // ERROR - enclosing classes not specialized
void S<T>::f<int> () 
{ // ERROR - template does not match any declaration
}
