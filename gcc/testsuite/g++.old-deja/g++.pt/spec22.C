// { dg-do assemble  }

template <class T>
struct S
{
  template <class U>
  void f();
};


template <class T> 
template <> // { dg-error "" } enclosing classes not specialized
void S<T>::f<int> () 
{ // { dg-error "" } template does not match any declaration
}
