// { dg-do assemble  }

template <class T>
struct S
{
  template <class U>
  void f();
};


template <class T> 
template <> // { dg-error "enclosing class templates|invalid explicit specialization" }
void S<T>::f<int> ()  // { dg-error "does not match|invalid function declaration" }
{
}
