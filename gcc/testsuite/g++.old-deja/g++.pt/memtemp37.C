// Build don't link:
// GROUPS passed templates membertemplates
template <class T>
struct S
{
  template <class U>
  void f(U u);
};


template <class T>
template <class U>
void S<T>::f(U)
{
}

enum 
{
  a = 3
};
