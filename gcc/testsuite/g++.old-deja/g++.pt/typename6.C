// Build don't link:
// Special g++ Options:

template <class T>
struct A
{
  typedef T A_Type;
};


template <class U>
struct B : public A<U>
{
  A_Type Func();
};


template <class U>
A<U>::A_Type B<U>::Func()
{
}
