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
};


template <class U>
struct C : public B<U>
{
  A_Type Func();
};


template <class U>
C<U>::A_Type C<U>::Func()
{
}
