// { dg-do assemble  }

template <class T>
struct A
{
  typedef T A_Type;
};


template <class U>
struct B : public A<U>
{
  typename B<U>::A_Type Func();
};


template <class U>
typename B<U>::A_Type B<U>::Func()
{
}
