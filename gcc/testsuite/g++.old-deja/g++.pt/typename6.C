// { dg-do assemble  }

template <class T>
struct A
{
  typedef T A_Type;
};


template <class U>
struct B : public A<U>
{
  A_Type Func();		// { dg-error "" } declaration
};

template <class U>
A<U>::A_Type B<U>::Func()       // { dg-error "" } function
{				
}
