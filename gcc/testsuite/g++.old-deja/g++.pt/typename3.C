// Build don't link:
// Special g++ Options: -Wno-deprecated

template <class T>
struct A
{
  typedef T A_Type;
};


template <class U>
struct B : public A<U>
{
  A_Type Func(); // WARNING - implicit typename
};


template <class U>
B<U>::A_Type B<U>::Func() { // WARNING - implicit typename
}
