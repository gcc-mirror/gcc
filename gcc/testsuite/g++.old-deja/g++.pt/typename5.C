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
  void Func(A_Type);
};


template <class U>
void C<U>::Func(A_Type)
{
}
