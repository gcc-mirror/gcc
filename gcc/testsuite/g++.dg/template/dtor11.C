template <class T>
struct B
{
  void f(T *p)
  {
    p->template A<int>::~A<int>();
    p->A::~A();
    p->~A<int>();
    p->~A();
    p->~T();
    p->T::~T();
  }
};

template <class T>
struct A
{ };

int main()
{
  B<A<int> >().f(0);
}
