// Build don't link:
// Special g++ Options:
// excess errors test - XFAIL *-*-*

template <class T>
struct B 
{
  typedef int I;
};


template <class T>
struct D : public B<T>
{
  void f();
};


template <class T>
void D<T>::f()
{
  I();
}


template <>
struct B<int> 
{
  void I();
};


int main()
{
  D<int> di;
  di.f();
}
