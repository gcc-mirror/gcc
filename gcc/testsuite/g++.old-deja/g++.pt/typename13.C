// { dg-do assemble  }
// { dg-options "-fpermissive" }

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
  I(); // { dg-warning "" }
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
