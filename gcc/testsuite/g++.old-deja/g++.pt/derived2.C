// Build don't link:
// Special g++ Options:

template <typename T>
void f(T);
template <>
void f(int) {}

struct B {
  typedef int I;
};

template <typename T> 
struct D1 : virtual public B {
  typedef T I;
};


template <typename T> 
struct D : virtual public B, public D1<T>
{
  void g()
    {
      I i;
      f(i);
    }
};

int
main()
{
  D<double> d;
  d.g();
}

