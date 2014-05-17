// PR c++/52875
// { dg-do compile { target c++11 } }

struct A
{
  friend void swap(A&,A&)  {}
};

class B
{
  A a;
    
  template <class T>
  friend auto swap(T& x, T& y) -> decltype(swap(x.a,y.a))
  {
    swap(x.a,y.a);
  }
};

int main()
{
  B x, y;
  swap(x, y);
}
