// PR c++/49042
// { dg-do compile { target c++11 } }

template <class T>
class A
{
  T p;
public:
  template <class U> auto f() -> decltype(+p) { return p; }
};

int main()
{
  A<int>().f<int>();
}
