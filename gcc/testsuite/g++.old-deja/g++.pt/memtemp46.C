// { dg-do link  }
// GROUPS passed templates membertemplates
template<class T, class U>
class A
{
};

template<class U>
class A<float, U>
{
public:
  template <class V>
  void func(V v1 = 0) {}
};

int main()
{
  A<float, int> a;
  a.func("abc");
}
