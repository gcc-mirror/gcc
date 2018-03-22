// PR c++/60153
// { dg-do compile { target c++11 } }

enum class foo :int {x,y,z};

template <int a, foo b>
class A
{
public:
  A()
  {
  }
};

template <typename T>
struct B
{
  typedef T value_type;
  static const T val;
};

template <typename... B>
struct madscience_intitializer
{
  template <typename B::value_type... args>
  using ret_type = A<args...>;
};

int main()
{
  madscience_intitializer<B<int>,B<foo> >::ret_type<1,foo::y> a;
}
