// PR c++/64647
// { dg-do compile { target c++14 } }

template<typename T>
constexpr T foo(T t)
{
  T tt = t;
  return tt;
}

struct X
{
  X() { }
};

int main()
{
  X x;
  foo(x);
}
