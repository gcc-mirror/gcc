// PR c++/53223
// { dg-do compile { target c++11 } }

#include <type_traits>

#define SA(x) static_assert ((x), #x)

struct A
{
  int good() const;
  int operator *() const;
  int operator ++() const;
  int operator [](int) const;
};

int operator-- (const A&);

template<typename T>
void func(T t)
{
  A x;
  auto &&g1 = x.good();
  auto &&g2 = x.operator*();
  auto &&error1 = *x;
  auto &&error2 = ++x;
  auto &&error3 = --x;
  auto &&error4 = x[5];
  SA ((std::is_same<int &&, decltype (error1)>::value));
  SA ((std::is_same<int &&, decltype (error2)>::value));
  SA ((std::is_same<int &&, decltype (error3)>::value));
  SA ((std::is_same<int &&, decltype (error4)>::value));
}

void func2(int)
{
  A x;
  auto &&g = *x;
}

int main()
{
  func(0);
  func2(0);
}

