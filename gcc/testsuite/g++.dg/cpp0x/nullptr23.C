// PR c++/50361
// { dg-options -std=c++0x }

#include <initializer_list>

struct Foo
{
  Foo(std::initializer_list<Foo>) { };

  template<class T> Foo(T t) { T u(t); };

private:
  union Data
  {
    Data() : null(nullptr) {}

    std::nullptr_t null;
  } u_;
};

int main()
{
  Foo f = { {} };
}
