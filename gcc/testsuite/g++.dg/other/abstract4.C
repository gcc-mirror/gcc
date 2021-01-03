// PR c++/51184

template<typename T>
struct S { };

template<typename T>
void foo();

struct Abs
{
  virtual void bar() = 0;
};

int main()
{
  S<Abs(int)> s;
  foo<Abs(int)>();
}
