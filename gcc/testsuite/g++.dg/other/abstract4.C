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
  S<Abs(int)> s;     // { dg-error "abstract" }
  foo<Abs(int)>();   // { dg-error "abstract" }
}
