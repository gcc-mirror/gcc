// Positive test for defaulted/deleted fns
// { dg-do run { target c++11 } }

struct A
{
  int i;
  A() = default;
  A(const A&) = delete;
  A& operator=(const A&) = default;
  ~A();
};

A::~A() = default;

void f() = delete;

struct B
{
  int i;
  B() = default;
};

int main()
{
  A a1, a2;
#if __cplusplus <= 201703L
  B b = {1};
#endif
  a1 = a2;
}

// fns defaulted in class defn are trivial
struct C
{
  C() = default;
  C(const C&) = default;
  C& operator=(const C&) = default;
  ~C() = default;
};

union U
{
  C c;
};
