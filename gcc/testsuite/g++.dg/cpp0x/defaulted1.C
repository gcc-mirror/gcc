// Positive test for defaulted/deleted fns
// { dg-do run }
// { dg-options "-std=c++0x" }

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
  B b = {1};
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
