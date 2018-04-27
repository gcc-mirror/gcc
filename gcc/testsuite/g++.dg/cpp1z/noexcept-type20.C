// PR c++/85545
// { dg-do compile { target c++11 } }

struct A
{
  void foo() noexcept;
};

template<typename T> void bar(T);

void baz()
{
  bar(static_cast<void(A::*)()>(&A::foo));
}
