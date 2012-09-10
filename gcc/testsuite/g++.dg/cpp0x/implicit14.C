// PR c++/54506
// { dg-do compile { target c++11 } }

template <class T>
struct A
{
  A() {}

  A(A const volatile &&) = delete;
  A &operator =(A const volatile &&) = delete;

  template <class U> A(A<U> &&) {}
  template <class U> A &operator =(A<U> &&) { return *this; }
};

struct B
{
  A<int> a;
  B() = default;
};

int main()
{
  B b = B();
  b = B();
}
