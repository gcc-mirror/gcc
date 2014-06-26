// PR c++/61537
// { dg-do compile }

struct A {};

template <typename T>
struct B
{
  template <typename U>
  void f(U, struct A);
};

template <typename T>
template <typename U>
void B<T>::f(U, struct A)
{
}

int main()
{
  B<char> b;
  b.f(42, A());
}
