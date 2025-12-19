// { dg-do compile { target c++20 } }
// PR c++/117034

template<typename, auto>
concept A = true;

void f(A<[] {}> auto) {}

int main()
{
  f(42);
}
