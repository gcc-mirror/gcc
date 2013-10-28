// PR c++/47774
// { dg-options -std=c++11 }

struct A
{
  A() {}
};

template <typename T>
struct array
{
  constexpr array() : mem() {}
  T mem[7];
};

int main()
{
  array<A> ar;
}
