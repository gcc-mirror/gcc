// PR c++/47774
// { dg-options -std=c++0x }

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
