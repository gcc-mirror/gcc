// PR c++/53856

struct A
{
  template<typename T>
  struct B;
};

template<typename T = int>
struct A::B
{
  int i;
};

A::B<int> b = { };
