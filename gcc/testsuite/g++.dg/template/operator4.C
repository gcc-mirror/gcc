// PR c++/17068

struct A
{
  template<int> void operator()() {}
};

template<typename> void foo()
{
  A().template operator()<0>();
}
