// PR c++/58647

struct A
{
  static void foo();
};

template<typename> void bar()
{
  A().foo;
}
