// PR c++/65061

struct B
{
  template<typename T>
  struct S {};
};

struct D : B
{
  using B::S;

  template<typename T>
  void doIt(/*struct*/ S<T>&);
};
