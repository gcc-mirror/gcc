// { dg-do compile { target c++11 } }

enum class A
{
  X
};

enum class B
{
  X = A::X // { dg-error "could not convert" }
};

