// PR c++/19440
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// { dg-do compile }

template<int> struct A
{
  ~A<0>(); // { dg-error "declaration" }
};
