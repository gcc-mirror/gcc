// PR c++/14283

struct A
{};

namespace N
{}

template <typename> struct C
{
  typedef A::template INVALID<void> X0;  // { dg-error "23:'INVALID' in 'struct A' does not name a template type" }
  typedef N::template INVALID<void> X1;  // { dg-error "23:'INVALID' in namespace 'N' does not name a template type" }
};
