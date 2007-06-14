// PR c++/31903
// Test for anonymous namespace internal linkage, for typeinfo

// { dg-do compile }
// { dg-final { scan-assembler-not "globl.*_ZTIN*3fooE" } }

#include <typeinfo>
namespace 
{
  class foo 
  {
    virtual void bar();
  };
}

const std::type_info &X = typeid(foo);
