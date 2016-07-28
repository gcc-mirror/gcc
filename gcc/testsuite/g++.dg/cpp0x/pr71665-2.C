// PR c++/71665
// { dg-do compile { target c++11 } }

class A 
{
  enum class E { e = 1 };
  enum { a = E::e };  // { dg-error "integral or unscoped enumeration" }
};
