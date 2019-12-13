// PR c++/84330
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

struct A
{
  template<typename T>
    requires (sizeof(T) >> 0) // { dg-error "constraint does not have type 'bool'" }
  void foo(T);

  void bar()
  {
    foo(0);  // { dg-error "no matching function" }
  }
};
