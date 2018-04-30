// PR c++/84330
// { dg-options "-fconcepts" }

struct A
{
  template<typename T> requires sizeof(T) >> 0 void foo(T);  // { dg-error "predicate constraint" }

  void bar()
  {
    foo(0);  // { dg-error "no matching" }
  }
};
