// PR c++/60254
// { dg-do compile { target c++11 } }

struct A
{
  template<typename T> bool foo(T)
  {
    static_assert(foo(0), "Error"); // { dg-error "non-constant condition|constant expression" }
    return true;
  }
};
