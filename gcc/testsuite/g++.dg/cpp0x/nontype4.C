// PR c++/56428
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

struct A { };

template<bool B>
  struct Builder
  {
    static A build() { return A(); }
  };

template<A (*F)()>
  A f()
  {
    return Builder<F != nullptr>::build();
  }

A g();

int main()
{
  f< &g >();
  f< nullptr >();
  f< &f<nullptr> >();
}
