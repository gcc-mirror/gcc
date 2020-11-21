// PR c++/97918
// { dg-do compile { target c++11 } }
// { dg-require-effective-target lto }
// { dg-additional-options "-g -O -flto" }

namespace { class A {}; }
class B {};
template <typename T> struct H {
  constexpr static unsigned h = 0;
};

template <typename T> A bar ()
{
  struct J {
    static void foo();
  };
  H<J>();
  return A ();
}

void fn ()
{
  bar<B>;			// only mentions the function
}
