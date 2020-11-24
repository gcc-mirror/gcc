// PR c++/97918
// { dg-additional-options "-g -O -flto" }
// { dg-do compile { target c++11 } }

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
