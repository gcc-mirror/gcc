// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wparentheses -Wno-non-template-friend" }

// the MVP warning triggered on a friend decl.  */
class X;
enum class Q {}; // C++ 11ness
enum R {};

namespace here 
{
  // these friends
  X friendFunc1();
  X *friendFunc2 ();
  int friendFunc3 ();
  int bob ();
  Q bill ();
  R ben ();
}

namespace nm
{
  namespace here 
  {
    // Not these friends
    void friendFunc1 ();
    void friendFunc2 ();
    void friendFunc3 ();
    int bob ();
    Q bill ();
    R ben ();
  }

  class TestClass
  {
    friend X (::here::friendFunc1 ()); // parens are needed
    friend X *(::here::friendFunc2 ()); // { dg-warning "" }
    friend X *::here::friendFunc2 ();
    friend int (::here::friendFunc3 ()); // { dg-warning "" }
  };

  template <typename T> class X
  {
    friend typename T::frob (::here::bob ());
    friend Q (::here::bill ());
    friend R (::here::ben ());
  };
}

