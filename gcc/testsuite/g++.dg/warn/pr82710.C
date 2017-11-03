// { dg-additional-options -Wparentheses }

// the MVP warning triggered on a friend decl.  */
class X;

namespace here 
{
  // these friends
  X friendFunc1();
  X *friendFunc2 ();
  int friendFunc3 ();
}

namespace nm
{
  namespace here 
  {
    // Not these friends
    void friendFunc1 ();
    void friendFunc2 ();
    void friendFunc3 ();
  }

  class TestClass
  {
    friend X (::here::friendFunc1 ()); // parens are needed
    friend X *(::here::friendFunc2 ()); // { dg-warning "" }
    friend X *::here::friendFunc2 ();
    friend int (::here::friendFunc3 ()); // { dg-warning "" }
  };
}

