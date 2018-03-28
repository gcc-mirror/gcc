// Check namespace needed only by internal reference is found
// { dg-additional-options -fdump-lang-module }

export module frob;
// { dg-module-bmi frob }

namespace silent 
{
  namespace inner 
  {
    static int X () 
    {
      return 1;
    }
  }
}

export int f (int = silent::inner::X ());
