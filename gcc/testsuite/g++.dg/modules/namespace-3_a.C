// Check namespace needed only by internal reference is not made visible
// { dg-additional-options "-fmodules-ts" }

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

export int f (int y)
{
  return y + silent::inner::X ();
}
