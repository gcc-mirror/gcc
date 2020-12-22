// { dg-additional-options -fmodules-ts }

export module hidden;
// { dg-module-cmi hidden }

export struct X 
{
  int m;

  X(int m) :m(m) {}

  operator int () const 
  {
    return m;
  }
};

// Not found via any ADL outside of module hidden
int frob (int x)
{
  return x;
}
