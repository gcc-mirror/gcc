// { dg-additional-options -fmodules-ts }

export module hidden;
// { dg-module-bmi hidden }

export struct X 
{
  int m;

  X(int m) :m(m) {}

  operator int () const 
  {
    return m;
  }
};

int frob (int x)
{
  return x;
}
