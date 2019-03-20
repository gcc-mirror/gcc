// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-bmi foo }

export struct X 
{
  enum q
  {
    frob
  };
};

export template <int I> struct TPL
{
  enum p
  {
    v = I
  };
};
