// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module billy.bob.thornton;
// { dg-module-cmi "billy.bob.thornton" }

export template<unsigned I> struct Outer
{
  template<unsigned J> struct Inner
  {
    static unsigned m () 
    {
      return I * J;
    }
  };
};

