// { dg-module-do run }

export module billy.bob.thornton;
// { dg-module-bmi "billy.bob.thornton" }

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

