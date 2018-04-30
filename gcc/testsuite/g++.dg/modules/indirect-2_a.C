// { dg-module-do run }

// indirect references to import, simple templates

export module foo;
// { dg-module-bmi foo }

namespace foo
{
  export template<int I> int frob ()
  {
    return I;
  }

  export template<int I> class X
  {
    int i = I;

  public:
    operator int () const { return i; }
  };
}

