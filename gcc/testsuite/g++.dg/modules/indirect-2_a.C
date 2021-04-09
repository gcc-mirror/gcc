// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

// indirect references to import, simple templates

export module foo;
// { dg-module-cmi foo }

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

