// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

// indirect references to import, template member of template case

export module foo;
// { dg-module-cmi foo }

namespace foo
{
  export template<int I> class TPL
  {
  public:
    template <int J> int frob () const 
    {
      return I + J;
    }
  };
}

