// { dg-module-do run }

// indirect references to import, template member of template case

export module foo;
// { dg-module-bmi foo }

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

