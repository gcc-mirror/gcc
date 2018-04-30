// { dg-module-do run }

// indirect references to import, template member case

export module foo;
// { dg-module-bmi foo }

namespace foo
{
  export class X
  {
  public:
    template <int I> int frob () const { return I; }
  };

  export template<int I> class TPL
  {
  public:
    int frob () const { return I; }
  };
}
