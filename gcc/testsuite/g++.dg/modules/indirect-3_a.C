// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

// indirect references to import, template member non-template or
// non-template member of template cases

export module foo;
// { dg-module-cmi foo }

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
