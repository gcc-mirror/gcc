// { dg-do assemble  }

// Origin: Larry Evans <jcampbell3@prodigy.net>

// Bug: enum in default template arguments are not properly handled.

enum Enum0 { E0, E1, E2 };
enum Enum1 { E3=E2+1, E4=E3+1 };

template <Enum0 Out_FARG0> class OutTmpl
{
  public:
    template <Enum0 In_FARG0, Enum1 In_FARG1=E4> class InTmpl
    {
    };
};

OutTmpl<E1> m;
