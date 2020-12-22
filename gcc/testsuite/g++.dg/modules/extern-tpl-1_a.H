// { dg-module-do link }
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template <unsigned I> struct TPL
{
  int Source () 
  {
    return I;
  }
};

extern template class TPL<1>;

struct Foo 
{
  TPL<1> m;

  Foo () {m.Source ();};
  
};

static Foo __ioinit;
