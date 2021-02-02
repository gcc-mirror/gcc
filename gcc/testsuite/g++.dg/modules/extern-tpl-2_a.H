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
