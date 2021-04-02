// { dg-additional-options -fmodules-ts }

import "tpl-tpl-parm-1_a.H";

template<> struct __xref<int> 
{
  template <typename T> struct __type;
};

template<> struct basic_common_reference<__xref<int>::__type>
{
  typedef int type;
};

__basic_common_ref<int> main ()
{
  return 0;
}
