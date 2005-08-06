/* { dg-do compile } */
namespace s
{
  template <int> struct _List_base
  {
     int _M_impl;
  };
  template<int i> struct list : _List_base<i>
  {
    using _List_base<i>::_M_impl;
  }
}  /* { dg-error "expected unqualified-id before '\}'" } */
s::list<1> OutputModuleListType;  /* { dg-error "expected" } */
