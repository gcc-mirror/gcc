// PR sanitizer/61272
// { dg-do compile }
// { dg-options "-fsanitize=undefined -std=c++11" }

namespace std
{
  template < typename _Tp > class allocator;
  template < typename _Alloc > struct allocator_traits {
  private:
      template < typename _Tp > auto construct ( _Alloc & __a, _Tp * __p)-> // { dg-message "private" }
      decltype (_S_construct (__a, __p)) { }
  };
  namespace __gnu_cxx
  {
    template < typename _Alloc > struct __alloc_traits:std::allocator_traits < _Alloc >
    {
      typedef std::allocator_traits < _Alloc > _Base_type;
      using _Base_type::construct; // { dg-error "within this context" }
    };
    template < typename _Tp, typename _Alloc > struct _Vector_base { typedef typename __gnu_cxx::__alloc_traits < _Alloc >::template rebind < _Tp >::other _Tp_alloc_type; }; // { dg-error "no class template" }
    template < typename _Tp, typename _Alloc = std::allocator < _Tp > >class vector : protected _Vector_base < _Tp, _Alloc > { };
    template < typename NumberT > struct Point2d { };
    typedef Point2d < int >GdsPoint;
    class GdsPointList : public vector < GdsPoint > {};}}
