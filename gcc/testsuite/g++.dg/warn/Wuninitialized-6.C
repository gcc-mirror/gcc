/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

namespace std {
     typedef __SIZE_TYPE__ size_t;
   }
    extern "C++" {
   inline void* operator new(std::size_t, void* __p) throw() {
  return __p;
  }
   }
    namespace boost{
      __extension__ typedef long long long_long_type;
   }
    namespace mpl_ {
   namespace aux {
 }
   }
    namespace boost {
   namespace mpl {
  using namespace mpl_;
  }
  }
    namespace mpl_ {
   struct integral_c_tag {
  };
   template< int N > struct int_ {
  };
   }
    namespace boost {
   namespace mpl {
 }
  }
    namespace mpl_ {
   template< bool C_ > struct bool_;
   typedef bool_<true> true_;
   typedef bool_<false> false_;
   }
    namespace mpl_ {
   template< bool C_ > struct bool_ {
      static const bool value = C_;
  };
   template< typename T, T N > struct integral_c {
      static const T value = N;
  };
   }
    namespace boost{
   template <class T, T val> struct integral_constant : public mpl::integral_c<T, val> {
  };
   template<> struct integral_constant<bool,false> : public mpl::false_ {
     typedef integral_constant<bool,false> type;
  };
   template< typename T > struct is_lvalue_reference : ::boost::integral_constant<bool,false> {
  };
   template< typename T > struct is_rvalue_reference : ::boost::integral_constant<bool,false> {
  };
   namespace type_traits {
  template <bool b1, bool b2, bool b3 = false, bool b4 = false, bool b5 = false, bool b6 = false, bool b7 = false> struct ice_or;
  template <> struct ice_or<false, false, false, false, false, false, false> {
     static const bool value = false;
 };
  }
   }
    namespace boost {
   namespace detail {
  template <typename T> struct is_reference_impl {
    static const bool value = (::boost::type_traits::ice_or< ::boost::is_lvalue_reference<T>::value, ::boost::is_rvalue_reference<T>::value >::value)                  ;
 };
  }
   template< typename T > struct is_reference : ::boost::integral_constant<bool,::boost::detail::is_reference_impl<T>::value> {
  };
   namespace detail {
  template< typename T > struct alignment_of_impl {
    static const std::size_t value = __alignof__(T);
 };
  }
   template< typename T > struct alignment_of : ::boost::integral_constant<std::size_t,::boost::detail::alignment_of_impl<T>::value> {
  };
   }
    namespace mpl_ {
   struct na {
  };
   }
    namespace boost {
   namespace mpl {
  }
  }
    namespace boost {
   namespace mpl {
  template<       bool C     , typename T1     , typename T2     > struct if_c {
     typedef T1 type;
 };
  template<       typename T1     , typename T2     > struct if_c<false,T1,T2> {
     typedef T2 type;
 };
  template<       typename T1 = na     , typename T2 = na     , typename T3 = na     > struct if_ {
  private:     typedef if_c<           static_cast<bool>(T1::value)         , T2         , T3         > almost_type_;
  public:     typedef typename almost_type_::type type;
 };
  }
  }
    namespace boost{
   template <bool x> struct STATIC_ASSERTION_FAILURE;
   template <> struct STATIC_ASSERTION_FAILURE<true> {
  };
   template<int x> struct static_assert_test{
 };
   namespace detail {
  class alignment_dummy;
  typedef void (*function_ptr)();
  typedef int (alignment_dummy::*member_ptr);
  typedef int (alignment_dummy::*member_function_ptr)();
  template <bool found, std::size_t target, class TestType> struct lower_alignment_helper {
     typedef char type;
     enum { value = true };
 };
  template <std::size_t target, class TestType> struct lower_alignment_helper<false,target,TestType> {
     enum { value = (alignment_of<TestType>::value == target) };
     typedef typename mpl::if_c<value, TestType, char>::type type;
 };
  template <typename T> struct has_one_T {
   T data;
 };
  template <std::size_t target> union lower_alignment {
     enum { found0 = false };
  typename lower_alignment_helper< found0,target,char >::type t0;
 enum { found1 = lower_alignment_helper<found0,target,char >::value };
 typename lower_alignment_helper< found1,target,short >::type t1;
 enum { found2 = lower_alignment_helper<found1,target,short >::value };
 typename lower_alignment_helper< found2,target,int >::type t2;
 enum { found3 = lower_alignment_helper<found2,target,int >::value };
 typename lower_alignment_helper< found3,target,long >::type t3;
 enum { found4 = lower_alignment_helper<found3,target,long >::value };
 typename lower_alignment_helper< found4,target,::boost::long_long_type >::type t4;
 enum { found5 = lower_alignment_helper<found4,target,::boost::long_long_type >::value };
 typename lower_alignment_helper< found5,target,float >::type t5;
 enum { found6 = lower_alignment_helper<found5,target,float >::value };
 typename lower_alignment_helper< found6,target,double >::type t6;
 enum { found7 = lower_alignment_helper<found6,target,double >::value };
 typename lower_alignment_helper< found7,target,long double >::type t7;
 enum { found8 = lower_alignment_helper<found7,target,long double >::value };
 typename lower_alignment_helper< found8,target,void* >::type t8;
 enum { found9 = lower_alignment_helper<found8,target,void* >::value };
 typename lower_alignment_helper< found9,target,function_ptr >::type t9;
 enum { found10 = lower_alignment_helper<found9,target,function_ptr >::value };
 typename lower_alignment_helper< found10,target,member_ptr >::type t10;
 enum { found11 = lower_alignment_helper<found10,target,member_ptr >::value };
 typename lower_alignment_helper< found11,target,member_function_ptr >::type t11;
 enum { found12 = lower_alignment_helper<found11,target,member_function_ptr >::value };
 typename lower_alignment_helper< found12,target,boost::detail::has_one_T< char > >::type t12;
 enum { found13 = lower_alignment_helper<found12,target,boost::detail::has_one_T< char > >::value };
 typename lower_alignment_helper< found13,target,boost::detail::has_one_T< short > >::type t13;
 enum { found14 = lower_alignment_helper<found13,target,boost::detail::has_one_T< short > >::value };
 typename lower_alignment_helper< found14,target,boost::detail::has_one_T< int > >::type t14;
 enum { found15 = lower_alignment_helper<found14,target,boost::detail::has_one_T< int > >::value };
 typename lower_alignment_helper< found15,target,boost::detail::has_one_T< long > >::type t15;
 enum { found16 = lower_alignment_helper<found15,target,boost::detail::has_one_T< long > >::value };
 typename lower_alignment_helper< found16,target,boost::detail::has_one_T< ::boost::long_long_type > >::type t16;
 enum { found17 = lower_alignment_helper<found16,target,boost::detail::has_one_T< ::boost::long_long_type > >::value };
 typename lower_alignment_helper< found17,target,boost::detail::has_one_T< float > >::type t17;
 enum { found18 = lower_alignment_helper<found17,target,boost::detail::has_one_T< float > >::value };
 typename lower_alignment_helper< found18,target,boost::detail::has_one_T< double > >::type t18;
 enum { found19 = lower_alignment_helper<found18,target,boost::detail::has_one_T< double > >::value };
 typename lower_alignment_helper< found19,target,boost::detail::has_one_T< long double > >::type t19;
 enum { found20 = lower_alignment_helper<found19,target,boost::detail::has_one_T< long double > >::value };
 typename lower_alignment_helper< found20,target,boost::detail::has_one_T< void* > >::type t20;
 enum { found21 = lower_alignment_helper<found20,target,boost::detail::has_one_T< void* > >::value };
 typename lower_alignment_helper< found21,target,boost::detail::has_one_T< function_ptr > >::type t21;
 enum { found22 = lower_alignment_helper<found21,target,boost::detail::has_one_T< function_ptr > >::value };
 typename lower_alignment_helper< found22,target,boost::detail::has_one_T< member_ptr > >::type t22;
 enum { found23 = lower_alignment_helper<found22,target,boost::detail::has_one_T< member_ptr > >::value };
 typename lower_alignment_helper< found23,target,boost::detail::has_one_T< member_function_ptr > >::type t23;
 enum { found24 = lower_alignment_helper<found23,target,boost::detail::has_one_T< member_function_ptr > >::value };
 };
  union max_align {
 };
  template<std::size_t TAlign, std::size_t Align> struct is_aligned {
     static const bool value = (TAlign >= Align) & (TAlign % Align == 0)          ;
 };
  template <std::size_t Align> class type_with_alignment_imp {
     typedef ::boost::detail::lower_alignment<Align> t1;
     typedef typename mpl::if_c<           ::boost::detail::is_aligned< ::boost::alignment_of<t1>::value,Align >::value         , t1         , ::boost::detail::max_align         >::type align_t;
     static const std::size_t found = alignment_of<align_t>::value;
     typedef ::boost::static_assert_test< sizeof(::boost::STATIC_ASSERTION_FAILURE< ((found >= Align) == 0 ? false : true) >)> boost_static_assert_typedef_206;
     typedef ::boost::static_assert_test< sizeof(::boost::STATIC_ASSERTION_FAILURE< ((found % Align == 0) == 0 ? false : true) >)> boost_static_assert_typedef_207;
  public:     typedef align_t type;
 };
  }
   template <std::size_t Align> class type_with_alignment   : public ::boost::detail::type_with_alignment_imp<Align> {
  };
   }
    namespace boost {
   namespace detail{
  }
   template< typename T > struct remove_reference {
  };
   }
    namespace boost {
   namespace mpl {
  namespace aux {
 template< typename T > struct nested_type_wknd     : T::type { };
 }
 }
   namespace mpl {
  namespace aux {
 template< long C_ > struct not_impl     : bool_<!C_> { };
 }
  template<       typename T = na     > struct not_     : aux::not_impl<           ::boost::mpl::aux::nested_type_wknd<T>::value         > {
 };
  }
   namespace detail {
  template <typename T> struct make_reference_content {
     typedef T type;
 };
  }
   namespace detail {
  struct none_helper{
};
  }
   typedef int detail::none_helper::*none_t ;
   }
    namespace boost_optional_detail {
     template <class T, class Factory>   inline void construct(Factory const& factory, void* address)   {
    }
   }
    namespace boost {
   class in_place_factory_base ;
   class typed_in_place_factory_base ;
   namespace optional_detail {
  template <class T> class aligned_storage {
     union     __attribute__((may_alias))     dummy_u     {         char data[ sizeof(T) ];         typename type_with_alignment<           ::boost::alignment_of<T>::value >::type aligner_;     }
 dummy_ ;
   public:     void const* address() const { return &dummy_; }
     void * address() { return &dummy_; }
 }
  ;
  template<class T> struct types_when_isnt_ref {
   typedef T const& reference_const_type ;
   typedef T & reference_type ;
   typedef T const* pointer_const_type ;
   typedef T * pointer_type ;
   typedef T const& argument_type ;
 }
  ;
  template<class T> struct types_when_is_ref {
   typedef typename remove_reference<T>::type raw_type ;
   typedef raw_type& reference_const_type ;
   typedef raw_type& argument_type ;
 }
  ;
  struct optional_tag {
}
  ;
  template<class T> struct optional_base : public optional_tag {
     typedef     typename     ::boost::detail::make_reference_content<T>::type internal_type ;
     typedef aligned_storage<internal_type> storage_type ;
     typedef types_when_isnt_ref<T> types_when_not_ref ;
     typedef types_when_is_ref<T> types_when_ref ;
     typedef optional_base<T> this_type ;
     typedef T value_type ;
     typedef mpl::true_ is_reference_tag ;
     typedef mpl::false_ is_not_reference_tag ;
     typedef typename is_reference<T>::type is_reference_predicate ;
     typedef typename mpl::if_<is_reference_predicate,types_when_ref,types_when_not_ref>::type types ;
     typedef bool (this_type::*unspecified_bool_type)() const;
     typedef typename types::reference_type reference_type ;
     typedef typename types::reference_const_type reference_const_type ;
     typedef typename types::pointer_type pointer_type ;
     typedef typename types::pointer_const_type pointer_const_type ;
     typedef typename types::argument_type argument_type ;
     optional_base() : m_initialized(false) {}
     optional_base ( argument_type val ) : m_initialized(false)     {       construct(val);     }
     optional_base ( bool cond, argument_type val ) : m_initialized(false)     {       if ( cond )         construct(val);     }
     optional_base ( optional_base const& rhs )       :       m_initialized(false)     {       if ( rhs.is_initialized() )         construct(rhs.get_impl());     }
     pointer_const_type get_ptr() const { return m_initialized ? get_ptr_impl() : 0 ; }
     pointer_type get_ptr() { return m_initialized ? get_ptr_impl() : 0 ; }
     bool is_initialized() const { return m_initialized ; }
     void construct ( argument_type val )      {        new (m_storage.address()) internal_type(val) ;        m_initialized = true ;      }
     template<class Expr>     void construct ( Expr const& factory, in_place_factory_base const* )      {        typedef ::boost::static_assert_test< sizeof(::boost::STATIC_ASSERTION_FAILURE< ((::boost::mpl::not_<is_reference_predicate>::value) == 0 ? false : true) >)> boost_static_assert_typedef_355 ;        boost_optional_detail::construct<value_type>(factory, m_storage.address());        m_initialized = true ;      }
     template<class Expr>     void construct ( Expr const& factory, typed_in_place_factory_base const* )      {        typedef ::boost::static_assert_test< sizeof(::boost::STATIC_ASSERTION_FAILURE< ((::boost::mpl::not_<is_reference_predicate>::value) == 0 ? false : true) >)> boost_static_assert_typedef_364 ;        factory.apply(m_storage.address()) ;        m_initialized = true ;      }
     void destroy()     {     }
     unspecified_bool_type safe_bool() const { return m_initialized ? &this_type::is_initialized : 0 ; }
     reference_const_type get_impl() const { return dereference(get_object(), is_reference_predicate() ) ; }
     reference_type get_impl() { return dereference(get_object(), is_reference_predicate() ) ; }
     pointer_const_type get_ptr_impl() const { return cast_ptr(get_object(), is_reference_predicate() ) ; }
     pointer_type get_ptr_impl() { return cast_ptr(get_object(), is_reference_predicate() ) ; }
     internal_type const* get_object() const     {         union { void const* ap_pvoid; internal_type const* as_ptype; } caster = { m_storage.address() };         return caster.as_ptype;     }
     internal_type * get_object()     {         union { void* ap_pvoid; internal_type* as_ptype; } caster = { m_storage.address() };         return caster.as_ptype;     }
     reference_const_type dereference( internal_type const* p, is_not_reference_tag ) const { return *p ; }
     pointer_type cast_ptr( internal_type * p, is_reference_tag ) { return &p->get() ; }
     bool m_initialized ;
     storage_type m_storage ;
 }
  ;
  }
   template<class T> class optional : public optional_detail::optional_base<T> {
      typedef optional_detail::optional_base<T> base ;
    public :     typedef optional<T> this_type ;
      typedef typename base::reference_const_type reference_const_type ;
      reference_const_type get() const {
 (static_cast<void> (0)) ;
 return this->get_impl();
 }
      reference_const_type operator *() const {
 return this->get() ;
 }
      bool operator!() const {
 return !this->is_initialized() ;
 }
  }
   ;
   template<class OptionalPointee> inline bool equal_pointees2 ( OptionalPointee const& x, OptionalPointee const& y ) {
    return (!x) != (!y) ? false : ( !x ? true : (*x) == (*y) ) ;
  }
   template<class T> inline bool operator == ( optional<T> const& x, optional<T> const& y ) {
  return equal_pointees2(x,y);
  }
   template<class T> inline bool operator != ( optional<T> const& x, optional<T> const& y ) {
  return !( x == y ) ;
  }
   }
    ::boost::optional< std::size_t > getitem();
    class BAR {
   public:     int FOO();
   private:     ::boost::optional< std::size_t > m_aHoveredItem;
   };
    int BAR::FOO() {
          ::boost::optional< std::size_t > aOldItem(getitem());
          ::boost::optional< std::size_t > aNewItem(m_aHoveredItem);
          if (aOldItem != aNewItem)             return 1;
	return 0;
   }
 
