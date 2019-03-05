// PR c++/82836
// { dg-do compile { target c++17 } }
// { dg-require-effective-target int128 }
// { dg-require-effective-target __float128 }
// { dg-additional-options "-Wno-pedantic -Wno-return-type" }
// { dg-add-options __float128 }
// We were resetting DECL_ASSEMBLER_NAME when processing pragma weak,
// breaking C++'s mangling alias hash table.  That hash table needs to
// be tickled in just the right way to hit the problem.

namespace std {
typedef __SIZE_TYPE__ size_t;
inline namespace __cxx11 {}
  double abs() {return 0;}
  __int128 abs(__int128 ) {return 0;}
  __float128 abs(__float128 ) {return 0;}

  
#pragma weak pthread_create
typedef int luaL_Reg;
typedef int swig_lua_const_info;
typedef int swig_lua_attribute;
typedef int swig_lua_class;
}
static int swig_types;
static int swig_module;
namespace std {
  template < typename > class allocator;
  template < class > struct char_traits;
  namespace __cxx11 {
  template < typename _CharT, typename =  _CharT ,
             typename =  _CharT  >
  class basic_string;
  typedef basic_string< char > string;
  }
}
namespace __gnu_cxx {
  template < typename > struct __numeric_traits_integer;
  struct __conditional_type {
    typedef __numeric_traits_integer< int > __type;
  }__is_null_pointer0;
  template < typename _Value > struct __numeric_traits_integer {
    static const _Value __min = 0;
    static const _Value __max = 0;
  };
  template < typename _Value >
  const _Value __numeric_traits_integer< _Value >::__min;
  template < typename _Value >
  const _Value __numeric_traits_integer< _Value >::__max;
  template < typename >
  struct __numeric_traits : __conditional_type::__type {};
}
namespace std {
  template < typename _Tp, _Tp __v > struct integral_constant {
    static constexpr _Tp value = __v;
  };
  template < typename _Tp, _Tp __v >
  constexpr _Tp integral_constant< _Tp, __v >::value;
  typedef integral_constant< bool, false > false_type;
  struct __is_void_helper : false_type {};
  struct is_void : __is_void_helper{};
  template < int > struct conditional  ;
  template < typename , typename ,
             template < typename... > class >
  struct __detector {
       ;
  };
  template < typename _Default, template < typename > class _Op>
  using __detected_or = __detector< _Default, void, _Op>;
  template < typename _Default, template < typename> class _Op
               >
  using __detected_or_t =
      typename __detected_or< _Default, _Op>::type;
  struct  {}piecewise_construct ;
  struct random_access_iterator_tag ;
  template < typename , typename > struct iterator {
    typedef int difference_type;
    typedef int reference;
  };
  using __make_not_void = conditional< is_void::value >;
      struct pointer_traits 
    ;
  
  template <> struct char_traits< char > {
    typedef char char_type;
    typedef int int_type;
    void assign() {}
    bool eq() {return 0;}
    bool lt() {return 0;}
    int compare() {return 0;}
    size_t length() {return 0;}
    char_type find() {return 0;}
    char_type move() {return 0;}
    char_type copy() {return 0;}
    char_type assign(char_type ) {return 0;}
    char_type to_char_type() {return 0;}
    int_type to_int_type() {return 0;}
    bool eq_int_type() {return 0;}
    int_type eof() {return 0;}
    int_type not_eof() {return 0;}
  };
  template <> struct char_traits< wchar_t > {
    typedef wchar_t char_type;
    typedef int int_type;
    void assign() {}
    bool eq() {return 0;}
    bool lt() {return 0;}
    int compare() {return 0;}
    size_t length() {return 0;}
    char_type find() {return 0;}
    char_type move() {return 0;}
    char_type copy() {return 0;}
    char_type assign(char_type ) {return 0;}
    char_type to_char_type() {return 0;}
    int_type to_int_type() {return 0;}
    bool eq_int_type() {return 0;}
    int_type eof() {return 0;}
    int_type not_eof() {return 0;}
  };
}
typedef int uint_least16_t;
typedef int uint_least32_t;
namespace std {
template <> struct char_traits< char16_t > {
  typedef char16_t char_type;
  typedef uint_least16_t int_type;
  void assign() {}
  bool eq() {return false;}
  bool lt() {return false;}
  int compare() {return 0;}
  size_t length() {return 0;}
  char_type find() {return 0;}
  char_type move() {return 0;}
  char_type copy() {return 0;}
  char_type assign(char_type ) {return 0;}
  char_type to_char_type() {return 0;}
  int_type to_int_type() {return 0;}
  bool eq_int_type() {return false;}
  int_type eof() {return 0;}
  int_type not_eof() {return 0;}
};
template <> struct char_traits< char32_t > {
  typedef char32_t char_type;
  typedef uint_least32_t int_type;
  void assign() {}
  bool eq() {return false;}
  bool lt() {return false;}
  int compare() {return 0;}
  size_t length() {return 0;}
  char_type find() {return 0;}
  char_type move() {return 0;}
  char_type copy() {return 0;}
  char_type assign(char_type ) {return 0;}
  char_type to_char_type() {return 0;}
  int_type to_int_type() {return 0;}
  bool eq_int_type() {return false;}
  int_type eof() {return 0;}
  int_type not_eof() {return 0;}
};
}
void *operator new(std::size_t) {return (void *)1;}
void *operator new[](std::size_t) {return (void *)1;}
void operator delete(void *){}
void operator delete[](void *) {}
namespace  {
        ;
}
namespace std {

template < typename  > class allocator {
public:
  
  
  ~allocator();
}
;
template < typename _Tp >
struct less {
  bool operator()( _Tp ,  _Tp ) {return false;}
};
}
typedef int _Atomic_word;
namespace {
  _Atomic_word __exchange_and_add_single0;
  static void __atomic_add_single();
  _Atomic_word __attribute____exchange_and_add_dispatch0;
  static void __attribute____atomic_add_dispatch();
}
namespace std {
  struct __allocator_traits_base {
    template < typename = void > struct __rebind {
      using type =
          allocator< char >;
    }

  ;
  };
  template < typename , typename >
  using __alloc_rebind =
      __allocator_traits_base::__rebind<>::type;
  template < typename >
  struct allocator_traits   {
       
    template < typename _Tp >
    using rebind_alloc = __alloc_rebind<  char , _Tp >;
  };
}
namespace __gnu_cxx {
  template < typename >
  struct __alloc_traits : std::allocator_traits< std::allocator< char > > {
    template < typename > struct rebind {
      typedef rebind_alloc< int >
          other;
    };
  };
}
namespace std {
  namespace __cxx11 {
  template < typename , typename , typename >
  class basic_string {
    typedef __gnu_cxx::__alloc_traits<  char  >::
        rebind< int >::other _Char_alloc_type
    ;
    struct  : _Char_alloc_type {}_M_dataplus;
    int _S_compare() {
      
      if (__gnu_cxx::__numeric_traits< int >::__max)
        
       (__gnu_cxx::__numeric_traits< int >::__min)
        ;
      return 0;
    }
  };
  }
  template < typename _CharT, typename _Traits, typename _Alloc >
  basic_string<  _Traits> operator+(
       basic_string<   _Alloc > );
  template < typename _CharT, typename _Traits, typename _Alloc >
  basic_string<  _Traits> operator+(
        _Alloc  );
  template < typename _CharT, typename _Traits, typename _Alloc >
  basic_string<  _Traits> operator+(
        _Alloc  );
  template < typename , typename > struct __hash_base ;
  template < typename > struct hash;
  template <> struct hash< char > {
    void operator0() {}
  };
  template <> struct hash< signed char > {
    void operator0() {}
  };
  template <> struct hash< unsigned char > {
    void operator0() {}
  };
  template <> struct hash< wchar_t > {
    void operator0() {}
  };
  template <> struct __hash_base< size_t, char16_t > {
    size_t operator0() {return 0;}
  };
  template <> struct __hash_base< size_t, char32_t > {
    size_t operator0() {return 0;}
  };
  template <> struct hash< short > {
    void operator0() {}
  };
  template <> struct hash< int > {
    void operator0() {}
  };
  template <> struct hash< long > {
    void operator0() {}
  };
  template <> struct hash< long long > {
    void operator0() {}
  };
  template <> struct hash< unsigned short > {
    void operator0() {}
  };
  template <> struct hash< unsigned > {
    void operator0() {}
  };
  template <> struct hash< unsigned long > {
    void operator0() {}
  };
  template <> struct hash< unsigned long long > {
    void operator0() {}
  };
  template <> struct hash< __int128 > {
    void operator0() {}
  };
  template <> struct hash< __int128 unsigned > {
    void operator0() {}
  };
  typedef long _Bit_type;
  struct _Bit_reference {
     ;
    _Bit_type _M_mask;
    _Bit_reference  () {}
    operator bool() {return false;}
    _Bit_reference (bool ) {}
    _Bit_reference operator=(_Bit_reference ) {return *this;}
    bool operator==(_Bit_reference ) {return false;}
    bool operator<(_Bit_reference ) {return false;}
    void flip() {}
  };
  void swap() {}
  void swap(bool ) {}
  void swap(_Bit_reference ) {}
  struct _Bit_iterator_base
      :iterator< random_access_iterator_tag, bool > {
    _Bit_type *_M_p;
    int _M_offset;
    _Bit_iterator_base(_Bit_type *, int 
         _M_offset) {}
    void _M_bump_up() {}
    void _M_bump_down() {}
    void _M_incr() {}
    bool operator==(_Bit_iterator_base ) {return false;}
    bool operator<(_Bit_iterator_base ) {return false;}
    bool operator!=(_Bit_iterator_base ) {return false;}
    bool operator>(_Bit_iterator_base ) {return false;}
    bool operator<=(_Bit_iterator_base ) {return false;}
    bool operator>=(_Bit_iterator_base ) {return false;}
  };
  struct _Bit_iterator : _Bit_iterator_base {
    _Bit_iterator() : _Bit_iterator_base(0, 0) {}
    _Bit_iterator(_Bit_type *__x, int __y)
        : _Bit_iterator_base(__x, __y) {}
    iterator _M_const_cast() {return iterator ();}
    iterator operator+() {return iterator ();}
    iterator operator-() {return iterator ();}
    reference operator[](difference_type ) {return 0;}
  };
  void operator+(_Bit_iterator ) {}
  struct _Bit_const_iterator : _Bit_iterator_base {
    _Bit_const_iterator() : _Bit_iterator_base(0, 0) {}
    _Bit_type _Bit_const_iterator___x;
    _Bit_const_iterator(int __y)
        : _Bit_iterator_base(&_Bit_const_iterator___x, __y) {}
    _Bit_const_iterator(_Bit_iterator __x)
        : _Bit_iterator_base(_M_p, _M_offset) {}
    void _M_const_cast() {}
    void operator*() {}
    void operator++() {}
    void operator++(int) {}
    void operator--() {}
  };
  class runtime_error {
  public:
    runtime_error(const string );
  };
  inline namespace _V2 {
  class error_category {
    bool operator<(error_category __other) {
      less< error_category * >()(this, &__other);
      return false;
    }
    bool operator==(error_category ) {return false;}
    bool operator!=(error_category ) {return false;}
  };
  }
  struct error_code {
    error_code() : _M_cat() {}
    error_code(error_category ) : _M_cat() {}
    void assign() {}
    void clear() {}
    int value() {return 0;}
    error_category category() {return error_category ();}
    string message() {return string();}
    error_category _M_cat;
  };
  inline error_code make_error_code() noexcept {return error_code ();}
  inline bool operator<(const error_code ,
                        const error_code ) noexcept {return false;}
  struct error_condition {
    error_condition() {}
    error_condition(const error_category ) {}
    void assign() noexcept {}
    void clear() noexcept {}
    int value() {return 0;}
    const error_category category() {return error_category ();}
    string message_M_cat;
  };
  inline error_condition make_error_condition() noexcept {return error_condition ();}
  inline bool operator<(const error_condition ,
                        const error_condition ) noexcept {return false;}
  inline bool operator==(const error_code ,
                         const error_code ) noexcept {return false;}
  inline bool operator==(const error_code ,
                         const error_condition ) noexcept {return false;}
  inline bool operator==(const error_condition ,
                         const error_code ) noexcept {return false;}
  inline bool operator==(const error_condition ,
                         const error_condition ) noexcept {return false;}
  inline bool operator!=(const error_code ,
                         const error_code ) noexcept {return false;}
  inline bool operator!=(const error_code ,
                         const error_condition ) noexcept {return false;}
  inline bool operator!=(const error_condition ,
                         const error_code ) noexcept {return false;}
  inline bool operator!=(const error_condition ,
                         const error_condition ) noexcept {return false;}
  class system_error : public runtime_error {
  error_code _M_code;

  system_error(error_code __ec = error_code())
        : runtime_error(__ec.message()) {}
    system_error(error_code __ec, const string &__what)
        : runtime_error(__ec.message()) {}
    system_error(error_code __ec, const char *__what)
        : runtime_error((__ec.message())) {}
    system_error(const char )
        : system_error() {}
    system_error(const error_category )
        : runtime_error(error_code().message()) {}
    system_error(const string &__what)
        : runtime_error(error_code().message()) {}
    const error_code code() {return error_code ();}
    void operator0() {}
  };
  enum _Ios_Fmtflags {};
  _Ios_Fmtflags operator&(_Ios_Fmtflags &__a) {return __a;}
  _Ios_Fmtflags operator|(_Ios_Fmtflags &__a,
                                           _Ios_Fmtflags __b) {return __a;}
  _Ios_Fmtflags operator^(_Ios_Fmtflags &__a,
                                           _Ios_Fmtflags __b) {return __a;}
  _Ios_Fmtflags operator~(_Ios_Fmtflags &__a) {return __a;}
  _Ios_Fmtflags &operator|=(_Ios_Fmtflags &__a,
                                         _Ios_Fmtflags __b) {return __a;}
  _Ios_Fmtflags &operator&=(_Ios_Fmtflags &__a,
                                         _Ios_Fmtflags __b) {return __a;}
  _Ios_Fmtflags &operator^=(_Ios_Fmtflags &__a,
                                         _Ios_Fmtflags __b) {return __a;}
  enum _Ios_Openmode {
    _S_ios_openmode_max };
  _Ios_Openmode operator&(_Ios_Openmode &__a__b) {return __a__b;}
  _Ios_Openmode operator|(_Ios_Openmode &__a,
                                           _Ios_Openmode __b) {return __a;}
  _Ios_Openmode operator^(_Ios_Openmode &__a,
                                           _Ios_Openmode __b) {return __a;}
  _Ios_Openmode operator~(_Ios_Openmode &__a) {return __a;}
  _Ios_Openmode &operator|=(_Ios_Openmode &__a,
                                         _Ios_Openmode __b) {return __a;}
  _Ios_Openmode &operator&=(_Ios_Openmode &__a,
                                         _Ios_Openmode __b) {return __a;}
  _Ios_Openmode &operator^=(_Ios_Openmode &__a,
                                         _Ios_Openmode __b) {return __a;}
  enum _Ios_Iostate {
    _S_ios_iostate_max };
  _Ios_Iostate operator&(_Ios_Iostate &__a__b) {return __a__b;}
  _Ios_Iostate operator|(_Ios_Iostate &__a, _Ios_Iostate __b) {return __a;}
  _Ios_Iostate operator^(_Ios_Iostate &__a, _Ios_Iostate __b) {return __a;}
  _Ios_Iostate operator~(_Ios_Iostate &__a) {return __a;}
  _Ios_Iostate &operator|=(_Ios_Iostate &__a, _Ios_Iostate __b) {return __a;}
  _Ios_Iostate &operator&=(_Ios_Iostate &__a, _Ios_Iostate __b) {return __a;}
  _Ios_Iostate &operator^=(_Ios_Iostate &__a, _Ios_Iostate __b) {return __a;}
  enum class io_errc;
  inline error_code make_error_code(io_errc __e) noexcept {return error_code();}
  inline error_condition make_error_condition_wrap_class_string;
static luaL_Reg swig_ClientProfile_methods;
static luaL_Reg swig_ClientProfile_meta;
static swig_lua_attribute swig_ClientProfile_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_ClientProfile_Sf_SwigStatic_constants;
static luaL_Reg swig_ClientProfile_Sf_SwigStatic_methods;
static swig_lua_class swig_ClientProfile_Sf_SwigStatic_classes;
static int swig_ClientProfile_Sf_SwigStatic;
static swig_lua_class swig_ClientProfile_bases;
static const char *swig_ClientProfile_base_names;
static swig_lua_class _wrap_class_ClientProfile;
static int _proxy__wrap_new_Connection0;
static swig_lua_attribute swig_Connection_attributes;
static luaL_Reg swig_Connection_methods;
static luaL_Reg swig_Connection_meta;
static swig_lua_attribute swig_Connection_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_Connection_Sf_SwigStatic_constants;
static luaL_Reg swig_Connection_Sf_SwigStatic_methods;
static swig_lua_class swig_Connection_Sf_SwigStatic_classes;
static int swig_Connection_Sf_SwigStatic;
static swig_lua_class swig_Connection_bases;
static const char swig_Connection_base_names = 0;
static swig_lua_class _wrap_class_Connection;
static int _proxy__wrap_new_ConnectionPool0;
static swig_lua_attribute swig_ConnectionPool_attributes;
static luaL_Reg swig_ConnectionPool_methods;
static luaL_Reg swig_ConnectionPool_meta;
static swig_lua_attribute swig_ConnectionPool_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_ConnectionPool_Sf_SwigStatic_constants;
static luaL_Reg swig_ConnectionPool_Sf_SwigStatic_methods;
static swig_lua_class swig_ConnectionPool_Sf_SwigStatic_classes;
static int swig_ConnectionPool_Sf_SwigStatic;
static swig_lua_class *swig_ConnectionPool_bases ;
static const char *swig_ConnectionPool_base_names ;
static swig_lua_class _wrap_class_ConnectionPool;
static int _proxy__wrap_new_Client0;
static swig_lua_attribute swig_Client_attributes;
static luaL_Reg swig_Client_methods;
static luaL_Reg swig_Client_meta;
static swig_lua_attribute swig_Client_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_Client_Sf_SwigStatic_constants;
static luaL_Reg swig_Client_Sf_SwigStatic_methods;
static swig_lua_class swig_Client_Sf_SwigStatic_classes;
static int swig_Client_Sf_SwigStatic;
static swig_lua_class *swig_Client_bases;
static const char *swig_Client_base_names;
static swig_lua_class _wrap_class_Client;
static int _proxy__wrap_new_PreludeLog0;
static swig_lua_attribute swig_PreludeLog_attributes;
static luaL_Reg swig_PreludeLog_methods;
static luaL_Reg swig_PreludeLog_meta;
static swig_lua_attribute swig_PreludeLog_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_PreludeLog_Sf_SwigStatic_constants;
static luaL_Reg swig_PreludeLog_Sf_SwigStatic_methods;
static swig_lua_class swig_PreludeLog_Sf_SwigStatic_classes;
static int swig_PreludeLog_Sf_SwigStatic;
static swig_lua_class swig_PreludeLog_bases;
static const char *swig_PreludeLog_base_names;
static swig_lua_class _wrap_class_PreludeLog;
static int _proxy__wrap_new_PreludeError0;
static swig_lua_attribute swig_PreludeError_attributes;
static luaL_Reg swig_PreludeError_methods;
static luaL_Reg swig_PreludeError_meta;
static swig_lua_attribute swig_PreludeError_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_PreludeError_Sf_SwigStatic_constants;
static luaL_Reg swig_PreludeError_Sf_SwigStatic_methods;
static swig_lua_class swig_PreludeError_Sf_SwigStatic_classes;
static int swig_PreludeError_Sf_SwigStatic;
static swig_lua_class swig_PreludeError_bases;
static const char *swig_PreludeError_base_names;
static swig_lua_class _wrap_class_PreludeError;
static int _proxy__wrap_new_ClientEasy0;
static swig_lua_attribute swig_ClientEasy_attributes;
static luaL_Reg swig_ClientEasy_methods;
static luaL_Reg swig_ClientEasy_meta;
static swig_lua_attribute swig_ClientEasy_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_ClientEasy_Sf_SwigStatic_constants;
static luaL_Reg swig_ClientEasy_Sf_SwigStatic_methods;
static swig_lua_class swig_ClientEasy_Sf_SwigStatic_classes;
static int swig_ClientEasy_Sf_SwigStatic;
static swig_lua_class *swig_ClientEasy_bases;
static const char *swig_ClientEasy_base_names;
static swig_lua_class _wrap_class_ClientEasy;
static int _proxy__wrap_new_IDMEFCriterion0;
static swig_lua_attribute swig_IDMEFCriterion_attributes;
static luaL_Reg swig_IDMEFCriterion_methods;
static luaL_Reg swig_IDMEFCriterion_meta;
static swig_lua_attribute swig_IDMEFCriterion_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_IDMEFCriterion_Sf_SwigStatic_constants;
static luaL_Reg swig_IDMEFCriterion_Sf_SwigStatic_methods;
static swig_lua_class swig_IDMEFCriterion_Sf_SwigStatic_classes;
static int swig_IDMEFCriterion_Sf_SwigStatic;
static swig_lua_class swig_IDMEFCriterion_bases;
static const char *swig_IDMEFCriterion_base_names;
static swig_lua_class _wrap_class_IDMEFCriterion;
static int _proxy__wrap_new_IDMEFCriteria0;
static swig_lua_attribute swig_IDMEFCriteria_attributes;
static luaL_Reg swig_IDMEFCriteria_methods;
static luaL_Reg swig_IDMEFCriteria_meta;
static swig_lua_attribute swig_IDMEFCriteria_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_IDMEFCriteria_Sf_SwigStatic_constants;
static luaL_Reg swig_IDMEFCriteria_Sf_SwigStatic_methods;
static swig_lua_class swig_IDMEFCriteria_Sf_SwigStatic_classes;
static int swig_IDMEFCriteria_Sf_SwigStatic;
static swig_lua_class swig_IDMEFCriteria_bases;
static const char *swig_IDMEFCriteria_base_names;
static swig_lua_class _wrap_class_IDMEFCriteria;
static int _proxy__wrap_new_IDMEFValue0;
static swig_lua_attribute swig_IDMEFValue_attributes;
static luaL_Reg swig_IDMEFValue_methods;
static luaL_Reg swig_IDMEFValue_meta;
static swig_lua_attribute swig_IDMEFValue_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_IDMEFValue_Sf_SwigStatic_constants;
static luaL_Reg swig_IDMEFValue_Sf_SwigStatic_methods;
static swig_lua_class swig_IDMEFValue_Sf_SwigStatic_classes;
static int swig_IDMEFValue_Sf_SwigStatic;
static swig_lua_class swig_IDMEFValue_bases;
static const char *swig_IDMEFValue_base_names;
static swig_lua_class _wrap_class_IDMEFValue;
static int _proxy__wrap_new_IDMEFPath0;
static swig_lua_attribute swig_IDMEFPath_attributes;
static luaL_Reg swig_IDMEFPath_methods;
static luaL_Reg swig_IDMEFPath_meta;
static swig_lua_attribute swig_IDMEFPath_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_IDMEFPath_Sf_SwigStatic_constants;
static luaL_Reg swig_IDMEFPath_Sf_SwigStatic_methods;
static swig_lua_class swig_IDMEFPath_Sf_SwigStatic_classes;
static int swig_IDMEFPath_Sf_SwigStatic;
static swig_lua_class swig_IDMEFPath_bases;
static const char *swig_IDMEFPath_base_names;
static swig_lua_class _wrap_class_IDMEFPath;
static int _proxy__wrap_new_IDMEFTime0;
static swig_lua_attribute swig_IDMEFTime_attributes;
static luaL_Reg swig_IDMEFTime_methods;
static luaL_Reg swig_IDMEFTime_meta;
static swig_lua_attribute swig_IDMEFTime_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_IDMEFTime_Sf_SwigStatic_constants;
static luaL_Reg swig_IDMEFTime_Sf_SwigStatic_methods;
static swig_lua_class swig_IDMEFTime_Sf_SwigStatic_classes;
static int swig_IDMEFTime_Sf_SwigStatic;
static swig_lua_class swig_IDMEFTime_bases;
static const char *swig_IDMEFTime_base_names;
static swig_lua_class _wrap_class_IDMEFTime;
static int _proxy__wrap_new_IDMEFClass0;
static swig_lua_attribute swig_IDMEFClass_attributes;
static luaL_Reg swig_IDMEFClass_methods;
static luaL_Reg swig_IDMEFClass_meta;
static swig_lua_attribute swig_IDMEFClass_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_IDMEFClass_Sf_SwigStatic_constants;
static luaL_Reg swig_IDMEFClass_Sf_SwigStatic_methods;
static swig_lua_class swig_IDMEFClass_Sf_SwigStatic_classes;
static int swig_IDMEFClass_Sf_SwigStatic;
static swig_lua_class swig_IDMEFClass_bases;
static const char *swig_IDMEFClass_base_names;
static swig_lua_class _wrap_class_IDMEFClass;
static int _proxy__wrap_new_IDMEF0;
static swig_lua_attribute swig_IDMEF_attributes;
static luaL_Reg swig_IDMEF_methods;
static luaL_Reg swig_IDMEF_meta;
static swig_lua_attribute swig_IDMEF_Sf_SwigStatic_attributes;
static swig_lua_const_info swig_IDMEF_Sf_SwigStatic_constants;
static luaL_Reg swig_IDMEF_Sf_SwigStatic_methods;
static swig_lua_class swig_IDMEF_Sf_SwigStatic_classes;
static int swig_IDMEF_Sf_SwigStatic;
static swig_lua_class swig_IDMEF_bases;
static const char *swig_IDMEF_base_names;
static swig_lua_class _wrap_class_IDMEF;
static swig_lua_attribute swig_SwigModule_attributes;
static swig_lua_const_info swig_SwigModule_constants;
static luaL_Reg swig_SwigModule_methods;
static swig_lua_class *swig_SwigModule_classes;
static int swig_SwigModule_namespaces;
static int swig_SwigModule;
}
static void *_p_Prelude__ClientTo_p_Prelude__ClientProfile0;
