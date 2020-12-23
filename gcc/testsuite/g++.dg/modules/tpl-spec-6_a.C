// { dg-additional-options "-fmodules-ts" }

export module foo;
// { dg-module-cmi foo }

template<typename _Tp, _Tp __v>
struct integral_constant;

template<typename _From, typename _To, bool>
struct __is_nt_convertible_helper;

template<typename _From, typename _To>
class __is_nt_convertible_helper<_From, _To, false>
{
  template<typename _To1>
  static void __test_aux(_To1) noexcept;
  
  template<typename _From1, typename _To1>
  static
  integral_constant<bool, noexcept(__test_aux<_To1> (_From1 ()))>
  __test(int);
};
