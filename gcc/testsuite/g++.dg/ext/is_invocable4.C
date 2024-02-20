// { dg-do compile { target c++11 } }
// Failed access check should be a substitution failure, not an error.

#define SA(X) static_assert((X),#X)

template<bool B>
struct bool_constant { static constexpr bool value = B; };

template<typename _Fn, typename... _ArgTypes>
struct is_invocable
: public bool_constant<__is_invocable(_Fn, _ArgTypes...)>
{ };

#if __cpp_variable_templates
template<typename _Fn, typename... _ArgTypes>
constexpr bool is_invocable_v = __is_invocable(_Fn, _ArgTypes...);
#endif

class Private
{
  void operator()() const
  {
    SA( ! is_invocable<Private>::value );
#if __cpp_variable_templates
    SA( ! is_invocable_v<Private> );
#endif
  }
};

SA( ! is_invocable<Private>::value );
#if __cpp_variable_templates
SA( ! is_invocable_v<Private> );
#endif
