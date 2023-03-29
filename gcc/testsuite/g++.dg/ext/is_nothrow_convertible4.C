// PR c++/107049
// { dg-do compile { target c++11 } }
// Failed access check should be a substitution failure, not an error.

template<bool B>
struct bool_constant { static constexpr bool value = B; };

template<typename From, typename To>
struct is_nt_convertible
: public bool_constant<__is_nothrow_convertible(From, To)>
{ };

#if __cpp_variable_templates
template<typename From, typename To>
constexpr bool is_nt_convertible_v = __is_nothrow_convertible(From, To);
#endif

class Private
{
  operator int() const
  {
    static_assert( not is_nt_convertible<Private, int>::value, "" );
#if __cpp_variable_templates
    static_assert( not is_nt_convertible_v<Private, int>, "" );
#endif
    return 0;
  }
};

static_assert( not is_nt_convertible<Private, int>::value, "" );
#if __cpp_variable_templates
static_assert( not is_nt_convertible_v<Private, int>, "" );
#endif
