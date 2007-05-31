// I, Howard Hinnant, hereby place this code in the public domain.

// Test the reference collapsing rules.  Note that there are recent differences
//    for how cv-qualifications are applied to reference types. 7.1.3, 14.3.1

// { dg-do compile }
// { dg-options "-std=c++0x" }

template <bool> struct sa;
template <> struct sa<true> {};

template <class T, T v>
struct integral_constant
{
	static const T                  value = v;
	typedef T                       value_type;
	typedef integral_constant<T, v> type;
};

typedef integral_constant<bool, true>  true_type;
typedef integral_constant<bool, false> false_type;

template <class T> struct is_lvalue_reference     : public integral_constant<bool, false> {};
template <class T> struct is_lvalue_reference<T&> : public integral_constant<bool, true> {};

template <class T> struct is_rvalue_reference      : public integral_constant<bool, false> {};
template <class T> struct is_rvalue_reference<T&&> : public integral_constant<bool, true> {};

template <class T> struct remove_reference      {typedef T type;};
template <class T> struct remove_reference<T&>  {typedef T type;};
template <class T> struct remove_reference<T&&> {typedef T type;};

template <class T> struct is_const          : public integral_constant<bool, false> {};
template <class T> struct is_const<T const> : public integral_constant<bool, true> {};

template <class T> struct is_volatile             : public integral_constant<bool, false> {};
template <class T> struct is_volatile<T volatile> : public integral_constant<bool, true> {};

struct A {};

typedef A& Alref;
typedef const A& cAlref;
typedef volatile A& vAlref;
typedef const volatile A& cvAlref;

typedef A&& Arref;
typedef const A&& cArref;
typedef volatile A&& vArref;
typedef const volatile A&& cvArref;

template <class T, bool is_lvalue_ref, bool is_rvalue_ref, bool s_const, bool s_volatile>
void test()
{
    sa<is_lvalue_reference<T>::value == is_lvalue_ref> t1;
    sa<is_rvalue_reference<T>::value == is_rvalue_ref> t2;
    sa<is_const   <typename remove_reference<T>::type>::value == s_const>    t3;
    sa<is_volatile<typename remove_reference<T>::type>::value == s_volatile> t4;
    sa<is_const   <typename remove_reference<const          T>::type>::value == s_const   > t5;
    sa<is_volatile<typename remove_reference<      volatile T>::type>::value == s_volatile> t6;
}

int main()
{
    // sanity check
    test<               A&,   true, false, false, false>();
    test<const          A&,   true, false,  true, false>();
    test<      volatile A&,   true, false, false,  true>();
    test<const volatile A&,   true, false,  true,  true>();
    test<               A&&, false,  true, false, false>();
    test<const          A&&, false,  true,  true, false>();
    test<      volatile A&&, false,  true, false,  true>();
    test<const volatile A&&, false,  true,  true,  true>();

// lvalue reference test

    // Alref
    test<               Alref&,  true, false, false, false>();
    test<const          Alref&,  true, false, false, false>();
    test<      volatile Alref&,  true, false, false, false>();
    test<const volatile Alref&,  true, false, false, false>();

    // cAlref
    test<               cAlref&,  true, false,  true, false>();
    test<const          cAlref&,  true, false,  true, false>();
    test<      volatile cAlref&,  true, false,  true, false>();
    test<const volatile cAlref&,  true, false,  true, false>();

    // vAlref
    test<               vAlref&,  true, false, false,  true>();
    test<const          vAlref&,  true, false, false,  true>();
    test<      volatile vAlref&,  true, false, false,  true>();
    test<const volatile vAlref&,  true, false, false,  true>();

    // cvAlref
    test<               cvAlref&,  true, false,  true,  true>();
    test<const          cvAlref&,  true, false,  true,  true>();
    test<      volatile cvAlref&,  true, false,  true,  true>();
    test<const volatile cvAlref&,  true, false,  true,  true>();

    // Arref
    test<               Arref&,  true, false, false, false>();
    test<const          Arref&,  true, false, false, false>();
    test<      volatile Arref&,  true, false, false, false>();
    test<const volatile Arref&,  true, false, false, false>();

    // cArref
    test<               cArref&,  true, false,  true, false>();
    test<const          cArref&,  true, false,  true, false>();
    test<      volatile cArref&,  true, false,  true, false>();
    test<const volatile cArref&,  true, false,  true, false>();

    // vArref
    test<               vArref&,  true, false, false,  true>();
    test<const          vArref&,  true, false, false,  true>();
    test<      volatile vArref&,  true, false, false,  true>();
    test<const volatile vArref&,  true, false, false,  true>();

    // vArref
    test<               cvArref&,  true, false,  true,  true>();
    test<const          cvArref&,  true, false,  true,  true>();
    test<      volatile cvArref&,  true, false,  true,  true>();
    test<const volatile cvArref&,  true, false,  true,  true>();

// rvalue reference test

    // Alref
    test<               Alref&&,  true, false, false, false>();
    test<const          Alref&&,  true, false, false, false>();
    test<      volatile Alref&&,  true, false, false, false>();
    test<const volatile Alref&&,  true, false, false, false>();

    // cAlref
    test<               cAlref&&,  true, false,  true, false>();
    test<const          cAlref&&,  true, false,  true, false>();
    test<      volatile cAlref&&,  true, false,  true, false>();
    test<const volatile cAlref&&,  true, false,  true, false>();

    // vAlref
    test<               vAlref&&,  true, false, false,  true>();
    test<const          vAlref&&,  true, false, false,  true>();
    test<      volatile vAlref&&,  true, false, false,  true>();
    test<const volatile vAlref&&,  true, false, false,  true>();

    // cvAlref
    test<               cvAlref&&,  true, false,  true,  true>();
    test<const          cvAlref&&,  true, false,  true,  true>();
    test<      volatile cvAlref&&,  true, false,  true,  true>();
    test<const volatile cvAlref&&,  true, false,  true,  true>();

    // Arref
    test<               Arref&&, false,  true, false, false>();
    test<const          Arref&&, false,  true, false, false>();
    test<      volatile Arref&&, false,  true, false, false>();
    test<const volatile Arref&&, false,  true, false, false>();

    // cArref
    test<               cArref&&, false,  true,  true, false>();
    test<const          cArref&&, false,  true,  true, false>();
    test<      volatile cArref&&, false,  true,  true, false>();
    test<const volatile cArref&&, false,  true,  true, false>();

    // vArref
    test<               vArref&&, false,  true, false,  true>();
    test<const          vArref&&, false,  true, false,  true>();
    test<      volatile vArref&&, false,  true, false,  true>();
    test<const volatile vArref&&, false,  true, false,  true>();

    // cvArref
    test<               cvArref&&, false,  true,  true,  true>();
    test<const          cvArref&&, false,  true,  true,  true>();
    test<      volatile cvArref&&, false,  true,  true,  true>();
    test<const volatile cvArref&&, false,  true,  true,  true>();

    return 0;
}
