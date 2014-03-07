// I, Howard Hinnant, hereby place this code in the public domain.

// Test the "Augmented" template argument deduction when binding an lvalue to an rvalue reference.

// { dg-do compile { target c++11 } }

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

template <bool is_lvalue_ref, bool is_rvalue_ref, class T>
void
test1(T&&)
{
    sa<is_lvalue_reference<T&&>::value == is_lvalue_ref> t1;
    sa<is_rvalue_reference<T&&>::value == is_rvalue_ref> t2;
}

template <bool is_lvalue_ref, bool is_rvalue_ref, class T>
void
test2(const T&&)		// { dg-message "argument" }
{
    sa<is_lvalue_reference<const T&&>::value == is_lvalue_ref> t1;
    sa<is_rvalue_reference<const T&&>::value == is_rvalue_ref> t2;
}

template <bool is_lvalue_ref, bool is_rvalue_ref, class T>
void
test3(T*&&)
{
    sa<is_lvalue_reference<T*&&>::value == is_lvalue_ref> t1;
    sa<is_rvalue_reference<T*&&>::value == is_rvalue_ref> t2;
}

struct A {};

A a;

A source() {return A();}
A* sourcep() {return 0;}

int main()
{
    test1<true, false>(a);
    test1<false, true>(source());
    test2<false, true>(a);	// { dg-error "lvalue" }
    test2<false, true>(source());
    test3<false, true>(&a);
    test3<false, true>(sourcep());
    return 0;
}
