// PR c++/86932
// { dg-do compile { target c++11 } }

template<bool, typename T> struct enable_if { using type = T; };
template<typename T> struct enable_if<false, T> { };

template<typename> struct is_foo { static constexpr bool value = false; };

// { dg-error "enable_if" "" { target *-*-* } .+1 }
template<class U, typename enable_if<is_foo<U>::value, int>::type...> void f() {}

int main()
{
  f<int>();			// { dg-error "no match" }
}
