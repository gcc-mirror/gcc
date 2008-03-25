// DR 339
//
// Test of the use of free functions with SFINAE
void foo(int) { }
template<typename T> void foo(T*) { }

typedef char yes_type;
struct no_type { char data[2]; };

template<typename T> T create_a();

template<bool, typename T = void> struct enable_if { typedef T type; };
template<typename T> struct enable_if<false, T> { };

template<typename T> 
  typename enable_if<(sizeof(foo(create_a<T const&>()), 1) > 0),
		     yes_type>::type
  check_has_foo(const volatile T&);

no_type check_has_foo(...);

template<typename T>
struct has_foo
{
  static const bool value = 
    (sizeof(check_has_foo(create_a<T const&>())) == sizeof(yes_type));
};

struct X { };

int a1[has_foo<int>::value? 1 : -1];
int a2[has_foo<long>::value? 1 : -1];
int a3[has_foo<int*>::value? 1 : -1];
int a4[has_foo<X>::value? -1 : 1];
int a5[has_foo<int X::*>::value? -1 : 1];
