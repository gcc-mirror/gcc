// DR 339
//
// Test of the use of member functions with SFINAE
typedef char yes_type;
struct no_type { char data[2]; };

template<typename T> T create_a();

template<bool, typename T = void> struct enable_if { typedef T type; };
template<typename T> struct enable_if<false, T> { };

template<typename T> 
  typename enable_if<(sizeof(create_a<T>().foo(), 1) > 0),
		     yes_type>::type
  check_has_member_foo(const volatile T&);

no_type check_has_member_foo(...);

template<typename T>
struct has_foo
{
  static const bool value = 
    (sizeof(check_has_member_foo(create_a<T const&>())) == sizeof(yes_type));
};

struct X { };
struct Y {
  void foo();
};
struct Z {
  void foo(int);
};

struct A {
  int foo;
};

struct B {
  static int foo();
};

int a1[has_foo<X>::value? -1 : 1];
int a2[has_foo<Y>::value? 1 : -1];
int a3[has_foo<Z>::value? -1 : 1];
int a4[has_foo<int>::value? -1 : 1];
int a5[has_foo<A>::value? -1 : 1];
int a6[has_foo<B>::value? 1 : -1];
