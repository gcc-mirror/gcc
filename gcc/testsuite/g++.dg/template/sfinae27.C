// Origin: PR c++/46162

struct small_type { char dummy; };
struct large_type { char dummy[2]; };

template<class T>
struct has_foo_member_variable
{
  template<int T::*> struct tester;
  template<class U> static small_type has_foo(tester<&U::foo> *);
  template<class U> static large_type has_foo(...);
  static const bool value = (sizeof(has_foo<T>(0)) == sizeof(small_type));
};

struct A
{
  static int foo()
  {
    return 0;
  }
};

struct B
{
  static int foo;
};

void
bar()
{
  bool b = has_foo_member_variable<A>::value;
}

