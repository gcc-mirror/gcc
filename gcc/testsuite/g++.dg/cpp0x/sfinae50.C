// PR c++/61083
// { dg-do compile { target c++11 } }

template<typename T> T declval();

template<typename T, typename U>
struct is_same {
  static const bool value = false;
};

template<typename T>
struct is_same<T, T> {
  static const bool value = true;
};

struct true_type {};
struct false_type {};

template <typename T>
struct is_foo {
private:
  template<typename U, U> struct helper {};

  template <typename Z> static auto
  test(Z z) -> decltype(helper<void (Z::*)() const, &Z::foo>(), true_type());

  template <typename> static auto test(...) -> false_type;

public:
  enum { value = is_same<decltype(test<T>(declval<T>())), true_type>::value };
};

struct A { 
  int foo();
  void foo() const; 
};

struct A1 : public A {};

static_assert (is_foo<A>::value == 1, "");
static_assert (is_foo<A1>::value == 0, "");
