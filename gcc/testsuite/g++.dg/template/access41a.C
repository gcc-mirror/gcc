// PR c++/96204
// { dg-do compile { target c++14 } }
// A variant of access40a.C where has_type_member is a variable template instead
// of a class template.

template<class T>
struct A {
  template<class, class = void>
  static constexpr bool has_type_member = false;
};

template<class T>
template<class U>
constexpr int A<T>::has_type_member<U, typename U::type> = true;

struct Child {
private:
  friend struct A<int>;
  typedef void type;
};

// The partial specialization matches because A<int> is a friend of Child.
static_assert(A<int>::has_type_member<Child>, "");
using type1 = const int;
using type1 = decltype(A<int>::has_type_member<Child>);

static_assert(!A<char>::has_type_member<Child>, "");
using type2 = const bool;
using type2 = decltype(A<char>::has_type_member<Child>);
