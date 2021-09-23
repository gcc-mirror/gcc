// PR c++/96204
// { dg-do compile { target c++14 } }
// A variant of access40.C where has_type_member is a variable template instead
// of a class template.

template<class, class = void>
constexpr bool has_type_member = false;

template<class T>
constexpr bool has_type_member<T, typename T::type> = true;

struct Parent;

struct Child {
private:
  friend struct Parent;
  typedef void type;
};

struct Parent {
  // The partial specialization does not match despite Child::type
  // being accessible from the current scope.
  static_assert(!has_type_member<Child>, "");
};
