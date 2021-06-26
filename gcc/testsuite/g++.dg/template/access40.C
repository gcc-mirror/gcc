// PR c++/96204

template<class, class = void>
struct has_type_member {
  static const bool value = false;
};

template<class T>
struct has_type_member<T, typename T::type> {
  static const bool value = true;
};

struct Parent;

struct Child {
private:
  friend struct Parent;
  typedef void type;
};

struct Parent {
  static void f() {
    // The partial specialization does not match despite Child::type
    // being accessible from the current scope.
    extern int x[1];
    extern int x[!has_type_member<Child>::value];
  }
};
