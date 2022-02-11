// PR c++/103749

struct foo {
  template<typename>
  friend struct bar;
};

struct bar { // { dg-error "redeclared as non-template" }
  int baz;
};

template<typename>
struct T; // { dg-message "previous" }

struct T { // { dg-error "redeclared as non-template" }
};

bar var; // { dg-error "" }
T t; // { dg-error "" }
