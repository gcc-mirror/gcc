// PR c++/118255
// { dg-do "compile" }

// The PR's case, that used to error out.
template <int non_template>
struct S {
  friend class non_template; // { dg-bogus "shadows template parameter" }
};

class non_template {};
S<0> s;

// We already accepted cases where the friend is already declared.
template <int non_template>
struct T {
  friend class non_template;
};
T<0> t;

// We should reject (re)declarations.
template <int non_template>
struct U {
  class non_template {};  // { dg-error "shadows template parameter" }
  void non_template () {} // { dg-error "shadows template parameter" }
};
U<0> u;
