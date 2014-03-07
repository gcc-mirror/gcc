// PR c++/48771
// { dg-do compile { target c++11 } }

struct NonLiteral {
  NonLiteral();
  ~NonLiteral();
};

static_assert(__is_literal_type(NonLiteral&), "Error");
static_assert(__is_literal_type(NonLiteral&&), "Error");
