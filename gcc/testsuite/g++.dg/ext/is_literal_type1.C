// PR c++/48771
// { dg-do compile }
// { dg-options "-std=c++0x" }

struct NonLiteral {
  NonLiteral();
  ~NonLiteral();
};

static_assert(__is_literal_type(NonLiteral&), "Error");
static_assert(__is_literal_type(NonLiteral&&), "Error");
