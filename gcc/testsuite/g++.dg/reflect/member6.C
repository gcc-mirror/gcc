// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S {
  int i;
};

constexpr auto r = ^^S::i;
/* A pointer to member is only formed when an explicit & is used and
   its operand is a qualified-id or splice-expression not enclosed in
   parentheses.  */
auto p = &([: r :]);  // { dg-error "cannot implicitly reference a class member .S::i. through a splice" }
auto q = &[: r :];
