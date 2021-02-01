// PR c++/98355
// { dg-do compile { target c++11 } }

struct S { int a; };
template <int> struct T
{
  static_assert (!__builtin_has_attribute (((S*)0) -> a, packed), ""); // { dg-message "sorry, unimplemented: .__builtin_has_attribute. with dependent argument not supported yet" }
};
