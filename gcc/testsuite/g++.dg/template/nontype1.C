// PR c++/4934
// dump_expr didn't know how to deal with a CONVERT_EXPR with no type.

template<unsigned>   struct A {};
template<typename T> struct B { A<sizeof(+int())> a; };
