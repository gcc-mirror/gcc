// PR c++/91889 - follow-up fix for DR 2352.
// { dg-do compile { target c++11 } }

template <typename U> struct A { typedef U *type; };
struct B {
  typedef A<B>::type node_ptr;
};
struct C {
  typedef B::node_ptr node_ptr;
  typedef A<const B>::type const_node_ptr;
};
struct {
  void to_value_ptr(C::node_ptr) {};
  void to_value_ptr(const C::const_node_ptr &);
} b;
C::node_ptr a;
void fn1() { b.to_value_ptr(a); }
