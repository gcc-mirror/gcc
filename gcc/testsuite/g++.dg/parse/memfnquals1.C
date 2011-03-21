// PR c++/48166

struct foo {
  static void func ();
};
void foo::func () const {}	// { dg-error "type qualifiers" }
