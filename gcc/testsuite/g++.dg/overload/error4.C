// PR c++/92451

template<typename T> struct Local {};
void f() {
  Local(int); // { dg-error "" }
}
