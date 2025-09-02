// { dg-do compile }

template <typename T> struct A {};  // { dg-bogus "declaration" }
template <typename T> struct A<T*> {  // { dg-message "closing brace" }
  A<int*> a;  // { dg-error "incomplete" }
};

template <typename T> struct B;
template <typename T> struct B {  // { dg-message "closing brace" }
  B<int*> b;  // { dg-error "incomplete" }
};

template <typename T> struct C { int value; };  // { dg-bogus "declaration" }
template <typename T> struct C<T*>;  // { dg-message "declaration" }
int test(C<int*>& b) {
  return b.value;  // { dg-error "incomplete" }
}
