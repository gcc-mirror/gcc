// PR c++/65396
// { dg-do compile { target c++11 } }

template<class T>     void f();
template<class T=int> void f();

template<class T=int> void g(); // { dg-message "original definition" }
template<class T=int> void g(); // { dg-error "redefinition of default" }

template<class T, class U=bool> void h();
template<class T=char, class U>
void h() {
  static_assert(__is_same(T, char), "");
  static_assert(__is_same(U, bool), "");
}

int main() {
  f();
  g();
  h();
}
