// PR c++/107484

namespace qualified_friend_no_match {
  void f(int);
  template<typename T> void f(T*);
  struct X {
    friend void qualified_friend_no_match::f(double); // { dg-error "does not match any template" }
  };
}
