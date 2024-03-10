// PR c++/109923

template<class T>
struct A {
private:
  int x;

public:
  A() : x(0) { }

  friend void non_templ_friend(A val, A<void> weird) {
    val.x++; // always works
    weird.x++; // { dg-error "private" } should only work when T=void
  }
};

int main() {
  non_templ_friend(A<void>(), A<void>()); // { dg-bogus "" }
  non_templ_friend(A<int>(), A<void>()); // { dg-message "required from here" }
}
