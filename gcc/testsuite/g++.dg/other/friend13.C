// PR c++/59480

void f(int, int, int=0);  // { dg-message "6:previous" }
class C {
  friend void f(int, int=0, int) {}  // { dg-error "15:friend declaration" }
};
