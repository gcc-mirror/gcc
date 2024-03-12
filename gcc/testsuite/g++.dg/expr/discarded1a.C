// A version of discarded1.C using volatile types.
// PR c++/111419

struct Incomplete;

template<class T, int> struct Holder { T t; }; // { dg-error "incomplete" }

extern volatile Holder<Incomplete, 0> a;
extern volatile Holder<Incomplete, 1>& b;
extern volatile Holder<Incomplete, 2>* c;

int main() {
  a; // { dg-message "required from here" }
  b; // { dg-message "required from here" }
  // { dg-warning "implicit dereference will not access object" "" { target *-*-* } .-1 }
  *c; // { dg-message "required from here" }
}
