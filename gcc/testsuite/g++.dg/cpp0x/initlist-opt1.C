// PR c++/110102
// { dg-do compile { target c++11 } }

// { dg-error "deleted|construct_at" "" { target *-*-* } 0 }

#include <list>

struct A {
  A(int) {}
  A(const A&) = delete;		// { dg-message "declared here" }
  A(A&&) {}
};
int main() {
  std::list<A> v = {1,2,3};
}
