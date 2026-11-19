// CWG 3195
// { dg-do compile { target c++26 } }

#include <memory>

union U { int x = 10; float f[2]; };
U u;

consteval {
  std::start_lifetime(u.f);	// { dg-message "constexpr" } changes active member
};

void foo() {
  U u;
  constexpr int x = (std::start_lifetime(u.f), 10); // { dg-message "constexpr" }
}

// { dg-error "from outside current evaluation" "" { target *-*-* } 0 }
