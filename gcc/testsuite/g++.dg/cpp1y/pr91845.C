// PR c++/91845 - ICE with invalid pointer-to-member.
// { dg-do compile { target c++14 } }

void non_const_mem_ptr() {
  struct A {
  };
  constexpr A a = {1, 2}; // { dg-error "too many initializers" }
  struct B {
    int A::*p;
    constexpr int g() const {
      return a.*p; // { dg-error "use of local variable" }
    };
  };
}
