// { dg-do compile }
// { dg-options "-Os -fno-move-loop-invariants -std=c++11" }

struct NonTrivial3 {
  ~NonTrivial3();
};
void i() { thread_local NonTrivial3 tlarr[10]; }
