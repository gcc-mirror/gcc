// PR c/64279
// { dg-do compile { target c++11 } }
// { dg-options "-Wduplicated-branches" }

template<typename _ITp>
struct S {
  static constexpr int i = sizeof(_ITp) > alignof(_ITp) ? sizeof(_ITp) : alignof(_ITp);
};
