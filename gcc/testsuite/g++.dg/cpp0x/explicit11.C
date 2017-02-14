// Test that we treat defaulted-in-class members like implicitly declared
// members for explicit instantiation.

// { dg-do compile { target c++11 } }

template<typename T>
struct A
{
  T x;
  A() = default;
  A(const A &other) = default;
  A& operator=(const A&) = default;
};

template class A<int>;

// { dg-final { scan-assembler-not "_ZN1AIiEC1Ev" } }
// { dg-final { scan-assembler-not "_ZN1AIiEC1ERKS0_" } }
// { dg-final { scan-assembler-not "_ZN1AIiEaSERKS0_" } }
