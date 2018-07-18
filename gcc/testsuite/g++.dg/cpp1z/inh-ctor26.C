// Testcase from P0136
// { dg-do compile { target c++11 } }
// { dg-options -fnew-inheriting-ctors }

struct A {
  template<typename T> A(T, typename T::type = 0);
  A(int);
};
struct B : A {
  using A::A;
  B(int);
};
B b(42L); // now calls B(int), used to call B<long>(long),
          // which called A(int) due to substitution failure
          // in A<long>(long).

// { dg-final { scan-assembler "_ZN1BC1Ei" } }
