// Test that non-aggregates don't get the aggregate deduction.
// { dg-do compile { target c++2a } }
// { dg-prune-output "no matching function" }

struct A { A(); };

template <typename T>
struct S1 {
  T x;
};

S1 s1 = {1};			// OK

template <typename T>
struct S2 {
  S2 ();
  T x;
};

S2 s2 = {1};			// { dg-error "deduction failed" }

template <typename T>
struct S3 {
private:
  T x;
};

S3 s3 = {1};			// { dg-error "deduction failed" }

template <typename T>
struct S4 {
  virtual void f();
  T x;
};

S4 s4 = {1};			// { dg-error "deduction failed" }

template <typename T>
struct S5: public A {
  using A::A;
  T x;
};

S5 s5 = {1};			// { dg-error "deduction failed" }

template <typename T>
struct S6: virtual A {
  T x;
};

S6 s6 = {1};			// { dg-error "deduction failed" }

