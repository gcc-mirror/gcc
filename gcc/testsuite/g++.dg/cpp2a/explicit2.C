// P0892R2
// { dg-do compile }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.
// { dg-options "-std=c++2a" }

int foo() { return 42; }
int g;

struct S {
  explicit(foo()) S(int); // { dg-error "call to" }
  explicit(int) S(int, int); // { dg-error "expected" }
  explicit(false ? 1 : throw 1) S(int, int, int); // { dg-error "not a constant" }
};

struct S2 {
  explicit(true) S2();
  explicit(false) S2(); // { dg-error "cannot be overloaded" }
};

int
main ()
{
  S s1 = { 1 };
  S s2 = { 1, 2 }; // { dg-error "could not convert" }
  S s3 = { 1, 2, 3 };
}
