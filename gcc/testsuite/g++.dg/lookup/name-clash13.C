typedef int T;
struct A {
  struct B {
    static T t;
  };
  typedef float T;		// { dg-error "changes meaning" }
};
