// PR c++/37971
// { dg-do compile }

class C {
private:
  static int f(int); // { dg-error "private" }
  static int f(char);
};

class D {
public:
  /* C::f is inaccessible, so this is an error, even if this function
     is never called.  */
  static void g(int (*)(int) = C::f); // { dg-error "context" }
};
