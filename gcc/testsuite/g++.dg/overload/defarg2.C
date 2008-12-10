// PR c++/37971
// { dg-do compile }

class C {
private:
  static int f(int);
  static int f(char);

public:  
  static void g(int (*)(int) = f);
};

void h() {
  /* Although C::f is inaccessible here, it is accessible in the
     context of C::g, so there is no error.  */
  C::g();
}
