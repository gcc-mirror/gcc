// Test that partial ordering ignores defaulted parms and 'this'.

struct A {
  template<class T> int f(T) { return 1; }
  template<class T> int f(T*, int=0) { return 0; }
  template<class T> int g(T*) { return 0; }
  template<class T> static int g(T, int=0) { return 1; }
  template<class T> int h(T*) { return 0; }
  template<class T> static int h(T, int=0) { return 1; }
  template<class T> static int j(T*, short y=0) { return 0; }
  template<class T> static int j(T, int=0) { return 1; }
};

int main(void) {
  A a;
  int *p;
  return (a.f(p) || a.g(p) || a.h(p) || a.j(p));
}
