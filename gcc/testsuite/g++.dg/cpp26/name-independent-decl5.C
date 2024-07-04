// P2169R4 - A nice placeholder with no name
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S {
  int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};
S s = { 1, 2 };

struct T {
  int _ = 3;
  int _ = 4;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};
T t1;
#if __cplusplus >= 201402L
T t2 = { 5, 6 };
#endif

struct U {
  int _ (int) { return 1; }
  long _ (long) { return 2; }
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};
U u = { 7 };

struct V {
  static int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};
V v = { 8 };

struct W : public S, T { int _; };
struct X : public S, T {
  int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};

struct Y {
  int _;
  int &foo () { return _; }
};

struct Z : public Y {
  int _;
  int bar ();
};

int
Z::bar ()
{
  return _ + Y::_;
}

struct A {
  int _;
  void foo () {
    int _;
    _ = 42;
    _ += ({ int _ = 0; _; });
  }
};

struct B {
  union { int _; };
  void foo () { ++_; };
};

struct C {
  int _;
  union { int x; };
  void foo () { ++_; };
};

struct D {
  struct { int _; };
  void foo () { ++_; };
};

struct E {
  struct _ {};
  int _;
  void foo () { ++_; int _; _ = 5; }
};
typedef struct E::_ E_;

struct F {
  struct _ {};
  int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};
typedef struct F::_ F_;
