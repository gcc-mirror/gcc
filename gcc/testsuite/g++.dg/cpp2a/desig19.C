// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A
{
  int i;
  constexpr operator int() { return 42; }
};

#define SA(X) static_assert ((X),#X)
constexpr A a1 { A() };
SA(a1.i == 0);
constexpr A a2 { i: A() };
SA(a2.i == 42);
#if __cpp_constexpr >= 201304L
constexpr int f3 () { A const &r { A() }; return r.i; }
SA(f3() == 0);
constexpr int f4 () { A const &r { i: A() }; return r.i; }
SA(f4() == 42);
constexpr int f5 () { A ar[1]{{ A() }}; return ar[0].i; }
SA(f5() == 0);
constexpr int f5a () { A ar[1]{{ i: A() }}; return ar[0].i; }
SA(f5a() == 42);
#if __cpp_constexpr >= 201907L
constexpr int f6 () { A* p = new A{A()}; int i = p->i; delete p; return i; }
SA(f6() == 0);
constexpr int f6a () { A* p = new A{i:A()}; int i = p->i; delete p; return i; }
SA(f6a() == 42);
#endif
#endif
constexpr int f7 (A a) { return a.i; }
SA(f7({A()}) == 0);
SA(f7({i:A()}) == 42);
