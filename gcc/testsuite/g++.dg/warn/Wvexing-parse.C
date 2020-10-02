// PR c++/25814
// { dg-do compile }
// Test -Wvexing-parse.

struct T { };

struct X {
  X();
};

struct S {
  S(int);
  S foo (int (int));
  S(T);
  int m;
};

struct W {
  W();
  W(X, X);
  int m;
};

int g;
int g1(int(g));
int g2(int());
void fg(int);

void
fn1 (double (a))
{
  extern int f0();
  extern int f1(int(a));
  int f2(int(a)); // { dg-warning "parentheses were disambiguated as a function declaration" }
  int (*f3)(int(a));
  int f4(int a);
  int f5(int()); // { dg-warning "parentheses were disambiguated as a function declaration" }
  int f6(...);
  int f7((int(a)));
  int (f8);
  int f9(S(s)); // { dg-warning "parentheses were disambiguated as a function declaration" }
  int(f10) __attribute__(());
  int(f11(int()));
  if (int(a) = 1) { }
  int j, k, l(); // { dg-warning "empty parentheses were disambiguated as a function declaration" }
  int m, f12(int(j)); // { dg-warning "parentheses were disambiguated as a function declaration" }

  T t1(); // { dg-warning "empty parentheses were disambiguated as a function declaration" }
  T t2(T()); // { dg-warning "parentheses were disambiguated as a function declaration" }
  /* Declares a variable t3.  */
  T(t3);
  T t4(), // { dg-warning "empty parentheses were disambiguated as a function declaration" }
    t5(); // { dg-warning "empty parentheses were disambiguated as a function declaration" }

  extern S s1(int(a));
  S s2(int(a)); // { dg-warning "parentheses were disambiguated as a function declaration" }
  S s3(int a);
  S s4(int()); // { dg-warning "parentheses were disambiguated as a function declaration" }
  S s5(int(int)); // { dg-warning "parentheses were disambiguated as a function declaration" }
  S s6(...);
  S s7((int(a)));
  S s8((int)a);
  S s9 = int(a);
  S(T());
  S s10(S()); // { dg-warning "parentheses were disambiguated as a function declaration" }
  S s11(T());
  S s12(X()); // { dg-warning "parentheses were disambiguated as a function declaration" }
  S s13 = S(T());
  S(T()).foo(0);
  S (S::*foo)(int (int));
  S(*s14)(int(a));
  S s15(); // { dg-warning "empty parentheses were disambiguated as a function declaration" }
  S s16(void);

  /* Don't warn here.  */
  void fv1(int(a));
  void fv2(int());
  void (fv3)();
  void (fv4)(void);
  void (fv5)(int);

  int n(); // { dg-warning "empty parentheses were disambiguated as a function declaration" }
  int (n2)(); // { dg-warning "empty parentheses were disambiguated as a function declaration" }
  int n3(void);

  typedef int F(const char*);
  typedef int F2();
  typedef int F3() const;
  typedef int F4(int(a)) const;

  W w(X(), X()); // { dg-warning "parentheses were disambiguated as a function declaration" }
}

struct C1 {
  C1(int);
};

struct C2 {
  C2(C1, int);
};

template<int N> int value() { return N; }

void
fn2 ()
{
  int i = 0;
  C2 c2(C1(int(i)), i);
  C1(value<0>());
}
