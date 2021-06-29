// basic test to ensure contracts work for class and member specializations
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <cstdio>

// template specializations can have differing contracts
template<typename T>
int body(int a)
  [[ pre: a > 0 ]]
{
  T t = a * 2.5;
  return t;
}

template<>
int body<double>(int a)
  [[ pre: a > 1 ]]
{
  double t = a * 3.3;
  return t;
}

template<typename T>
int none(int a)
  [[ pre: a > 0 ]]
{
  return -a;
}

template<>
int none<double>(int a)
  [[ pre: a > 1 ]]
{
  return a - 100;
}

template<typename T>
int arg0(T t)
  [[ pre: t > 0 ]]
{
  return -t - 10;
}

template<>
int arg0<double>(double t)
  [[ pre: t > 1 ]]
{
  return -t + 10;
}

template<typename T>
int arg1(int a, T t)
  [[ pre: a > 0 ]]
  [[ pre: t > 0 ]]
{
  return -t * a;
}

template<>
int arg1<double>(int a, double t)
  [[ pre: a > 1 ]]
  [[ pre: t > 1 ]]
{
  return -t * a + 17;
}

template<typename T>
T ret(int a)
  [[ pre: a > 0 ]]
{
  return -a;
}

template<>
double ret<double>(int a)
  [[ pre: a > 1 ]]
{
  return -a * 3.3;
}

// template specializations can have no contracts
template<typename T>
int g1(T t) [[ pre: t > 0 ]]
{
  return (int)t;
}

template<>
int g1<double>(double t)
{
  return (int)t;
}

// template specializations can have no contracts in the first decl but add
// them later
template<typename T>
int g2(T t) [[ pre: t > 0 ]]
{
  return (int)t;
}

template<>
int g2<double>(double t);

template<>
int g2<double>(double t)
  [[ pre: t < 0 ]]
{
  return (int)t;
}

template<>
int g2<char>(char t)
  [[ pre: t < 'c' ]]
{
  return (int)t;
}

// contracts can be different on the general template, partial and full specs
template<typename T, typename S>
struct G3
{
  void f(T t, S s)
    [[ pre: t > 0 ]]
    [[ pre: s > 0 ]]
  {
    printf ("G3 general T S\n");
  }
};

template<typename S>
struct G3<int, S>
{
  void f(int t, S s);
};

template<typename S>
void G3<int, S>::f(int t, S s)
  [[ pre: t > 1 ]]
  [[ pre: s > 1 ]]
{
  printf ("G3 partial int S\n");
}

template<>
void G3<int, double>::f(int t, double s)
  [[ pre: t > 2 ]]
  [[ pre: s > 2 ]]
{
  printf ("G3 full int double\n");
}

struct C
{
  bool operator>(int rhs) { return false; }
};

// deletes contracts
template<>
void G3<int, C>::f(int t, C s);

template<>
void G3<int, C>::f(int t, C s)
{
  printf ("G3 full int C\n");
};

// specialized ctors
template<typename T, typename S>
struct G4
{
  G4(T t, S s)
    [[ pre: t > 0 ]]
    [[ pre: s > 0 ]]
    [[ post: x > 0 ]]
  {
    printf ("G4 general T S\n");
    return;
  }
  int x{-1};
};

template<typename S>
struct G4<char, S>
{
  G4(char t, S s)
    [[ pre: t > 'c' ]]
    [[ pre: s > 3 ]]
    [[ post: x2 > 3 ]]
  {
    printf ("G4 partial char S\n");
    return;
  }
  int x2{-1};
};

template<>
G4<double, double>::G4(double, double)
{
  printf ("G4 full double double\n");
  return;
}

template<>
G4<double, char>::G4(double a, char b)
  [[ pre: a > 0 ]]
  [[ pre: b > 'b' ]]
  [[ post: x > 1 ]]
{
  printf ("G4 full double char\n");
  return;
}

// crossover of template classes and template members ok
template<typename T, typename S>
struct G5
{
  template<typename P>
  void f(T t, S s, P r)
    [[ pre: t > 0 ]]
    [[ pre: s > 0 ]]
    [[ pre: r > 0 ]]
  {
    printf ("G5 gen T S, f gen R\n");
  }
};

template<typename S>
struct G5<char, S>
{
  template<typename R>
  void f(char x, S y, R z)
    [[ pre: x > 'z' ]]
    [[ pre: y > 1 ]]
    [[ pre: z > 1 ]]
  {
    printf ("G5 partial char S, f gen R\n");
  }
};

template<>
template<typename Q>
void G5<double, double>::f(double a, double b, Q c)
  [[ pre: a > 2 ]]
  [[ pre: b > 2 ]]
  [[ pre: c > 2 ]]
{
  printf ("G5 full double double, f gen R\n");
}

int main(int, char**)
{
  printf("%d\n", body<int>(-1));
  printf("%d\n", body<double>(-1));
  printf("%d\n", none<int>(-1));
  printf("%d\n", none<double>(-1));
  printf("%d\n", arg0(-1));
  printf("%d\n", arg0(-1.0));
  printf("%d\n", arg1(-3, -1));
  printf("%d\n", arg1(-3, -1.0));
  printf("%d\n", (int)ret<int>(-1));
  printf("%d\n", (int)ret<double>(-1));
  printf("%f\n", ret<double>(-1));

  printf("%d\n", g1(-1));
  printf("%d\n", g1(-1.0));

  printf("%d\n", g2(-1));
  printf("%d\n", g2(1.0));
  printf("%d\n", g2('d'));

  G3<double, double> g3_gen;
  G3<int, int> g3_partial;
  G3<int, double> g3_full;
  g3_gen.f(-1.0, -1.0); // general
  g3_partial.f(-2, -2); // partial spec
  g3_full.f(-3, -3.0); // full spec

  G3<char, char> g3_gen2;
  G3<int, char> g3_partial2;
  g3_gen2.f((char)-1, (char)-1);
  g3_partial2.f(-1, (char)-1);

  G3<int, C> g3_full2;
  g3_full2.f(5, C{});
  g3_full2.f(-5, C{});

  G4 g4_gen{-1, -1};
  G4 g4_full1{-1.0, -1.0};
  G4 g4_full2{-1.0, (char)'b'};
  G4 g4_partial{(char)'c', -5};

  G5<int, int> g5_gen;
  g5_gen.f(-1, -1, -2);
  g5_gen.f(-1, -1, -2.0);

  G5<char, int> g5_part;
  g5_part.f('a', -1, -2);
  g5_part.f('a', -1, -2.1);

  G5<double, double> g5_full;
  g5_full.f(-1.0, -1.0, -2);
  g5_full.f(-1.0, -1.0, -2.1);
  return 0;
}


// { dg-output "default std::handle_contract_violation called: .*.C 9 body<int> .*(\n|\r\n|\r)*" }
// { dg-output "-2(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 17 body<double> .*(\n|\r\n|\r)*" }
// { dg-output "-3(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 25 none<int> .*(\n|\r\n|\r)*" }
// { dg-output "1(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 32 none<double> .*(\n|\r\n|\r)*" }
// { dg-output "-101(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 39 arg0<int> .*(\n|\r\n|\r)*" }
// { dg-output "-9(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 46 arg0<double> .*(\n|\r\n|\r)*" }
// { dg-output "11(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 53 arg1<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 54 arg1<int> .*(\n|\r\n|\r)*" }
// { dg-output "-3(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 61 arg1<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 62 arg1<double> .*(\n|\r\n|\r)*" }
// { dg-output "14(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 69 ret<int> .*(\n|\r\n|\r)*" }
// { dg-output "1(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 76 ret<double> .*(\n|\r\n|\r)*" }
// { dg-output "3(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 76 ret<double> .*(\n|\r\n|\r)*" }
// { dg-output "3.300000(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 83 g1<int> .*(\n|\r\n|\r)*" }
// { dg-output "-1(\n|\r\n|\r)*" }
// { dg-output "-1(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 97 g2<int> .*(\n|\r\n|\r)*" }
// { dg-output "-1(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 107 g2<double> .*(\n|\r\n|\r)*" }
// { dg-output "1(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 114 g2<char> .*(\n|\r\n|\r)*" }
// { dg-output "100(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 124 G3<double, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 125 G3<double, .*(\n|\r\n|\r)*" }
// { dg-output "G3 general T S(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 139 G3<int, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 140 G3<int, .*(\n|\r\n|\r)*" }
// { dg-output "G3 partial int S(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 147 G3<int, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 148 G3<int, .*(\n|\r\n|\r)*" }
// { dg-output "G3 full int double(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 124 G3<char, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 125 G3<char, .*(\n|\r\n|\r)*" }
// { dg-output "G3 general T S(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 139 G3<int, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 140 G3<int, .*(\n|\r\n|\r)*" }
// { dg-output "G3 partial int S(\n|\r\n|\r)*" }
// { dg-output "G3 full int C(\n|\r\n|\r)*" }
// { dg-output "G3 full int C(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 173 G4<int, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 174 G4<int, .*(\n|\r\n|\r)*" }
// { dg-output "G4 general T S(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 175 G4<int, .*(\n|\r\n|\r)*" }
// { dg-output "G4 full double double(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 206 G4<double, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 207 G4<double, .*(\n|\r\n|\r)*" }
// { dg-output "G4 full double char(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 208 G4<double, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 187 G4<char, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 188 G4<char, .*(\n|\r\n|\r)*" }
// { dg-output "G4 partial char S(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 189 G4<char, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 220 G5<int, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 221 G5<int, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 222 G5<int, .*(\n|\r\n|\r)*" }
// { dg-output "G5 gen T S, f gen R(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 220 G5<int, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 221 G5<int, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 222 G5<int, .*(\n|\r\n|\r)*" }
// { dg-output "G5 gen T S, f gen R(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 233 G5<char, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 234 G5<char, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 235 G5<char, .*(\n|\r\n|\r)*" }
// { dg-output "G5 partial char S, f gen R(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 233 G5<char, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 234 G5<char, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 235 G5<char, .*(\n|\r\n|\r)*" }
// { dg-output "G5 partial char S, f gen R(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 244 G5<double, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 245 G5<double, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 246 G5<double, .*(\n|\r\n|\r)*" }
// { dg-output "G5 full double double, f gen R(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 244 G5<double, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 245 G5<double, .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 246 G5<double, .*(\n|\r\n|\r)*" }
// { dg-output "G5 full double double, f gen R(\n|\r\n|\r)*" }

