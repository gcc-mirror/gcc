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

// { dg-output {contract violation in function body<int> at .*:9: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {-2(\n|\r\n|\r)} }
// { dg-output {contract violation in function body<double> at .*:17: a > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {-3(\n|\r\n|\r)} }
// { dg-output {contract violation in function none<int> at .*:25: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {1(\n|\r\n|\r)} }
// { dg-output {contract violation in function none<double> at .*:32: a > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {-101(\n|\r\n|\r)} }
// { dg-output {contract violation in function arg0<int> at .*:39: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {-9(\n|\r\n|\r)} }
// { dg-output {contract violation in function arg0<double> at .*:46: t > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {11(\n|\r\n|\r)} }
// { dg-output {contract violation in function arg1<int> at .*:53: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function arg1<int> at .*:54: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {-3(\n|\r\n|\r)} }
// { dg-output {contract violation in function arg1<double> at .*:61: a > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function arg1<double> at .*:62: t > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {14(\n|\r\n|\r)} }
// { dg-output {contract violation in function ret<int> at .*:69: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {1(\n|\r\n|\r)} }
// { dg-output {contract violation in function ret<double> at .*:76: a > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
// { dg-output {contract violation in function ret<double> at .*:76: a > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {3.300000(\n|\r\n|\r)} }
// { dg-output {contract violation in function g1<int> at .*:83: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {-1(\n|\r\n|\r)} }
// { dg-output {-1(\n|\r\n|\r)} }
// { dg-output {contract violation in function g2<int> at .*:97: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {-1(\n|\r\n|\r)} }
// { dg-output {contract violation in function g2<double> at .*:107: t < 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {1(\n|\r\n|\r)} }
// { dg-output {contract violation in function g2<char> at .*:114: t < 'c'(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {100(\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<double, double>::f at .*:124: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<double, double>::f at .*:125: s > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G3 general T S(\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<int, int>::f at .*:139: t > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<int, int>::f at .*:140: s > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G3 partial int S(\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<int, double>::f at .*:147: t > 2(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<int, double>::f at .*:148: s > 2(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G3 full int double(\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<char, char>::f at .*:124: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<char, char>::f at .*:125: s > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G3 general T S(\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<int, char>::f at .*:139: t > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G3<int, char>::f at .*:140: s > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G3 partial int S(\n|\r\n|\r)} }
// { dg-output {G3 full int C(\n|\r\n|\r)} }
// { dg-output {G3 full int C(\n|\r\n|\r)} }
// { dg-output {contract violation in function G4<int, int>::G4 at .*:173: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G4<int, int>::G4 at .*:174: s > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G4 general T S(\n|\r\n|\r)} }
// { dg-output {contract violation in function G4<int, int>::G4 at .*:175: x > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G4 full double double(\n|\r\n|\r)} }
// { dg-output {contract violation in function G4<double, char>::G4 at .*:206: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G4<double, char>::G4 at .*:207: b > 'b'(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G4 full double char(\n|\r\n|\r)} }
// { dg-output {contract violation in function G4<double, char>::G4 at .*:208: x > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G4<char, int>::G4 at .*:187: t > 'c'(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G4<char, int>::G4 at .*:188: s > 3(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G4 partial char S(\n|\r\n|\r)} }
// { dg-output {contract violation in function G4<char, int>::G4 at .*:189: x2 > 3(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, int>::f<int> at .*:220: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, int>::f<int> at .*:221: s > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, int>::f<int> at .*:222: r > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G5 gen T S, f gen R(\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, int>::f<double> at .*:220: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, int>::f<double> at .*:221: s > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, int>::f<double> at .*:222: r > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G5 gen T S, f gen R(\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<char, int>::f<int> at .*:233: x > 'z'(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<char, int>::f<int> at .*:234: y > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<char, int>::f<int> at .*:235: z > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G5 partial char S, f gen R(\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<char, int>::f<double> at .*:233: x > 'z'(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<char, int>::f<double> at .*:234: y > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<char, int>::f<double> at .*:235: z > 1(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G5 partial char S, f gen R(\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<double, double>::f<int> at .*:244: a > 2(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<double, double>::f<int> at .*:245: b > 2(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<double, double>::f<int> at .*:246: c > 2(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G5 full double double, f gen R(\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<double, double>::f<double> at .*:244: a > 2(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<double, double>::f<double> at .*:245: b > 2(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<double, double>::f<double> at .*:246: c > 2(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G5 full double double, f gen R(\n|\r\n|\r)} }
