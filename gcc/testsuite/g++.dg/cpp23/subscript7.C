// PR c++/103460
// { dg-do compile }
// { dg-options "-std=c++23" }

struct S {
  int &operator[] (int, ...);
} s;
struct T {
  int &operator[] (auto...);
} t;
struct U {
  int &operator[] (...);
} u;

int a = s[1] + s[2, 1] + s[3, 2, 1] + s[4, 3, 2, 1]
	+ t[0.0] + t[nullptr, s, 42]
	+ u[] + u[42] + u[1.5L, 1LL];
