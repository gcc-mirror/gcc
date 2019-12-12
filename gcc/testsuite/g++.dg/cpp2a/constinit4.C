// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++2a } }

struct S { };
constinit extern S s;
constinit S s2 = { };

struct T {
  int i;
};

constinit T t;
struct U : T {
  int j;
};
constinit U u;
