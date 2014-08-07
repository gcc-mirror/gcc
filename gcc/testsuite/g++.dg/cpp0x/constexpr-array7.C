// PR c++/61994
// { dg-do compile { target c++11 } }

struct A { int i,j; };

struct X {
  A a = {1,1};
};

constexpr X table[1][1] = {{ {} }};

#define SA(X) static_assert(X,#X)
SA(table[0][0].a.i == 1);
