// PR c++/90996
// { dg-do compile { target c++14 } }

struct S
{
  int &&a = 2;
  int b[1] {a};
};

S c[2][2] {{{5}}};

struct T
{
  S c[2][2] {{{7}}};
};

T d {};
