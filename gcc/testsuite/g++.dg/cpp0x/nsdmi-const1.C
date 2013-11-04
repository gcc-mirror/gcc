// PR c++/50707
// { dg-options -std=c++11 }

int g;

struct S {
   int const v=g;
};

S s;
