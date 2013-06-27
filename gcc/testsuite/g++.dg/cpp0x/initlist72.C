// DR 1589
// { dg-require-effective-target c++11 }

#include <initializer_list>

struct Pair
{
  Pair(const char *, const char *);
};

struct String
{
  String(const char *);
};

void f1(int);			     // #1
int f1(std::initializer_list<long>); // #2
int g1() { return f1({42}); }	     // chooses #2

void f2(Pair);			       // #3
int f2(std::initializer_list<String>); // #4
int g2() { return f2({"foo","bar"}); } // chooses #4
