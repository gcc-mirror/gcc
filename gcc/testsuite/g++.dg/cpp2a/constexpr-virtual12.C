// PR c++/87425
// P1064R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct A
{
  virtual A& operator= (int);
};

struct B
{
  A a;
  B() { a = 0; }
};
