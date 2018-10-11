// { dg-do compile { target c++11 } }

struct A {
  unsigned char i : 1;
};

struct B: A
{
  unsigned char j : 7;
};

struct B2
{
  [[no_unique_address]] A a;
  unsigned char j : 7;
};

#define SA(X) static_assert ((X), #X)
SA (sizeof (B) == sizeof (B2));
