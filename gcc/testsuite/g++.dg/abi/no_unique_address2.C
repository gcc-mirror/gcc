// { dg-do compile { target c++11 } }

struct A
{
  virtual void f();
  char c;
};

struct B1 : A
{
  char c2;
};

struct B2
{
  A a [[no_unique_address]];
  char c2;
};

struct C
{
  char c;
};

struct D: virtual C
{
  virtual void f();
};

struct B3: D
{
  char c2;
};

struct B4
{
  D d [[no_unique_address]];
  char c2;
};

#define SA(X) static_assert ((X), #X)
SA (sizeof (B2) == sizeof (B1));
SA (sizeof (B3) == sizeof (B4));
