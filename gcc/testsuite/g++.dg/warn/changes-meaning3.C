// { dg-additional-options "-Wno-changes-meaning" }

struct Lock { };
struct Traits
{
  Lock lock;
  typedef ::Lock Lock;
};
struct Traits2
{
  Lock lock;
  typedef int Lock;
};
