// PR gcov-profile/55650
// { dg-do link }
// { dg-require-profiling "-fprofile-generate" }
// { dg-options "-O2 -fprofile-generate" }
// { dg-additional-sources "pr55650.cc" }

struct A
{
  virtual void foo ();
};

struct B : public A
{
  B ();
  void foo () {}
};

inline A *
bar ()
{
  return new B;
}
