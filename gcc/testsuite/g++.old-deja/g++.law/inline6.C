// Build don't link: 
// GROUPS passed inlining
// inline file
// Message-Id: <199307162240.AA04019@world.std.com>
// From: kol@world.std.com (Nikolay Yatsenko)
// Subject: g++ bug: crash with extern C friend
// Date: Fri, 16 Jul 1993 18:40:48 -0400

inline void Ignore(){}

extern "C" void foo() {}  // but without extern C  g++ compiles it

struct A
{
  void f()     {Ignore();}
  friend void foo ();
};
