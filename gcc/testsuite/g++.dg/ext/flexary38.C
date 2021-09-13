// PR c++/99033
// { dg-do compile }
// { dg-options "" }

struct T { int t; };
struct S { char c; int T::*b[]; } a;
struct U { char c; int T::*b[0]; } b;
struct V { char c; int T::*b[1]; } c;
struct W { char c; int T::*b[2]; } d;

void
foo ()
{
  a.c = 1;
  b.c = 2;
  c.c = 3;
  d.c = 4;
}
