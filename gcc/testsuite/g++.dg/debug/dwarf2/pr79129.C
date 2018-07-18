/* PR debug/79129 */
/* { dg-do compile } */
/* { dg-options "-gdwarf-4 -O2 -fdebug-types-section" } */

struct B
{
  struct A { void foo (int &); };
  A *bar ();
  ~B () { int a = 1; bar ()->foo (a); }
};
struct C { ~C (); B c; };
C::~C () {}
