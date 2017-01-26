/* PR debug/78835 */
/* { dg-do compile } */
/* { dg-options "-gdwarf-4 -O2 -fdebug-types-section" } */

struct A { void foo (); };

void
bar (A &x)
{
  x.foo ();
}
