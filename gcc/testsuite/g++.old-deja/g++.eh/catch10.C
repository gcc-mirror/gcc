// { dg-do assemble  }
// Test that we notice unfortunate handler ordering.

struct A { };
struct B: public A { };
struct C: private A { };

void f();
void g()
{
  try { f(); }
  catch (...) { }		// { dg-error "" } ... followed by others
  catch (A*) { }

  try { f(); }
  catch (A*) { }		// { dg-warning "" } A* before B*
  catch (B*) { }		// { dg-warning "" } A* before B*

  try { f(); }
  catch (A*) { }
  catch (C*) { }		// no warning; A is private base
}
