// Test that we notice unfortunate handler ordering.

struct A { };
struct B: public A { };
struct C: private A { };

void f();
void g()
{
  try { f(); }
  catch (...) { }		// ERROR - ... followed by others
  catch (A*) { }

  try { f(); }
  catch (A*) { }		// WARNING - A* before B*
  catch (B*) { }		// WARNING - A* before B*

  try { f(); }
  catch (A*) { }
  catch (C*) { }		// no warning; A is private base
}
