// { dg-do assemble  }
// pushdecl gets confused by this.

void f ();
void f (int, int);
template <class T> T f (T) { }
