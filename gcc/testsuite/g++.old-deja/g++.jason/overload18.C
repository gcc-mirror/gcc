// pushdecl gets confused by this.
// Build don't link:

void f ();
void f (int, int);
template <class T> T f (T) { }
