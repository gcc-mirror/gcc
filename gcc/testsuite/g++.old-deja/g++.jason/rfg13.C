// Bug: g++ does overloading on a function-by-function basis.

void
f ()
{
  void (*fp)(void);
  {
    extern void g ();
  }
  fp = g;	/* ERROR - no 'g' in scope */
}
