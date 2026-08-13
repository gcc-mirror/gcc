// PR tree-optimization/126815
// { dg-do compile }


struct Guard { ~Guard (); };      // EH cleanup: makes f() end its basic block
int f (int);
void sink (int);

void h (int a, int b, int c)
{
  Guard g;
  int t;
  if (c)
    t = f (a);                    // throwing call feeds the PHI
  else
    t = b;                        // non-constant: prevents jump threading
  if (t == 42)                    // t's only use
    __builtin_unreachable ();
  sink (a);
}
