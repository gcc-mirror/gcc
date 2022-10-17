/* PR middle-end/102403 - ICE in init_from_control_deps, at
   gimple-predicate-analysis.cc:2364
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

extern int a[], b, c, d, e, f, g, h;

inline void foo (void) { b = 1 ^ a[b ^ (c & 1)]; }

void bar (void);

int main (void)
{
  if (!f && ~h)
    {
      if (g)
	goto L2;
    }
  else
    {
      int m = 0;              // { dg-message "declared here" }
    L1:
      e = m;
    L2:
      m ^= 1;                 // { dg-warning "-Wmaybe-uninitialized" }
      if (d)
	bar ();

      for (int j = 0; j < 10; j++)
	foo ();

      goto L1;
    }
}
