/* PR optimization/5747
   This testcase ICEd on sparc because move_movables created new pseudos,
   but did not update reg info which load_mems needed.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -fPIC" { target sparc*-*-* } } */

extern void foo (void);
static char a[256];

void
bar (void)
{
  unsigned int i;
  static int b = 0;
  int c;

  if (b == 0)
    {
      b = 1;
      foo ();
      c = 0;
      for (i = 0; i < 10; i++)
	a[i + '0'] = c++;
      for (i = 'A'; i <= 'Z'; i++)
	a[i] = c++;
      a['$'] = c++;
      a['%'] = c++;
      a['.'] = c++;
      a['_'] = c++;
      for (i = 'a'; i <= 'z'; i++)
	a[i] = c++;
    }
}
