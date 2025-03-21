/* PR rtl-optimization/118615 */
/* { dg-do compile { target scheduling } } */
/* { dg-options "-O2 -fcompare-debug -fschedule-insns" } */

void foo (void);
void bar (int);
void baz (int *);
int j;

void
qux (int k, int *m)
{
  int n;
  if (k)
    {
      foo ();
      if (m)
	{
	  bar (j);
	  baz (m);
	}
    }
  baz (&n);
}
