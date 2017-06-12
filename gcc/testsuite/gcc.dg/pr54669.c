/* PR tree-optimization/54669 */
/* Testcase by Zdenek Sojka <zsojka@seznam.cz> */

/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions -fnon-call-exceptions" } */

int a[10];

void
foo (void)
{
  int x;
  int i;
  for (i = 0; i < 1;)
    {
      int b[3];
      for (i = 0; i < 1; i++)
	b[i] = a[i];
      if (&x)
	a[0] = b[0];
    }
}
