/* PR rtl-optimization/83512 */
/* { dg-do compile } */
/* { dg-options "-O2 -freorder-blocks-algorithm=simple" } */

int a;

void
foo (int *x)
{
  for (;;)
    {
      for (*x = 0; *x < 1; *x++)
	;
      ++a;
    }
}
