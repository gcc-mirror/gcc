/* PR c/16180 */
/* { dg-options "-O2" } */

extern int b;
int foo (int a)
{
  if (a)
    {
      b = 0;
      for(;;)
	goto L;
    }
 L:
  for(;;)
    return 0;
}
