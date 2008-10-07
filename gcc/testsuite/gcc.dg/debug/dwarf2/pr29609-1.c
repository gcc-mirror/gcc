/* PR debug/29609 */
/* Verify that breakpoint on the break is hit.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gdwarf-2 -dA" } */

void abort (void);

int
foo (void)
{
  int a, i;

  for (i = 1; i <= 10; i++)
    {
      if (i < 3)
	a = 1;
      else
	break;
      a = 5;
    }
  return a;
}

int
main (void)
{
  if (foo () != 5)
    abort ();
  return 0;
}

/* { dg-final { scan-assembler "pr29609-1.c:18" } } */
