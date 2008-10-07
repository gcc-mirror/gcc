/* PR debug/36690 */
/* Verify that breakpoint can be put on goto f1, it is hit and
   varz at that spot is defined and contains 5.  Nowhere else
   in the function should be varz in the scope.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gdwarf-2 -dA" } */

int cnt;

void
bar (int i)
{
  cnt += i;
}

void
foo (int i)
{
  if (!i)
    bar (0);
  else
    {
      static int varz = 5;
      goto f1;
    }
  bar (1);
f1:
  bar (2);
}

int
main (void)
{
  foo (0);
  foo (1);
  return 0;
}

/* { dg-final { scan-assembler "pr36690-2.c:24" } } */
