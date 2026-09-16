/* { dg-do compile } */
/* { dg-options "-Os" } */

/* Outside loops the target skips its cost check only when optimizing for
   speed.  When optimizing for size the three selects are larger than the
   branch they replace.  */

void sink3 (long, long, long);

void
f (long c)
{
  long a, b, d;
  if (c == 3)
    {
      a = 5;
      b = 7;
      d = 13;
    }
  else
    {
      a = 9;
      b = 11;
      d = 17;
    }
  sink3 (a, b, d);
}

/* { dg-final { scan-assembler-not "\\tcsel\\t" } } */
