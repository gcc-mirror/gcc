/* { dg-do compile } */
/* { dg-options "-O1 -mtune=z13 -fschedule-insns -dp" } */

void
foo (char *a, int b)
{
  int i;

  for (i = 0; i < b; i++)
    a[i] += 1;
}

/* We should not unconditionally emit an osc_break here.  */
void
bar (char *a, int b)
{
  int i;

  for (i = 0; i < b; i++)
    {
      if (a[i] & 1)
	a[i] = 1;
    }
}

/* { dg-final { scan-assembler-times "osc_break" 1 } } */
