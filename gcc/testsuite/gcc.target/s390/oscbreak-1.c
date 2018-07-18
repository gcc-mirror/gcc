/* { dg-do compile } */
/* { dg-options "-O3 -mtune=z13 -dp" } */

void
foo (char *a, int b)
{
  int i;

  for (i = 0; i < b; i++)
    a[i] += 1;
}

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

/* { dg-final { scan-assembler "osc_break" } } */
