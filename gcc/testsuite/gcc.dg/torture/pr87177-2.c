/* { dg-do compile } */
/* { dg-options "-w" } */

int dk;

void
lv (void)
{
  int nm;

  dk = 1;
  while (dk != 0)
    {
    }

  if (1 / 0)
    {
      dk = 0;
      while (dk != 0)
	{
	}
    }

  for (;;)
    nm = !!dk;

  (void) nm;
}
