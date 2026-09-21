/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=olympus -fdump-rtl-ce1" } */

void
select_values (unsigned int n, const volatile unsigned long input[][3],
	       volatile unsigned long output[][2])
{
  for (unsigned int i = 0; i < n; ++i)
    {
      unsigned long c = input[i][0], x = input[i][1], y = input[i][2];
      unsigned long a, b;
      if (c > 7)
	{
	  a = x * 3 + y;
	  b = 2;
	}
      else
	{
	  a = x + 9;
	  b = 3;
	}
      output[i][0] = a;
      output[i][1] = b;
    }
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
