/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-timeout 10 } */

short int i;

void
foo (void)
{
  for (i = 1; i < 2; i += 4)
    {
      int j;

      for (j = 0; j < 5; j += 4)
        {
          int k;

          for (k = 0; k < 68; k += 4)
            {
              i &= j;
              j &= i;
            }
        }
    }
}
