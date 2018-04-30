/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

typedef __PTRDIFF_TYPE__ intptr_t;
int wo;

void
sy (long int *as)
{
  for (;;)
    {
      *as = wo;
      while (as < (long int *) (void *) 2)
        {
          int *y9;

          if (wo != 0)
            *y9 = (int) (intptr_t) &wo;
          wo /= (wo != 0 && *y9 != 0);
          ++as;
        }
    }
}
