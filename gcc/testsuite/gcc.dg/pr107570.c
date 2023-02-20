/* { dg-do compile } */
/* { dg-options "-Os" } */

long int n;

void
foo (int *p, int x)
{
  for (;;)
    {
      for (*p = 0; *p < 1; ++*p)
        {
          n += *p < 0;
          if (n < x)
            {
              while (x < 1)
                ++x;

              __builtin_unreachable ();
            }
        }

      p = &x;
    }
}
