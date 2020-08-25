/* { dg-do compile } */

int c9, d3;

void
sg (int *rs, int f2)
{
  for (;;)
    {
      if (*rs < 1)
        __builtin_unreachable ();

      for (c9 = 0; c9 < 1; ++c9)
        while (f2 < 1)
          ++c9;

      if (d3)
        c9 += !!f2 ? 0 : d3;
    }
}
