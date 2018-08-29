#define MAX 98

void foo (unsigned long *res, unsigned long in)
{
  for (unsigned long a = 0; a < MAX; a++)
    for (unsigned long b = 0; b < MAX; b++)
      for (unsigned long c = 0; c < MAX; c++)
        for (unsigned long d = 0; d < MAX; d++)
          for (unsigned long e = 0; e < MAX; e++)
            for (unsigned long f = 0; f < MAX; f++)
              for (unsigned long g = 0; g < MAX; g++)
                *res += a * in;
}

