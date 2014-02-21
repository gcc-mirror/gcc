/* { dg-do run } */

extern void abort (void);

static void 
foo (int *out, const int *lp, unsigned samples)
{
  int x, target;
  for (x = 0, target = 0; x < (int)samples; x += 2, target++)
    {
      out[x] = lp[target];
      out[x - 1] = out[x - 2] + out[x];
    }
}

static void 
foo_novec (int *out, const int *lp, unsigned samples)
{
  int x, target;
  for (x = 0, target = 0; x < (int)samples; x += 2, target++)
    {
      out[x] = lp[target];
      out[x - 1] = out[x - 2] + out[x];
      __asm__ volatile ("" : : : "memory");
    }
}

int main(void)
{
  const int lp[25] = {
      0, 2, 4, 6, 8,
      10, 12, 14, 16,
      18, 20, 22, 24,
      26, 28, 30, 32,
      34, 36, 38, 40,
      42, 44, 46, 48,
  };
  int out[49] = {0};
  int out2[49] = {0};
  int s;

  foo (out + 2, lp + 1, 48);
  foo_novec (out2 + 2, lp + 1, 48);

  for (s = 0; s < 49; s++)
    if (out[s] != out2[s])
      abort ();

  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
