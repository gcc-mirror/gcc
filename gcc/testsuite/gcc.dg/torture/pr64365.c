/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);
extern int memcmp (const void * , const void *, __SIZE_TYPE__);

void __attribute__((noinline,noclone))
foo(int *in)
{
  int i;
  for (i = 62; i >= 10; i--)
    {
      in[i - 8] -= in[i];
      in[i - 5] += in[i] * 2;
      in[i - 4] += in[i];
    }
}

int main()
{
  int x[64];
  int y[64] = { 0, 1, -2380134, -1065336, -1026376, 3264240, 3113534, 2328130, 3632054, 3839634, 2380136, 1065339, 1026380, 1496037, 1397286, 789976, 386408, 450984, 597112, 497464, 262008, 149184, 194768, 231519, 173984, 87753, 60712, 82042, 87502, 60014, 30050, 25550, 33570, 32386, 20464, 10675, 10868, 13329, 11794, 6892, 3988, 4564, 5148, 4228, 2284, 1568, 1848, 1943, 1472, 741, 628, 702, 714, 474, 230, 234, 238, 242, 120, 59, 60, 61, 62, 63 };
  int i;

  for (i = 0; i < 64; ++i)
    {
      x[i] = i;
      __asm__ volatile ("");
    }

  foo (x);

  if (memcmp (x, y, sizeof (x)) != 0)
    abort ();

  return 0;
}
