/* { dg-do run } */

static inline const unsigned short *
min(unsigned short *d, const unsigned short *e)
{
  return *e < *d ? e : d;
}

unsigned short __attribute__((noipa))
test(unsigned short arr, unsigned short val)
{
  unsigned short tem = 1;
  unsigned short tem2 = *min(&arr, &tem);
  return tem2 / (arr ? arr : val);
}

int
main()
{
  if (test (2, 2) != 0)
    __builtin_abort();
  return 0;
}
