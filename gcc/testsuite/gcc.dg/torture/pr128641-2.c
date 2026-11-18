/* { dg-do run } */

extern void *malloc (__SIZE_TYPE__);

int n;

__attribute__((noinline)) void
f (char *p)
{
  *(long *) p = 7;
}

int
main (void)
{
  char *b = malloc (72);
  f (b);
  long s = 0;
  for (int k = 0; k < (n & 7) + 1; k++)
    {
      s = s * 31 + *(long *) (b + k * 8);
      *(long long *) (b + 8 + k * 8) = k;
    }
  if (s != 7)
    __builtin_abort ();
  return 0;
}
