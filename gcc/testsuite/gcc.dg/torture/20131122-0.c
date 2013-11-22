/* { dg-do run } */

extern int memcmp (const void *, const void *, __SIZE_TYPE__);
extern void abort (void);

void __attribute__ ((noinline))
f (long *s, long *t, int len1, int len2)
{
  int i, j;

  j = 2;
  for (i = len1 - 1; i >= 0; i--)
    {
      s[j--] = (i < len2 ? t[i] : t[len2 - 1] < 0 ? -1 : 0);
      if (j < 0)
	break;
    }
}

long s[3];
long t[3];

int
main (void)
{
  t[0] = 1;
  t[1] = 2;
  t[2] = 3;
  f (s, t, 3, 3);
  if (memcmp (s, t, sizeof (s)) != 0)
    abort ();
  return 0;
}

