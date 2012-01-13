/* { dg-do run } */
/* { dg-options "-Os --param large-stack-frame=30 -Wno-pointer-to-int-cast" } */

extern void abort (void);

void __attribute__((noinline))
bar (char *a)
{
}

void __attribute__((noinline))
foo (char *a, int b)
{
}

void __attribute__((noinline))
test_align (char *p, int aligned, unsigned int mask)
{
  int p_aligned = ((unsigned long int)p & mask) == 0;
  if (aligned != p_aligned)
    abort ();
}

int
main ()
{
  const int kIterations = 4;
  char results[kIterations];
  int i;
  unsigned int mask;

  mask = 0xf;
  test_align (results, ((unsigned long int)results & mask) == 0, mask);
  mask = 0x7;
  test_align (results, ((unsigned long int)results & mask) == 0, mask);
  mask = 0x3;
  test_align (results, ((unsigned long int)results & mask) == 0, mask);
  mask = 0x1;
  test_align (results, ((unsigned long int)results & mask) == 0, mask);

  bar (results);
  for (i = 0; i < kIterations; i++)
    foo ("%d ", results[i]);

  return 0;
}
