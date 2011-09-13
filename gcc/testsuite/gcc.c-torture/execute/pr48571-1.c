#define S (sizeof (int))

unsigned int c[624];
void __attribute__((noinline))
bar (void)
{
  unsigned int i;
  /* Obfuscated c[i] = c[i-1] * 2.  */
  for (i = 1; i < 624; ++i)
    *(unsigned int *)((void *)c + (__SIZE_TYPE__)i * S)
	= 2 * *(unsigned int *)((void *)c + ((__SIZE_TYPE__)i +
					     ((__SIZE_TYPE__)-S)/S) * S);
}
extern void abort (void);
int
main()
{
  unsigned int i, j;
  for (i = 0; i < 624; ++i)
    c[i] = 1;
  bar();
  j = 1;
  for (i = 0; i < 624; ++i)
    {
      if (c[i] != j)
	abort ();
      j = j * 2;
    }
  return 0;
}
