/* { dg-do run } */
/* { dg-options "-fwrapv" } */

int __attribute__((noinline))
foo (long i, long j)
{
  if (i >= 1)
    if (j > -(long)(((unsigned long)(long)-1)>>1))
      {
        long x;
	j--;
	x = i + j;
	if (x >= 0)
	  return 1;
      }
  return 0;
}
extern void abort (void);
int main()
{
  if (foo (1, 1) != 1)
    abort ();
  return 0;
}
