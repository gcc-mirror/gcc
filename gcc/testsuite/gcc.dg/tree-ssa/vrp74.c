/* { dg-do run } */
/* { dg-options "-O2" } */

void abort (void);
void exit (int);

__attribute__ ((noinline)) void
f (unsigned long long i)
{
  if (i <= 0x1000000000000000ull)
    {
      unsigned long long j = i | 0x1000000000000000ull;
      if (j == 0x1100000000000000ull)
	exit (0);
    }
}

int
main ()
{
  f (0x0100000000000000ull);
  abort ();
}

