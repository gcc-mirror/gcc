/* { dg-do run { target ia32 } } */
/* { dg-options "-Os" } */

void
__attribute__ ((noipa))
foo (const char *x)
{
  asm volatile ("" : "+g" (x) : : "memory");
  if (x)
    __builtin_abort ();
}

int a, b = 1;

int
main ()
{
  while (1)
    {
      unsigned long long d = 18446744073709551615UL;
      while (1)
	{
	  int e = b;
	  while (d < 2)
	    foo ("0");
	  if (a)
	    d++;
	  if (b)
	    break;
	}
      break;
    }
  return 0;
}
