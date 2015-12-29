/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mx32 -maddress-mode=long" } */

extern void abort (void);
static __thread unsigned char foo [32]
__attribute__ ((tls_model ("initial-exec"), aligned (sizeof (void *))));

void
test2 (void)
{
  unsigned int s;
  for (s = 0; s < sizeof (foo); ++s)
    {
      if (foo [s] != s)
	abort ();
      foo [s] = sizeof (foo) - s;
    }
}
