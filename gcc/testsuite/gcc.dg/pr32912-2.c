/* { dg-do run } */
/* { dg-options "-O2 -w" } */
/* { dg-skip-if "TImode not supported" { "avr-*-*" } { "*" } { "" } } */

extern void abort (void);

#if(__SIZEOF_INT__ >= 4)
typedef int __m128i __attribute__ ((__vector_size__ (16)));
#else
typedef long __m128i __attribute__ ((__vector_size__ (16)));
#endif
__m128i
foo (void)
{
  __m128i x = { 0x11111111, 0x22222222, 0x44444444 };
  return x;
}

__m128i
bar (void)
{
  __m128i x = { 0x11111111, 0x22222222, 0x44444444 };
  return ~x;
}

int
main (void)
{
#if(__SIZEOF_INT__ >= 4)
  union { __m128i v; int i[sizeof (__m128i) / sizeof (int)]; } u, v;
#else
  union { __m128i v; long i[sizeof (__m128i) / sizeof (long)]; } u, v;
#endif
  int i;

  u.v = foo ();
  v.v = bar ();
  for (i = 0; i < sizeof (u.i) / sizeof (u.i[0]); i++)
    {
      if (u.i[i] != ~v.i[i])
	abort ();
      if (i < 3)
	{
	  if (u.i[i] != (0x11111111 << i))
	    abort ();
	}
      else if (u.i[i])
	abort ();
    }
  return 0;
}
