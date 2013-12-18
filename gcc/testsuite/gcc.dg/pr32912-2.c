/* { dg-do run } */
/* { dg-options "-O2 -w" } */

extern void abort (void);

#if(__SIZEOF_INT__ >= 4)
# define TYPE      int
# define TYPED(a)  a

#elif(__SIZEOF_INT__ > 2)
# define TYPE      long
# define TYPED(a)  a##L

#else
# define TYPE      long long
# define TYPED(a)  a##LL
#endif


typedef TYPE __m128i __attribute__ ((__vector_size__ (16)));

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
  union { __m128i v; TYPE i[sizeof (__m128i) / sizeof (TYPE)]; } u, v;
  int i;

  u.v = foo ();
  v.v = bar ();
  for (i = 0; i < sizeof (u.i) / sizeof (u.i[0]); i++)
    {
      if (u.i[i] != ~v.i[i])
	abort ();

      if (i < 3)
	{
	  if (u.i[i] != (TYPED (0x11111111) << i))
	    abort ();
	}
      else if (u.i[i])
	abort ();
    }
  return 0;
}
